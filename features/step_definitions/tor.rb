def iptables_parse(iptables_output)
  chains = Hash.new
  cur_chain = nil
  cur_chain_policy = nil
  parser_state = :expecting_chain_def

  iptables_output.split("\n").each do |line|
    line.strip!
    if /^Chain /.match(line)
      assert_equal(:expecting_chain_def, parser_state)
      m = /^Chain (.+) \(policy (.+) \d+ packets, \d+ bytes\)$/.match(line)
      if m.nil?
        m = /^Chain (.+) \(\d+ references\)$/.match(line)
      end
      assert_not_nil m
      _, cur_chain, cur_chain_policy = m.to_a
      chains[cur_chain] = {
        "policy" => cur_chain_policy,
        "rules" => Array.new
      }
      parser_state = :expecting_col_descs
    elsif /^pkts\s/.match(line)
      assert_equal(:expecting_col_descs, parser_state)
      assert_equal(["pkts", "bytes", "target", "prot", "opt",
                    "in", "out", "source", "destination"],
                   line.split(/\s+/))
      parser_state = :expecting_rule_or_empty
    elsif line.empty?
      assert_equal(:expecting_rule_or_empty, parser_state)
      cur_chain = nil
      parser_state = :expecting_chain_def
    else
      assert_equal(:expecting_rule_or_empty, parser_state)
      pkts, _, target, prot, opt, in_iface, out_iface, source, destination, extra =
        line.split(/\s+/, 10)
      [pkts, target, prot, opt, in_iface, out_iface, source, destination].each do |var|
        assert_not_empty(var)
        assert_not_nil(var)
      end
      chains[cur_chain]["rules"] << {
        "rule" => line,
        "pkts" => pkts.to_i,
        "target" => target,
        "protocol" => prot,
        "opt" => opt,
        "in_iface" => in_iface,
        "out_iface" => out_iface,
        "source" => source,
        "destination" => destination,
        "extra" => extra
      }
    end
  end
  assert_equal(:expecting_rule_or_empty, parser_state)
  return chains
end

def iptables_chains_parse(iptables, table = "filter", &block)
  assert(block_given?)
  cmd = "#{iptables}-save -t #{table} | iptables-xml"
  xml_str = $vm.execute_successfully(cmd).stdout
  rexml = REXML::Document.new(xml_str)
  rexml.get_elements('iptables-rules/table/chain').each do |element|
    yield(
      element.attribute('name').to_s,
      element.attribute('policy').to_s,
      element.get_elements('rule')
    )
  end
end

def ip4tables_chains(table = "filter", &block)
  iptables_chains_parse('iptables', table, &block)
end

def ip6tables_chains(table = "filter", &block)
  iptables_chains_parse('ip6tables', table, &block)
end

def iptables_rules_parse(iptables, chain, table)
  iptables_chains_parse(iptables, table) do |name, _, rules|
    return rules if name == chain
  end
  return nil
end

def iptables_rules(chain, table = "filter")
  iptables_rules_parse("iptables", chain, table)
end

def ip6tables_rules(chain, table = "filter")
  iptables_rules_parse("ip6tables", chain, table)
end

def try_xml_element_text(element, xpath, default = nil)
  node = element.elements[xpath]
  (node.nil? or not(node.has_text?)) ? default : node.text
end

Then /^the firewall's policy is to (.+) all IPv4 traffic$/ do |expected_policy|
  expected_policy.upcase!
  ip4tables_chains do |name, policy, _|
    if ["INPUT", "FORWARD", "OUTPUT"].include?(name)
      assert_equal(expected_policy, policy,
                   "Chain #{name} has unexpected policy #{policy}")
    end
  end
end

Then /^the firewall is configured to only allow the (.+) users? to connect directly to the Internet over IPv4$/ do |users_str|
  users = users_str.split(/, | and /)
  expected_uids = Set.new
  users.each do |user|
    expected_uids << $vm.execute_successfully("id -u #{user}").stdout.to_i
  end
  allowed_output = iptables_rules("OUTPUT").find_all do |rule|
    out_iface = rule.elements['conditions/match/o']
    is_maybe_accepted = rule.get_elements('actions/*').find do |action|
      not(["DROP", "REJECT", "LOG"].include?(action.name))
    end
    is_maybe_accepted &&
    (
      # nil => match all interfaces according to iptables-xml
      out_iface.nil? ||
      ((out_iface.text == 'lo') == (out_iface.attribute('invert').to_s == '1'))
    )
  end
  uids = Set.new
  allowed_output.each do |rule|
    rule.elements.each('actions/*') do |action|
      destination = try_xml_element_text(rule, "conditions/match/d")
      if action.name == "ACCEPT"
        # nil == 0.0.0.0/0 according to iptables-xml
        assert(destination == '0.0.0.0/0' || destination.nil?,
               "The following rule has an unexpected destination:\n" +
               rule.to_s)
        state_cond = try_xml_element_text(rule, "conditions/state/state")
        next if state_cond == "RELATED,ESTABLISHED"
        assert_not_nil(rule.elements['conditions/owner/uid-owner'])
        rule.elements.each('conditions/owner/uid-owner') do |owner|
          uid = owner.text.to_i
          uids << uid
          assert(expected_uids.include?(uid),
                 "The following rule allows uid #{uid} to access the " +
                 "network, but we only expect uids #{expected_uids.to_a} " +
                 "(#{users_str}) to have such access:\n#{rule.to_s}")
        end
      elsif action.name == "call" && action.elements[1].name == "lan"
        lan_subnets = ["10.0.0.0/8", "172.16.0.0/12", "192.168.0.0/16"]
        assert(lan_subnets.include?(destination),
               "The following lan-targeted rule's destination is " +
               "#{destination} which may not be a private subnet:\n" +
               rule.to_s)
      else
        raise "Unexpected iptables OUTPUT chain rule:\n#{rule.to_s}"
      end
    end
  end
  uids_not_found = expected_uids - uids
  assert(uids_not_found.empty?,
         "Couldn't find rules allowing uids #{uids_not_found.to_a.to_s} " \
         "access to the network")
end

Then /^the firewall's NAT rules only redirect traffic for Tor's TransPort and DNSPort$/ do
  tor_onion_addr_space = "127.192.0.0/10"
  iptables_nat_output = $vm.execute_successfully("iptables -t nat -L -n -v").stdout
  chains = iptables_parse(iptables_nat_output)
  chains.each_pair do |name, chain|
    rules = chain["rules"]
    if name == "OUTPUT"
      good_rules = rules.find_all do |rule|
        rule["target"] == "REDIRECT" &&
          (
           (
            rule["destination"] == tor_onion_addr_space &&
            rule["extra"] == "redir ports 9040"
           ) ||
           rule["extra"] == "udp dpt:53 redir ports 5353"
          )
      end
      assert_equal(rules, good_rules,
                   "The NAT table's OUTPUT chain contains some unexpected " \
                   "rules:\n" +
                   ((rules - good_rules).map { |r| r["rule"] }).join("\n"))
    else
      assert(rules.empty?,
             "The NAT table contains unexpected rules for the #{name} " \
             "chain:\n" + (rules.map { |r| r["rule"] }).join("\n"))
    end
  end
end

Then /^the firewall is configured to block all external IPv6 traffic$/ do
  ip6_loopback = '::1/128'
  expected_policy = "DROP"
  ip6tables_chains do |name, policy, rules|
    assert_equal(expected_policy, policy,
                 "The IPv6 #{name} chain has policy #{policy} but we " \
                 "expected #{expected_policy}")
    good_rules = rules.find_all do |rule|
      ["DROP", "REJECT", "LOG"].any? do |target|
        rule.elements["actions/#{target}"]
      end \
      ||
      ["s", "d"].all? do |x|
        try_xml_element_text(rule, "conditions/match/#{x}") == ip6_loopback
      end
    end
    bad_rules = rules - good_rules
    assert(bad_rules.empty?,
           "The IPv6 table's #{name} chain contains some unexpected rules:\n" +
           (bad_rules.map { |r| r.to_s }).join("\n"))
  end
end

def firewall_has_dropped_packet_to?(proto, host, port)
  regex = "^Dropped outbound packet: .* "
  regex += "DST=#{Regexp.escape(host)} .* "
  regex += "PROTO=#{Regexp.escape(proto)} "
  regex += ".* DPT=#{port} " if port
  $vm.execute("journalctl --dmesg --output=cat | grep -qP '#{regex}'").success?
end

When /^I open an untorified (TCP|UDP|ICMP) connections to (\S*)(?: on port (\d+))? that is expected to fail$/ do |proto, host, port|
  assert(!firewall_has_dropped_packet_to?(proto, host, port),
         "A #{proto} packet to #{host}" +
         (port.nil? ? "" : ":#{port}") +
         " has already been dropped by the firewall")
  @conn_proto = proto
  @conn_host = host
  @conn_port = port
  case proto
  when "TCP"
    assert_not_nil(port)
    cmd = "echo | netcat #{host} #{port}"
    user = LIVE_USER
  when "UDP"
    assert_not_nil(port)
    cmd = "echo | netcat -u #{host} #{port}"
    user = LIVE_USER
  when "ICMP"
    cmd = "ping -c 5 #{host}"
    user = 'root'
  end
  @conn_res = $vm.execute(cmd, :user => user)
end

Then /^the untorified connection fails$/ do
  case @conn_proto
  when "TCP"
    expected_in_stderr = "Connection refused"
    conn_failed = !@conn_res.success? &&
      @conn_res.stderr.chomp.end_with?(expected_in_stderr)
  when "UDP", "ICMP"
    conn_failed = !@conn_res.success?
  end
  assert(conn_failed,
         "The untorified #{@conn_proto} connection didn't fail as expected:\n" +
         @conn_res.to_s)
end

Then /^the untorified connection is logged as dropped by the firewall$/ do
  assert(firewall_has_dropped_packet_to?(@conn_proto, @conn_host, @conn_port),
         "No #{@conn_proto} packet to #{@conn_host}" +
         (@conn_port.nil? ? "" : ":#{@conn_port}") +
         " was dropped by the firewall")
end

When /^the system DNS is(?: still)? using the local DNS resolver$/ do
  resolvconf = $vm.file_content("/etc/resolv.conf")
  bad_lines = resolvconf.split("\n").find_all do |line|
    !line.start_with?("#") && !/^nameserver\s+127\.0\.0\.1$/.match(line)
  end
  assert_empty(bad_lines,
               "The following bad lines were found in /etc/resolv.conf:\n" +
               bad_lines.join("\n"))
end

def stream_isolation_info(application)
  case application
  when "htpdate"
    {
      :grep_monitor_expr => '/curl\>',
      :socksport => 9062
    }
  when "tails-security-check", "tails-upgrade-frontend-wrapper"
    # We only grep connections with ESTABLISHED state since `perl`
    # is also used by monkeysphere's validation agent, which LISTENs
    {
      :grep_monitor_expr => '\<ESTABLISHED\>.\+/perl\>',
      :socksport => 9062
    }
  when "Tor Browser"
    {
      :grep_monitor_expr => '/firefox\>',
      :socksport => 9150
    }
  when "Gobby"
    {
      :grep_monitor_expr => '/gobby\>',
      :socksport => 9050
    }
  when "SSH"
    {
      :grep_monitor_expr => '/\(connect-proxy\|ssh\)\>',
      :socksport => 9050
    }
  when "whois"
    {
      :grep_monitor_expr => '/whois\>',
      :socksport => 9050
    }
  else
    raise "Unknown application '#{application}' for the stream isolation tests"
  end
end

When /^I monitor the network connections of (.*)$/ do |application|
  @process_monitor_log = "/tmp/netstat.log"
  info = stream_isolation_info(application)
  $vm.spawn("while true; do " +
            "  netstat -taupen | grep \"#{info[:grep_monitor_expr]}\"; " +
            "  sleep 0.1; " +
            "done > #{@process_monitor_log}")
end

Then /^I see that (.+) is properly stream isolated$/ do |application|
  expected_port = stream_isolation_info(application)[:socksport]
  assert_not_nil(@process_monitor_log)
  log_lines = $vm.file_content(@process_monitor_log).split("\n")
  assert(log_lines.size > 0,
         "Couldn't see any connection made by #{application} so " \
         "something is wrong")
  log_lines.each do |line|
    addr_port = line.split(/\s+/)[4]
    assert_equal("127.0.0.1:#{expected_port}", addr_port,
                 "#{application} should use SocksPort #{expected_port} but " \
                 "was seen connecting to #{addr_port}")
  end
end

And /^I re-run tails-security-check$/ do
  $vm.execute_successfully("tails-security-check", :user => LIVE_USER)
end

And /^I re-run htpdate$/ do
  $vm.execute_successfully("service htpdate stop && " \
                           "rm -f /var/run/htpdate/* && " \
                           "systemctl --no-block start htpdate.service")
  step "the time has synced"
end

And /^I re-run tails-upgrade-frontend-wrapper$/ do
  $vm.execute_successfully("tails-upgrade-frontend-wrapper", :user => LIVE_USER)
end

When /^I connect Gobby to "([^"]+)"$/ do |host|
  @screen.wait("GobbyWindow.png", 30)
  @screen.wait("GobbyWelcomePrompt.png", 10)
  @screen.click("GnomeCloseButton.png")
  @screen.wait("GobbyWindow.png", 10)
  # This indicates that Gobby has finished initializing itself
  # (generating DH parameters, etc.) -- before, the UI is not responsive
  # and our CTRL-t is lost.
  @screen.wait("GobbyFailedToShareDocuments.png", 30)
  @screen.type("t", Sikuli::KeyModifier.CTRL)
  @screen.wait("GobbyConnectPrompt.png", 10)
  @screen.type(host + Sikuli::Key.ENTER)
  @screen.wait("GobbyConnectionComplete.png", 60)
end

When /^the Tor Launcher autostarts$/ do
  @screen.wait('TorLauncherWindow.png', 60)
end

When /^I configure some (\w+) pluggable transports in Tor Launcher$/ do |bridge_type|
  bridge_type.downcase!
  bridge_type.capitalize!
  begin
    @bridges = $config["Tor"]["Transports"][bridge_type]
    assert_not_nil(@bridges)
    assert(!@bridges.empty?)
  rescue NoMethodError, Test::Unit::AssertionFailedError
    raise(
<<EOF
It seems no '#{bridge_type}' pluggable transports are defined in your local configuration file (#{LOCAL_CONFIG_FILE}). See wiki/src/contribute/release_process/test/usage.mdwn for the format.
EOF
)
  end
  @bridge_hosts = []
  for bridge in @bridges do
    @bridge_hosts << bridge["ipv4_address"]
  end

  @screen.wait_and_click('TorLauncherConfigureButton.png', 10)
  @screen.wait('TorLauncherBridgePrompt.png', 10)
  @screen.wait_and_click('TorLauncherYesRadioOption.png', 10)
  @screen.wait_and_click('TorLauncherNextButton.png', 10)
  @screen.wait_and_click('TorLauncherBridgeList.png', 10)
  for bridge in @bridges do
    bridge_line = bridge_type.downcase   + " " +
                  bridge["ipv4_address"] + ":" +
                  bridge["ipv4_port"].to_s
    bridge_line += " " + bridge["fingerprint"].to_s if bridge["fingerprint"]
    bridge_line += " " + bridge["extra"].to_s if bridge["extra"]
    @screen.type(bridge_line + Sikuli::Key.ENTER)
  end
  @screen.wait_and_click('TorLauncherNextButton.png', 10)
  @screen.hide_cursor
  @screen.wait_and_click('TorLauncherFinishButton.png', 10)
  @screen.wait('TorLauncherConnectingWindow.png', 10)
  @screen.waitVanish('TorLauncherConnectingWindow.png', 120)
end

When /^all Internet traffic has only flowed through the configured pluggable transports$/ do
  assert_not_nil(@bridge_hosts, "No bridges has been configured via the " +
                 "'I configure some ... bridges in Tor Launcher' step")
  leaks = FirewallLeakCheck.new(@sniffer.pcap_file,
                                :accepted_hosts => @bridge_hosts)
  leaks.assert_no_leaks
end

Then /^the Tor binary is configured to use the expected Tor authorities$/ do
  tor_auths = Set.new
  tor_binary_orport_strings = $vm.execute_successfully(
    "strings /usr/bin/tor | grep -E 'orport=[0-9]+'").stdout.chomp.split("\n")
  tor_binary_orport_strings.each do |potential_auth_string|
    auth_regex = /^\S+ orport=\d+( bridge)?( no-v2)?( v3ident=[A-Z0-9]{40})? ([0-9\.]+):\d+( [A-Z0-9]{4}){10}$/
    m = auth_regex.match(potential_auth_string)
    if m
      auth_ipv4_addr = m[4]
      tor_auths << auth_ipv4_addr
    end
  end
  expected_tor_auths = Set.new(TOR_AUTHORITIES)
  assert_equal(expected_tor_auths, tor_auths,
               "The Tor binary does not have the expected Tor authorities " +
               "configured")
end
