def get_ssh_prefix(location)
  case location
  when 'Internet', ''
    prefix = ''
  when 'LAN'
    prefix = 'LAN_'
  else
    raise "Unknown location #{location} specified."
  end
  return prefix
end

def read_and_validate_ssh_config(prefix)
  @secret_ssh_key = $config[prefix + "SSH_private_key"]
  @public_ssh_key = $config[prefix + "SSH_public_key"]
  @ssh_username   = $config["#{prefix}SSH_username"]
  @ssh_host       = $config["#{prefix}SSH_host"]
  @ssh_port       = $config["#{prefix}SSH_port"] if $config["#{prefix}SSH_port"]

  assert(!@secret_ssh_key.nil? && @secret_ssh_key.length > 0, "Mandatory variable #{prefix}SSH_private_key not been set.")
  assert(!@public_ssh_key.nil? && @public_ssh_key.length > 0, "Mandatory variable  #{prefix}SSH_public_key not been set.")

  unless prefix.include?('Unsafe')
    assert(!@ssh_username.nil? && @ssh_username.length > 0, "Mandatory variable " +
           "#{prefix}SSH_username not set.")
    assert(!@ssh_host.nil? && @ssh_host.length > 0, "Mandatory variable " +
           "#{prefix}SSH_host not set.")
    if prefix == 'LAN_'
      assert(@ssh_host.match(/^(10|192\.168|172\.(1[6-9]|2[0-9]|3[01]))/), "#{@ssh_host} " +
             "does not look like a LAN IP address.")
    else
      assert(!@ssh_host.match(/^(10|192\.168|172\.(1[6-9]|2[0-9]|3[01]))/), "#{@ssh_host} " +
             "looks like a LAN IP address.")
    end
    if @ssh_port
      assert(@ssh_port.is_a?(Integer), "#{prefix}SSH_port must be an integer.")
    end
  end
end

Given /^I have the SSH key pair for an? (Git repository|SSH server)(?: on the| on a)?\s?(|LAN|Internet)$/ do |server_type, loc|
  next if @skip_steps_while_restoring_background
  @vm.execute_successfully("install -m 0700 -d '/home/#{LIVE_USER}/.ssh/'", LIVE_USER)
  case server_type
  when "SSH server"
    prefix = ''
  when "Git repository"
    prefix = "Unsafe_"
  else
    raise "Unknown server type #{server_type}"
  end

  prefix = get_ssh_prefix(loc) + prefix
  read_and_validate_ssh_config(prefix)

  @vm.execute_successfully("echo '#{@secret_ssh_key}' > '/home/#{LIVE_USER}/.ssh/id_rsa'", LIVE_USER)
  @vm.execute_successfully("echo '#{@public_ssh_key}' > '/home/#{LIVE_USER}/.ssh/id_rsa.pub'", LIVE_USER)
  @vm.execute_successfully("chmod 0600 '/home/#{LIVE_USER}/.ssh/'id*", LIVE_USER)
end

Given /^I verify the SSH fingerprint for the (Git repository|SSH server)$/ do |server_type|
  next if @skip_steps_while_restoring_background
  assert(server_type = 'Git repository' || server_type = 'SSH server',
         "Unknown server type #{server_type} specified.")
  @screen.wait("SSHFingerprint.png", 60)
  @screen.type('yes' + Sikuli::Key.ENTER)
end

When /^I connect to an SSH server(?: on the| on a)?\s?(|LAN|Internet)$/ do |loc|
  next if @skip_steps_while_restoring_background

  prefix = get_ssh_prefix(loc)
  read_and_validate_ssh_config(prefix)

  ssh_port_suffix = "-p #{@ssh_port}" if @ssh_port

  cmd = "ssh #{@ssh_username}@#{@ssh_host} #{ssh_port_suffix}"

  step "process \"ssh\" is not running"
  step "I run \"#{cmd}\" in GNOME Terminal"
  step "process \"ssh\" is running within 10 seconds"
end

Then /^I have sucessfully logged into the SSH server$/ do
  next if @skip_steps_while_restoring_background
  @screen.wait('SSHLoggedInPrompt.png', 60)
end
