
import os
import os.path
import logging
import pipes
import subprocess
from typing import Union

import tailsgreeter.config


class AdminSetting(object):
    """Setting controlling the sudo password"""

    def __init__(self):
        self.password = None  # type: Union[None, str]

    def apply_to_upcoming_session(self):
        setting_file = tailsgreeter.config.admin_password_output_path

        if self.password:
            proc = subprocess.run(
                ["mkpasswd", "-s", "--method", "sha512crypt"],
                input=self.password,
                capture_output=True,
                check=True,
            )
            hashed_and_salted_pw = proc.stdout.decode().strip()

            with open(setting_file, 'w') as f:
                os.chmod(setting_file, 0o600)
                f.write('TAILS_USER_PASSWORD=%s\n' % pipes.quote(hashed_and_salted_pw))
                logging.debug('password written to %s', setting_file)
            return

        # Try to remove the password file
        try:
            os.unlink(setting_file)
            logging.debug('removed %s', setting_file)
        except OSError:
            # It's bad if the file exists and couldn't be removed, so we
            # we raise the exception in that case (which prevents the login)
            if os.path.exists(setting_file):
                raise