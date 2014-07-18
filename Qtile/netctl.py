import re
import subprocess
import time

from libqtile import widget


class NetctlStatus(widget.base.InLoopPollText):
    """
        Show status of netctl interfaces.
    """
    defaults = [
        ('update_interval', 5., 'Update interval for netctl list'),
    ]

    def __init__(self, **config):
        widget.base.InLoopPollText.__init__(self, **config)
        self.add_defaults(NetctlStatus.defaults)

    def _is_auto_active(self):
        try:
            subprocess.check_call(
                [
                    'systemctl',
                    'is-active',
                    'netctl-auto@wlo1'
                ],
                stdout=open('/dev/null', 'a')
            )
        except subprocess.CalledProcessError:
            return False
        return True

    def _get_status(self):
        output = []

        netctl_list_re = re.compile('^\* (?P<profile>.*)$')

        netctl_list = subprocess.Popen(
            ['netctl', 'list'],
            stdout=subprocess.PIPE,
            universal_newlines=True
        )
        for line in netctl_list.stdout.readlines():
            match = netctl_list_re.match(line)
            if match:
                output.append(match.group('profile'))
        if self._is_auto_active():
            output.append('Auto')
        return ' '.join(output)

    def tick(self):
        ts = time.time()
        self.timeout_add(self.update_interval - ts % self.update_interval,
                         self.tick)
        self.update(self.poll())
        return False

    def poll(self):
        return self._get_status()
