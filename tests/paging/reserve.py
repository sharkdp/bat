#!/usr/bin/env python3
"""PTY regressions: python3 tests/paging/reserve.py /path/to/bat /path/to/less.
Requires POSIX and less 632 or newer. No dependencies outside the standard library.
"""
import argparse
import fcntl
import os
from pathlib import Path
import pty
import select
import signal
import struct
import tempfile
import termios
import time

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('bat')
parser.add_argument('less')
args = parser.parse_args()
binary, pager = str(Path(args.bat).resolve()), str(Path(args.less).resolve())


def check(root, name, text, options, expected_pager, expected_error=None):
    source = root / 'input.txt'
    source.write_text(text)
    pid, fd = pty.fork()
    if pid == 0:
        fcntl.ioctl(1, termios.TIOCSWINSZ, struct.pack('HHHH', 24, 60, 0, 0))
        env = dict(os.environ, TERM='xterm-256color')
        for key in ['BAT_OPTS', 'BAT_PAGER', 'PAGER', 'LESS', 'LESS_LINES', 'LESSOPEN', 'LESSCLOSE', 'BAT_STYLE', 'BAT_THEME']:
            env.pop(key, None)
        command = [binary, '--no-config', '--paging=auto', '--color=never', '--decorations=always', '--style=plain', '--terminal-width=60', f'--pager={pager}', *options, str(source)]
        os.execve(binary, command, env)
    data = b''
    ended = False
    status = None
    deadline = time.monotonic() + 2
    try:
        while time.monotonic() < deadline:
            ready, _, _ = select.select([fd], [], [], 0.03)
            if ready:
                try:
                    chunk = os.read(fd, 65536)
                    data += chunk
                except OSError:
                    pass
            child, state = os.waitpid(pid, os.WNOHANG)
            if child:
                ended, status = True, state
                break
        assert (not ended) == expected_pager, (name, expected_pager, data.decode(errors='replace'))
        if expected_error:
            assert expected_error.encode() in data, (name, data)
            assert os.waitstatus_to_exitcode(status) != 0
        elif ended:
            assert os.waitstatus_to_exitcode(status) == 0, (name, data)
        else:
            os.write(fd, b'q')
            deadline = time.monotonic() + 2
            while time.monotonic() < deadline:
                ready, _, _ = select.select([fd], [], [], 0.03)
                if ready:
                    try:
                        os.read(fd, 65536)
                    except OSError:
                        pass
                child, state = os.waitpid(pid, os.WNOHANG)
                if child:
                    ended, status = True, state
                    break
            assert ended and os.waitstatus_to_exitcode(status) == 0, name
        print(f'{name}: passed', flush=True)
    finally:
        if not ended:
            os.kill(pid, signal.SIGKILL)
            os.waitpid(pid, 0)
        os.close(fd)


with tempfile.TemporaryDirectory(prefix='bat-reserve-') as directory:
    root = Path(directory)
    medium = ''.join(f'ROW{n:03d}\n' for n in range(1, 22))
    short = ''.join(f'ROW{n:03d}\n' for n in range(1, 11))
    check(root, 'normal automatic threshold', medium, [], False)
    check(root, 'reserved prompt rows engage paging', medium, ['--paging-reserve=4'], True)
    check(root, 'short output still exits', short, ['--paging-reserve=4'], False)
    check(root, 'zero disables reservation', medium, ['--paging-reserve=0'], False)
    check(root, 'last reservation wins', medium, ['--paging-reserve=4', '--paging-reserve=0'], False)
    check(root, 'disabled paging ignores reservation', medium, ['--paging-reserve=4', '--paging=never'], False)
    check(root, 'forced paging stays enabled', short, ['--paging-reserve=4', '--paging=always'], True)
    check(root, 'visible ranges determine threshold', medium, ['--paging-reserve=4', '--line-range=1:10'], False)
    check(root, 'wrapped output uses rendered rows', ('x' * 120 + '\n') * 10, ['--paging-reserve=4', '--wrap=character'], True)
    check(root, 'custom less arguments retain automatic exit', short, ['--paging-reserve=4', f'--pager={pager} -R'], False)
    check(root, 'large reservation stays usable', short, ['--paging-reserve=65535'], True)
    check(root, 'unsupported pager reports error', short, ['--paging-reserve=4', '--pager=builtin'], False, '--paging-reserve requires less 632 or newer')
