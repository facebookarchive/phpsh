#!/usr/bin/env python
from distutils.core import setup
from subprocess import Popen
import sys
import os

sys.path.insert(0, 'src')
from phpsh import __version__

if len(sys.argv) > 1 and sys.argv[1] == "build":
    build_root = os.path.dirname(os.path.realpath(__file__))
    make_dir = os.path.join(build_root, "src/xdebug-clients/geben")
    p = Popen(["make", "-C", make_dir])
    os.waitpid(p.pid, 0)

# something better than this?
if os.getenv("USER") == "root" or not os.getenv("HOME"):
    config_dir = "/etc/phpsh"
else:
    config_dir = os.getenv("HOME") + "/.phpsh"

setup(
    name="phpsh",
    version=__version__,
    description="interactive shell into a php codebase",
    author="facebook",
    author_email="phpsh@googlegroups.com",
    url="http://www.phpsh.org/",
    packages=["phpsh"],
    package_dir={"phpsh": "src"},
    package_data={"phpsh": ["*.php", "*.el",
                            "xdebug-clients/geben/README",
                            "xdebug-clients/geben/LICENSE",
                            "xdebug-clients/geben/Makefile",
                            "xdebug-clients/geben/geben.el",
                            "xdebug-clients/geben/geben.elc",
                            "xdebug-clients/geben/help",
                            "xdebug-clients/geben/tree-widget/geben/*.png"]},
    scripts=["src/phpsh", "src/dbgp-phpsh.py"],
    data_files=[
        (config_dir, ["src/rc.php", "src/php_manual.db", "src/config.sample"]),
        ("man/man1", ["src/doc/phpsh.1"]),
    ],
    requires=["pysqlite", "readline"],
)
