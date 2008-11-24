#!/usr/bin/env python
from setuptools import setup
import sys
import os

if sys.argv[1] == 'build':
    build_root = os.path.dirname(os.path.realpath(__file__))
    make_dir = os.path.join(build_root, "src/xdebug-clients/geben")
    os.system("make -C " + make_dir)

setup(
    name='phpsh',
    version='1.2',
    description='interactive shell into a php codebase',
    author='facebook',
    author_email='phpsh@facebook.com',
    url='http://www.phpsh.org/',
    packages=['phpsh'],
    package_dir={'phpsh': 'src'},
    package_data={'phpsh': ['*.php', '*.el',
                            'xdebug-clients/geben/README',
                            'xdebug-clients/geben/LICENSE',
                            'xdebug-clients/geben/Makefile',
                            'xdebug-clients/geben/geben.el',
                            'xdebug-clients/geben/geben.elc',
                            'xdebug-clients/geben/help',
                            'xdebug-clients/geben/tree-widget/geben/*.png']},
    scripts=['src/phpsh', 'src/phpsh_check_syntax'],
    data_files=[
        ('/etc/phpsh', ['src/phpshrc.php']),
        ('/etc/phpsh', ['src/php_manual.db']),
        ('/etc/phpsh', ['src/config.sample']),
    ],
    install_requires=['pysqlite'],
)
