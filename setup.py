#!/usr/bin/env python
from setuptools import setup

setup(
    name='phpsh',
    version='1.1',
    description='interactive shell into a php codebase',
    author='facebook',
    author_email='phpsh@facebook.com',
    url='http://www.phpsh.org/',
    packages=['phpsh'],
    package_dir={'phpsh': 'src'},
    package_data={'phpsh': ['*.php']},
    scripts=['src/phpsh', 'src/phpsh_check_syntax'],
    data_files=[
        ('/etc/phpsh', ['src/phpshrc.php']),
        ('/etc/phpsh', ['src/php_manual.db']),
    ],
    install_requires=['pysqlite'],
)
