Archived Repo
=============
This is an archived project and is no longer supported or updated by Facebook. Please do not file issues or pull-requests against this repo. If you wish to continue to develop this code yourself, we recommend you fork it.

Proceed and be bold!

phpsh
=====
phpsh is a read-eval-print-loop for php that features readline history, tab completion, and quick access to documentation. It was developed at Facebook and ironically, is written mostly in python. It is open source and released under a modified BSD license.

Mailing list at: [http://groups.google.com/group/phpsh]([http://groups.google.com/group/phpsh])

PHP Dependencies
===================
__Version:__

 * PHP 5+

__Modules:__

 * [pcntl](http://php.net/manual/en/book.pcntl.php) *recommended but not required.*
 * [pcre](http://php.net/manual/en/book.pcre.php)
 * [posix](http://php.net/manual/en/book.posix.php)
 * [tokenizer](http://php.net/manual/en/book.tokenizer.php)

Python Dependencies
===================


* sqlite  
  * Python <  2.6: `sudo easy_install pysqlite`  
  * Python >= 2.6: pysqlite should already be installed, since it's part of the stdlib
* readline  
  * Mac OS X: `sudo easy_install readline`
  * ActivePython: `pypm install readline`
  * Most Linux distributions: readline should already be installed, since it's part of the stdlib

Installing Globally
===================
_Note that this install currently clobbers any previous /etc/phpsh/rc.php_  
* `python setup.py build`  
* `sudo python setup.py install`  
* `phpsh`  

Installing Locally
==================
_Note that this install currently clobbers any previous ~/.phpsh/rc.php_  
* `python setup.py install --prefix=~`  
* `export PYTHONPATH=~/lib/python2.6/site-packages` (Adjust this if your python minor version differs from 2.6)  
* `~/bin/phpsh`  

_You will probably want to put the PYTHONPATH line in your .bashrc or such, and just add ~/bin to your PATH as well._

There are more details on installing from a setup.py at [http://docs.python.org/library/distutils.html](http://docs.python.org/library/distutils.html)

After Install
=============

Vanilla PHP
-------------
* Just run `phpsh` and have fun.

Connecting to an Actual Codebase
--------------------------------
* `cd ~/www`  
* `ctags -R` # for phpsh ctags integration, recommended  
* `phpsh lib/init.php` # or some file(s) that load codebase libraries  

Autoloading
-----------
* Modify /etc/phpsh/rc.php. Then:
  * `cd ~/www`  
  * `phpsh`  
* And for times when you just want vanilla php:
  * `phpsh -c none`

Individual Configuration
------------------------
* For individual configuration, also see rc.example.php in the php distribution.

Hacking on phpsh
================
* For faster iteration, after installing once, you can run phpsh from src/ directly without reinstalling.  
  * `cd ~/www`  
  * `~/projects/phpsh/src/phpsh`  


Todo (after phpsh 1.2)
======================
* Simple phpsh breakpoints that you can insert into your php code.  
    * jlindamood has suggested adding
    a very basic debugging facility in which phpsh.php would define some
    function called like phpsh_breakpoint() which you would be able to put
    in your code that would cause the php> to re-happen at that breakpoint
    so that you can investigate whatever state until you issue like a
    phpsh_resume() (or whatever it would be called).
    
    * My suspicion is that that can be done pretty easily be having a new
    message type from phpsh.php to phpsh.py.  (Currently there is only the
    'ready' message when phpsh.php is ready to receive a new line of codes
    from phpsh.py.)
    
* Magic fatal undoing?  
    * brent suggested adding
    some kind of katamari-esque memory snapshotting, so that on fatals like
    calling a nonexistent function (which are not catchable in php-land,
    sadly) can be magically recovered from.  I think dweatherford or larry
    would be good to ask about this kind of black magic.
    
* Convert from /tmp file to named pipe for 'ready' message passing [http://my.opera.com/zomg/blog/2007/08/29/php-and-named-pipes](http://my.opera.com/zomg/blog/2007/08/29/php-and-named-pipes)
* Maybe phpsh_check_syntax shouldn't actually be installed as a script, and
  just be a pkg_resource?  Didn't want to worry about zip file overhead, and
  installing didn't seem like a big deal.
* Similarly, php_manual.db should probably go in share/ not etc/ but similarly
  was worried about b.s. with setuptools..
* Paging for long php> d ..  results?  Or is terminal scroll fine..
* Thread loading ctags and starting php?  Would speed start but not restart.
* Command-line apc for faster php startup for large codebases.
* Note on php start error to start from codebase place?
* Make tab to show function signature work with multiline func sigs.

