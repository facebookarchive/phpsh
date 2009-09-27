<?php

// Have this file include/do anything to set up your php codebase environment.
// Expect it to run from the top of your repo, where you should start phpsh
// from (or add top-of-repo-finding logic here, etc).  Run ctags from the top
// of your repo periodically too, to get the benefits of phpsh ctags
// integration.

// You may make use of the string $___phpsh___codebase_mode which may be
// specified on phpsh startup via -c.  The string is '' by default.


// E_ALL catches certain coding errors and we recommend it.  Comment this out
// if it produces too many warnings you don't have time to fix.
// You could also try moving it to the bottom of this file, after you load your
// codebase libraries, to have fewer pre-existing warnings show up, but still
// get added safety in phpsh.
error_reporting(E_ALL);


switch ($___phpsh___codebase_mode) {
case '':
  // Put default library includes here.
  //require_once 'relative-path-from-repo-head/lib/codebase_include.php';
  break;

case 'none':
  // Vanilla php
  break;

// Put any custom codebase modes here, e.g.:
//case 'core':
//  // Would only load 'core' library includes here, perhaps for faster startup
//  // in a very large codebase.

default:
  fwrite(STDERR, 'Unknown codebase mode '.$___phpsh___codebase_mode."\n");
  break;
}

