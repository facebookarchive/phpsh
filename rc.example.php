<?php
# copy this file to ~/.phpsh/rc.php via:
# mkdir -p ~/.phpsh && cp rc.example.php ~/.phpsh/rc.php

# load any system defaults / codebase-modes
require_once '/etc/phpsh/rc.php';

# the examples here are some functions i use for easy io with the outside world

define('DEFAULT_IO_FILE', getenv('HOME').'/o');

/**
 * append array or var to ~/o
 * @author dcorson
 */
function o($x, $fn=DEFAULT_IO_FILE) {
  $f = fopen($fn, 'a');
  if (is_array($x)) {
    fwrite($f, implode("\n", $x)."\n");
  } else {
    fwrite($f, $x."\n");
  }
  fclose($f);
  return true;
}

/**
 * strip last char (typically used to kill "\n") from line
 * @author dcorson
 */
function _rstrip($l) {
  return substr($l, 0, strlen($l) - 1);
}

/**
 * read array from ~/o
 * @author dcorson
 */
function i($fn=DEFAULT_IO_FILE) {
  return array_map('_rstrip', file($fn));
}
