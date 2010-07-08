import unittest
from phpsh import PhpshState, line_encode, do_sugar


class TestPhpsh(unittest.TestCase):

    def setUp(self):
        self.ps = PhpshState(
            cmd_incs=set(),
            do_color=False,
            do_echo=False,
            codebase_mode='none',
            do_autocomplete=False,
            do_ctags=False,
            interactive=True,
            with_xdebug=False,
            verbose=False)

    def php(self, line):
        expr = line_encode(do_sugar(line)) + '\n'
        return self.ps.do_expr(expr).strip()

    def assertPhp(self, line, expected=''):
        self.assertEqual(str(expected), self.php(line))

    def test_simple(self):
        self.assertPhp('=1+1', 2)
        self.assertPhp('$a=3')
        self.assertPhp('=$a+2', 5)

    def test_long_running_func(self):
        func = ("function foo(){ "
                "  $a=0;"
                "  for ($i=0; $i<100000; $i++)"
                "    $a+=1;"
                "  return $a;"
                "}")
        self.php(func)
        self.assertPhp('=foo()', '100000')

    def test_undefined_func_check(self):
        # make sure state is maintained after undefined function call
        # is avoided.
        self.assertPhp('$a=3')
        self.assertPhp(
            'does_not_exist()',
            'Not executing input: Possible call to undefined '
            'function does_not_exist()\n'
            'See /etc/phpsh/config.sample to disable UndefinedFunctionCheck.')
        self.assertPhp('=$a', 3)

    def test_fatal(self):
        # create some state
        self.assertPhp('$a=3')

        # run a function that does not exist (and get around the simple check)
        self.assertPhp("$func = 'does_not_exist'")
        result = self.php('$func()')
        expected = 'Fatal error: Call to undefined function does_not_exist()'
        self.assertTrue(expected in result)

        # verify that the state is still there.
        self.assertPhp('=$a', 3)


if __name__ == '__main__':
    unittest.main()
