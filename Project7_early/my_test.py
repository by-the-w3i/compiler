#! /usr/bin/env python3
import unittest

class TestCase(unittest.TestCase):
    def test_1(self):
        from Project7.project import generate_bad_code_from_string, generate_ugly_code_from_string
        from Project7.bad_and_ugly_interpreter import run_bad_code_from_string, run_ugly_code_from_string

        def test(input_, expected):
          print("Good Input:")
          print(input_)
          print()
          bc = generate_bad_code_from_string(input_)
          print("Generated Bad Code:")
          print(bc)
          print()
          bc_output = run_bad_code_from_string(bc)
          print("Executed BC Output:")
          print(bc_output)
          print()
          uc = generate_ugly_code_from_string(input_)
          print("Generated Ugly Code:")
          print(uc)
          print()
          uc_output = run_ugly_code_from_string(uc)
          print("Executed UC Output:")
          print(uc_output)
          print()

          self.assertEqual(bc_output, uc_output)
          self.assertEqual(uc_output, expected)

        test("""

define val even(val value) {
   if (value == 0) return 1;
   if (value < 0) return 0;
   return odd(value -= 1);
}
define val odd(val value) {
   return even(value -= 1);
}

print(even(0));
print(even(1));
print(even(2));
print(even(3));
print(even(4));


             """, "1\n0\n1\n0\n1\n")


unittest.main()
