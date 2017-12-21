#! /usr/bin/env python3
import unittest

class TestCase(unittest.TestCase):
    def test_1(self):

        input_ = """

        val a = random(5);
        char b = 'Y';
        val z = 4;
        val q = z + a;
        val p = z + a;
        if (a > 2) {
            print(4);
            b = 'n';
            z += 100;
        }
        val y = 7;
        z += 1;
        z *= 0;

        print(p, q);
        print(b, z, a);

        """

        expected = "4\n77\nn03\n"

        # note: underscores act as meaningless sparators in python
        # see: https://docs.python.org/3/whatsnew/3.6.html#whatsnew36-pep515
        max_bad_cycles  =  30_000
        max_ugly_cycles = 100_000


        from Project8.project import generate_bad_code_from_string, generate_ugly_code_from_string
        from interpreter import run_bad_code_from_string, run_ugly_code_from_string

        print("Good Input:")
        print(input_)
        print()

        print("Generated (unoptimized) Bad Code:")
        slow_bc = generate_bad_code_from_string(input_, optimize_mode=False)
        print(slow_bc)
        print()

        print("Executed  (unoptimized) BC Output:")
        slow_bc_output = run_bad_code_from_string(slow_bc)
        print(slow_bc_output)
        print()

        self.assertEqual(slow_bc_output, expected)

        print("Generated (optimized) BC:")
        fast_bc = generate_bad_code_from_string(input_, optimize_mode=True)
        print(fast_bc)
        print()

        print("Executed  (optimized) BC Output:")
        fast_bc_output = run_bad_code_from_string(fast_bc)
        print(fast_bc_output)
        print()

        self.assertEqual(fast_bc_output, expected)


        print("Generated (unoptimized) Ugly Code:")
        slow_uc = generate_ugly_code_from_string(input_, optimize_mode=False)
        print(slow_uc)
        print()

        print("Executed  (unoptimized) UC Output:")
        slow_uc_output = run_ugly_code_from_string(slow_uc)
        print(slow_uc_output)
        print()

        self.assertEqual(slow_uc_output, expected)

        print("Generated (optimized) UC:")
        fast_uc = generate_ugly_code_from_string(input_, optimize_mode=True)
        print(fast_uc)
        print()

        print("Executed  (optimized) UC Output:")
        fast_uc_output = run_ugly_code_from_string(fast_uc)
        print(fast_uc_output)
        print()

        self.assertEqual(fast_uc_output, expected)
        print("CORRECT OUTPUT!!! (now testing speed)")

        unoptimized_bc_cycles = run_bad_code_from_string(slow_bc, profile_mode=True)
        print(f"Your Bad Code cycles (unoptimized)\t= {unoptimized_bc_cycles:,}")
        print(f"Max Allowed Bad Code cycles \t\t= {max_bad_cycles:,}")
        optimized_bc_cycles = run_bad_code_from_string(fast_bc, profile_mode=True)
        print(f"Your Bad Code cycles (optimized)\t= {optimized_bc_cycles:,}")

        self.assertLessEqual(optimized_bc_cycles, max_bad_cycles)

        unoptimized_uc_cycles = run_ugly_code_from_string(slow_uc, profile_mode=True)
        print(f"Your Ugly Code cycles (unoptimized)\t= {unoptimized_uc_cycles:,}")
        print(f"Max Allowed Ugly Code cycles \t\t= {max_ugly_cycles:,}")
        optimized_uc_cycles = run_ugly_code_from_string(fast_uc, profile_mode=True)
        print(f"Your Ugly Code cycles (optimized)\t= {optimized_uc_cycles:,}")

        self.assertLessEqual(optimized_uc_cycles, max_ugly_cycles)


unittest.main()
