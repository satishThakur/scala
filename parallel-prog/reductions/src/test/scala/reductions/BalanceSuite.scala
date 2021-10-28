package reductions

import org.junit.Test

class BalanceSuite {

  import ParallelParenthesesBalancing._

  @Test def `balance should work for empty string`: Unit = {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  @Test def `balance should work for string of length 1`: Unit = {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  @Test def `balance should work for string of length 2`: Unit = {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  @Test def `balance should work for string of length N`: Unit = {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 2) == expected,
        s"balance($input) should be $expected")

    check("eds()()()abc", true)
    check("something)(or)(", false)
    check("((", false)
    check("))...((", false)
    check(".)", false)
    check(".(", false)
    check("(.....())", true)
    check(").", false)
  }

}
