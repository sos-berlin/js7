package js7.base.utils

import js7.base.test.OurTestSuite
import js7.base.utils.L3.{False, True, Unknown}

final class L3Test extends OurTestSuite:

  "Not !" in:
    assert(!False == True)
    assert(!Unknown == Unknown)
    assert(!True == False)

  "And &" in:
    assert((False && False) == False)
    assert((False && Unknown) == False)
    assert((False && True) == False)
    assert((Unknown && False) == False)
    assert((Unknown && Unknown) == Unknown)
    assert((Unknown && True) == Unknown)
    assert((True && False) == False)
    assert((True && Unknown) == Unknown)
    assert((True && True) == True)

  "And &&" in:
    for a <- L3.values; b <- L3.values do assert((a && b) == (a & b))

  "Or |" in:
    assert((False | False) == False)
    assert((False | Unknown) == Unknown)
    assert((False | True) == True)
    assert((Unknown | False) == Unknown)
    assert((Unknown | Unknown) == Unknown)
    assert((Unknown | True) == True)
    assert((True | False) == True)
    assert((True | Unknown) == True)
    assert((True | True) == True)

  "Or ||" in:
    for a <- L3.values; b <- L3.values do assert((a || b) == (a | b))

  "Łukasiewicz semantics" - {
    //"xor (?)" in:
    //  assert((False xor False) == False)
    //  assert((False xor Unknown) == Unknown)
    //  assert((False xor True) == True)
    //  assert((Unknown xor False) == Unknown)
    //  assert((Unknown xor Unknown) == Unknown)
    //  assert((Unknown xor True) == Unknown)
    //  assert((True xor False) == True)
    //  assert((True xor Unknown) == Unknown)
    //  assert((True xor True) == False)

    "Subjunction -->" in:
      assert(False --> False == True)
      assert(False --> Unknown == True)
      assert(False --> True == True)
      assert(Unknown --> False == Unknown)
      assert(Unknown --> Unknown == True) // <-- K3 would return Unknown
      assert(Unknown --> True == True)
      assert(True --> False == False)
      assert(True --> Unknown == Unknown)
      assert(True --> True == True)
  }

  "Kleene semantics (?)" - {
    "k3xor" in:
      assert((False k3xor False) == False)
      assert((False k3xor Unknown) == Unknown)
      assert((False k3xor True) == True)
      assert((Unknown k3xor False) == Unknown)
      assert((Unknown k3xor Unknown) == Unknown)
      assert((Unknown k3xor True) == Unknown)
      assert((True k3xor False) == True)
      assert((True k3xor Unknown) == Unknown)
      assert((True k3xor True) == False)

    "k3subjunction" in:
      assert((False k3subjunction False) == True)
      assert((False k3subjunction Unknown) == True)
      assert((False k3subjunction True) == True)
      assert((Unknown k3subjunction False) == Unknown)
      assert((Unknown k3subjunction Unknown) == Unknown) // <-- S3 would return True
      assert((Unknown k3subjunction True) == True)
      assert((True k3subjunction False) == False)
      assert((True k3subjunction Unknown) == Unknown)
      assert((True k3subjunction True) == True)

    "a k3subjunction b === !a | b" in:
      def f(a: L3, b: L3) = !a | b
      for a <- L3.values; b <- L3.values do
        withClue(s"$a --> $b: ")
          assert((a k3subjunction b) == f(a, b))
  }
