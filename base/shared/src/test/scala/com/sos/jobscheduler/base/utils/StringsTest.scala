package js7.base.utils

import js7.base.utils.Strings._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class StringsTest extends AnyFreeSpec {

  "truncateWithEllipsis" in {
    assert("".truncateWithEllipsis(0) == "")
    assert("".truncateWithEllipsis(1) == "")
    assert("".truncateWithEllipsis(4) == "")
    assert("A".truncateWithEllipsis(0) == "A")
    assert("AB".truncateWithEllipsis(0) == "AB")
    assert("ABC".truncateWithEllipsis(0) == "ABC")
    assert("ABCD".truncateWithEllipsis(0) == "...")
    assert("ABCDE".truncateWithEllipsis(0) == "...")
    assert("A".truncateWithEllipsis(1) == "A")
    assert("AB".truncateWithEllipsis(1) == "AB")
    assert("ABC".truncateWithEllipsis(1) == "ABC")
    assert("ABCD".truncateWithEllipsis(1) == "...")
    assert("ABCDE".truncateWithEllipsis(1) == "...")
    assert("A".truncateWithEllipsis(2) == "A")
    assert("AB".truncateWithEllipsis(2) == "AB")
    assert("ABC".truncateWithEllipsis(2) == "ABC")
    assert("ABCD".truncateWithEllipsis(2) == "...")
    assert("ABCDE".truncateWithEllipsis(2) == "...")
    assert("A".truncateWithEllipsis(3) == "A")
    assert("AB".truncateWithEllipsis(3) == "AB")
    assert("ABC".truncateWithEllipsis(3) == "ABC")
    assert("ABCD".truncateWithEllipsis(3) == "...")
    assert("ABCDE".truncateWithEllipsis(3) == "...")
    assert("A".truncateWithEllipsis(4) == "A")
    assert("AB".truncateWithEllipsis(4) == "AB")
    assert("ABC".truncateWithEllipsis(4) == "ABC")
    assert("ABCD".truncateWithEllipsis(4) == "ABCD")
    assert("ABCDE".truncateWithEllipsis(4) == "A...")
    assert("A".truncateWithEllipsis(5) == "A")
    assert("AB".truncateWithEllipsis(5) == "AB")
    assert("ABC".truncateWithEllipsis(5) == "ABC")
    assert("ABCD".truncateWithEllipsis(5) == "ABCD")
    assert("ABCDE".truncateWithEllipsis(5) == "ABCDE")
    assert("ABCDE".truncateWithEllipsis(6) == "ABCDE")
    assert("ABCDEF".truncateWithEllipsis(6) == "ABCDEF")
    assert("ABCDEFG".truncateWithEllipsis(6) == "ABC...")
    assert("ABCDEFGHIJKLMNOPQRSTUVWXYZ".truncateWithEllipsis(6) == "ABC...")
    val expected = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx...(length 100)"
    assert(("x" * 100).truncateWithEllipsis(50, showLength = true) == expected)
    assert(expected.length == 50)
  }

  "replaceChar" in {
    val empty = ""
    assert(empty.replaceChar('-', '+') eq empty)
    val a = "abc"
    assert(a.replaceChar('-', '+') eq a)
    assert(("a-b--c---").replaceChar('-', '+') == "a+b++c+++")
  }

  "reverseDropWhile" in {
    assert("".reverseDropWhile(_ == '/') == "")
    assert("/".reverseDropWhile(_ == '/') == "")
    assert("//".reverseDropWhile(_ == '/') == "")
    assert("/abc".reverseDropWhile(_ == '/') == "/abc")
    assert("/abc//".reverseDropWhile(_ == '/') == "/abc")
  }

  "String ?" in {
    assert((false ?: "STRING") == "")
    assert((true ?: "STRING") == "STRING")
    assert(("STRING" when false) == "")
    assert(("STRING" when true) == "STRING")
  }
}
