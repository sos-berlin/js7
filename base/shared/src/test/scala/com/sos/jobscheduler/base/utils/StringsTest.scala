package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.base.utils.Strings._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class StringsTest extends FreeSpec {

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
  }

  "isExtendedIdentifier" in {
    assert(isExtendedIdentifier("a"))
    assert(isExtendedIdentifier("aaa"))
    assert(isExtendedIdentifier("a12"))
    assert(isExtendedIdentifier("a_"))
    assert(isExtendedIdentifier("a___"))
    assert(isExtendedIdentifier("a_b"))
    assert(isExtendedIdentifier("a-b"))
    assert(isExtendedIdentifier("åäöüÅÄÖÜßµπæç"))
    assert(isExtendedIdentifier("български")) // Bulgarisch
    assert(isExtendedIdentifier("ქართული")) // Georgisch
    assert(isExtendedIdentifier("片仮名"))     // Japanisch: Katakana
    assert(isExtendedIdentifier("カタカナ"))    // Japanisch: Katakana
    assert(isExtendedIdentifier("平仮名"))     // Japanisch: Hiragana
    assert(isExtendedIdentifier("漢字"))       // Japanisch: Kanji
    assert(isExtendedIdentifier("中文字"))     // Chinesisch
    assert(isExtendedIdentifier("ㄓㄨㄥ"))     // Chinesisch: Pinyin
    assert(isExtendedIdentifier("漢字"))       // Chinesisch: Han-Schrift
    assert(isExtendedIdentifier("汉字"))       // Chinesisch: Han-Schrift
    assert(isExtendedIdentifier("ひらがな"))

    assert(!isExtendedIdentifier(""))
    assert(!isExtendedIdentifier("$"))
    assert(!isExtendedIdentifier("_"))
    assert(!isExtendedIdentifier("1"))
    assert(!isExtendedIdentifier("-a"))
    assert(!isExtendedIdentifier("a-"))
    assert(!isExtendedIdentifier("a$"))
    assert(!isExtendedIdentifier("a$"))
    assert(!isExtendedIdentifier("a\u0000"))
    assert(!isExtendedIdentifier("a\u0001"))
    assert(!isExtendedIdentifier("a\u0002"))
    assert(!isExtendedIdentifier("a\u0003"))
    assert(!isExtendedIdentifier("a\u0004"))
    assert(!isExtendedIdentifier("a\u0005"))
    assert(!isExtendedIdentifier("a\u0006"))
    assert(!isExtendedIdentifier("a\u0007"))
    assert(!isExtendedIdentifier("a\u0008"))
    assert(!isExtendedIdentifier("a\u0009"))
    assert(!isExtendedIdentifier("a\u000a"))
    assert(!isExtendedIdentifier("a\u007f"))
    assert(!isExtendedIdentifier("a\u0080"))
    assert(!isExtendedIdentifier("a\u009f"))
  }
}
