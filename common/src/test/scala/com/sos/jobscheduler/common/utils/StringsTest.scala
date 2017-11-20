package com.sos.jobscheduler.common.utils

import com.sos.jobscheduler.common.utils.Strings._
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

  "isIdentifier" in {
    assert(isIdentifier("a"))
    assert(isIdentifier("aaa"))
    assert(isIdentifier("a12"))
    assert(isIdentifier("a_"))
    assert(isIdentifier("a___"))
    assert(isIdentifier("a_b"))
    assert(isIdentifier("åäöüÅÄÖÜßµπæç"))
    assert(isIdentifier("български")) // Bulgarisch
    assert(isIdentifier("ქართული")) // Georgisch
    assert(isIdentifier("片仮名"))     // Japanisch: Katakana
    assert(isIdentifier("カタカナ"))    // Japanisch: Katakana
    assert(isIdentifier("平仮名"))     // Japanisch: Hiragana
    assert(isIdentifier("漢字"))       // Japanisch: Kanji
    assert(isIdentifier("中文字"))     // Chinesisch
    assert(isIdentifier("ㄓㄨㄥ"))     // Chinesisch: Pinyin
    assert(isIdentifier("漢字"))       // Chinesisch: Han-Schrift
    assert(isIdentifier("汉字"))       // Chinesisch: Han-Schrift
    assert(isIdentifier("ひらがな"))

    assert(!isIdentifier(""))
    assert(!isIdentifier("$"))
    assert(!isIdentifier("_"))
    assert(!isIdentifier("1"))
    assert(!isIdentifier("a-b"))
    assert(!isIdentifier("a$"))
    assert(!isIdentifier("a$"))
    assert(!isIdentifier("a\u0000"))
    assert(!isIdentifier("a\u0001"))
    assert(!isIdentifier("a\u0002"))
    assert(!isIdentifier("a\u0003"))
    assert(!isIdentifier("a\u0004"))
    assert(!isIdentifier("a\u0005"))
    assert(!isIdentifier("a\u0006"))
    assert(!isIdentifier("a\u0007"))
    assert(!isIdentifier("a\u0008"))
    assert(!isIdentifier("a\u0009"))
    assert(!isIdentifier("a\u000a"))
    assert(!isIdentifier("a\u007f"))
    assert(!isIdentifier("a\u0080"))
    assert(!isIdentifier("a\u009f"))
  }
}
