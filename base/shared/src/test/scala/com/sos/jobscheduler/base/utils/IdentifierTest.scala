package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.base.utils.Identifier._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class IdentifierTest extends FreeSpec {

  "isIdentifier" in {
    assert(isIdentifier("a"))
    assert(isIdentifier("aaa"))
    assert(isIdentifier("a12"))
    assert(isIdentifier("a_"))
    assert(isIdentifier("a___"))
    assert(isIdentifier("a_b"))
    assert(isIdentifier("a-b"))
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
    assert(!isIdentifier("-a"))
    assert(!isIdentifier("a-"))
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
