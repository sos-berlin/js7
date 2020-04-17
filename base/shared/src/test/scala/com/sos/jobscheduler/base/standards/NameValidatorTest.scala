package com.sos.jobscheduler.base.standards

import com.sos.jobscheduler.base.standards.NameValidator._
import org.scalatest.FreeSpec
import scala.jdk.CollectionConverters._

/**
  * @author Joacim Zschimmer
  */
final class NameValidatorTest extends FreeSpec {

  "isValid" in {
    assert(isValid("a"))
    assert(isValid("aaa"))
    assert(isValid("a12"))
    assert(isValid("a_"))
    assert(isValid("a___"))
    assert(isValid("a_b"))
    assert(isValid("a-b"))
    assert(isValid("a.b"))
    assert(isValid("åäöüÅÄÖÜßµπæç"))
    assert(isValid("български")) // Bulgarisch
    assert(isValid("ქართული")) // Georgisch
    assert(isValid("片仮名"))     // Japanisch: Katakana
    assert(isValid("カタカナ"))    // Japanisch: Katakana
    assert(isValid("平仮名"))     // Japanisch: Hiragana
    assert(isValid("漢字"))       // Japanisch: Kanji
    assert(isValid("中文字"))     // Chinesisch
    assert(isValid("ㄓㄨㄥ"))     // Chinesisch: Pinyin
    assert(isValid("漢字"))       // Chinesisch: Han-Schrift
    assert(isValid("汉字"))       // Chinesisch: Han-Schrift
    assert(isValid("ひらがな"))

    assert(!isValid(""))
    assert(!isValid("$"))
    assert(!isValid("_"))
    assert(!isValid("1"))
    assert(!isValid("-a"))
    assert(!isValid("a-"))
    assert(!isValid("a$"))
    assert(!isValid("a$"))
    assert(!isValid("/"))
    assert(!isValid("a/b"))
    assert(!isValid("a\u0000"))
    assert(!isValid("a\u0001"))
    assert(!isValid("a\u0002"))
    assert(!isValid("a\u0003"))
    assert(!isValid("a\u0004"))
    assert(!isValid("a\u0005"))
    assert(!isValid("a\u0006"))
    assert(!isValid("a\u0007"))
    assert(!isValid("a\u0008"))
    assert(!isValid("a\u0009"))
    assert(!isValid("a\u000a"))
    assert(!isValid("a\u007f"))
    assert(!isValid("a\u0080"))
    assert(!isValid("a\u009f"))
  }

  "Surrogates are allowed" in {
    val allowed = "🍏🍎🍐🍊🍋🍌🍉🍇🍓🍈🍒🍑🍍🥥🥝🍅🍆🥑🥦🥒🌶🌽🥕🥔🍠🥐🍞🥖🥨🧀🥚🍳🥞🥓🥩🍗🍖🌭🍔🍟" +
                  "🍕🥪🥙🌮🌯🥗🥘🥘🥫🍝🍜🍲🍛🍱🥟🍤🍙🍚🍘🍥🍢🍡🍧🍨🍦🥧🍰🎂🍮🍭🍬🍫🍿🍩🍪🌰🥜🍯🥛🍼" +
                  "🐣🐥🦆🦅🦉🦇🐺🐗🐴🦄🐝🐛🦋🐌🐚🐞🐜🦗🕷🕸🦂🐢🐍🦎🦖🦕🐙🦑🦐🦀🐡🐠🐟🐬🐳🐋🐊🐆🦓🦍" +
                  "🐘🐪🐫🦒🐃🐄🐎🐏🐑🦌🐕🐩🐓🕊🐁🐿🐉🎄🌲" +
                  "🇸🇪"
    for ((cp, i) <- allowed.codePoints.iterator.asScala.zipWithIndex) {
      val identifier = new String(Character.toChars(cp))
      assert(isValid(identifier), f"#$i U+${cp.toInt}%04x $identifier")
    }
    assert(isValid(allowed))
    assert(!isValid("☺︎"))
    assert(!isValid("->"))
  }
}
