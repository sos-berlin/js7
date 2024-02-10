package js7.base.utils

import js7.base.utils.Ascii.{byteToPrintableChar, toPrintableChar}
import org.scalatest.freespec.AnyFreeSpec

final class AsciiTest extends AnyFreeSpec:

  "byteToPrintableChar".in:
    assert(byteToPrintableChar(65) == 'A')
    assert(byteToPrintableChar(-27) == '�')
    assert(byteToPrintableChar(127) == '␡')

  "toPrintableChar".in:
    assert(toPrintableChar(' ') == ' ')
    assert(toPrintableChar('A') == 'A')
    assert(toPrintableChar('å') == 'å')
    assert(toPrintableChar('Ж') == 'Ж')
    assert(toPrintableChar('\u7fff') == '\u7fff')
    assert(toPrintableChar('\u0000') == '␀')
    assert(toPrintableChar('\n') == '␊')
    assert(toPrintableChar('\u007f') == '␡')
