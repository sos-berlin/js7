package js7.base.data

import js7.base.test.OurTestSuite

final class ByteSeqAsciiCharSequenceTest extends OurTestSuite:

  private val byteSeq = ByteArray.fromString("0123456789åäö")
  private val charSeq = ByteSeqAsciiCharSequence(byteSeq, 2, 12)

  "Test" in:
    assert(charSeq.length == 10)
    assert(charSeq.charAt(0) == '2')
    assert(charSeq.charAt(9) == '�')
    assert((charSeq: CharSequence) == "23456789��")
    assert(charSeq.toString == "23456789��")
    assert(charSeq.subSequence(3, 5) == "56")
    assert(charSeq.subSequence(3, 3) == "")
    succeed

  "Empty" in:
    val empty = ByteSeqAsciiCharSequence(byteSeq, 1, 1)
    assert(empty.length == 0)
    assert(empty.isEmpty)
    assert(empty.subSequence(0, 0) == "")
    assert(empty.toString == "")
    intercept[IndexOutOfBoundsException]:
      empty.charAt(0)
    intercept[IndexOutOfBoundsException]:
      empty.subSequence(0, 1)

    val array = new Array[Char](1)
    empty.getChars(0, 0, array, 0)
    assert(array(0) == '\u0000')

    intercept[IndexOutOfBoundsException]:
      empty.getChars(0, 1, array, 0)


  "IndexOutOfBoundsException" in:
    intercept[IndexOutOfBoundsException]:
      charSeq.charAt(-1)
    intercept[IndexOutOfBoundsException]:
      charSeq.charAt(10)
    intercept[IndexOutOfBoundsException]:
      charSeq.subSequence(-1, 1)
    intercept[IndexOutOfBoundsException]:
      charSeq.subSequence(10, 11)
    intercept[IndexOutOfBoundsException]:
      charSeq.subSequence(1, 0)
