package js7.base.data

import cats.syntax.monoid._
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.data.ByteSequence.ops._
import js7.base.problem.Problem
import org.scalatest.freespec.AnyFreeSpec

abstract class ByteSequenceTester[ByteSeq](implicit ByteSeq: ByteSequence[ByteSeq])
extends AnyFreeSpec
{
  "empty" in {
    assert(ByteSeq.empty.length == 0)
  }

  "fromString" in {
    assert(ByteSeq.fromString("å").unsafeArray sameElements Array(0xc3.toByte, 0xa5.toByte))
    assert(ByteSeq.fromString("A") == ByteSeq("A"))
  }

  "fromArray" in {
    val a = Array(1.toByte)
    assert(ByteSeq.fromArray(a).unsafeArray ne a)
    assert(ByteSeq.fromArray(a).unsafeArray sameElements a)
    assert(ByteSeq.fromArray(a) == ByteSeq(a))
  }

  "toArray" in {
    val a = Array(1.toByte)
    assert(ByteSeq.unsafeWrap(a).toArray ne a)
    assert(ByteSeq.unsafeWrap(a).toArray sameElements a)
  }

  "random" in {
    assert(ByteSeq.random(3).length == 3)
  }

  "isEmpty" in {
    assert(ByteSeq.empty.isEmpty)
    assert(!ByteSeq("X").isEmpty)
  }

  "nonEmpty" in {
    assert(!ByteSeq.empty.nonEmpty)
    assert(ByteSeq("X").nonEmpty)
  }

  "length" in {
    assert(ByteSeq.empty.length == 0)
    assert(ByteSeq("a").length == 1)
    assert(ByteSeq("å").length == 2)
  }

  "at" in {
    val byteArray = ByteSeq("ab")
    assert(byteArray(0) == 'a'.toByte)
    assert(byteArray(1) == 'b'.toByte)
    intercept[RuntimeException](byteArray(-1))
    intercept[RuntimeException](byteArray(2))
  }

  "lastOption" in {
    val byteArray = ByteSeq("ab")
    assert(byteArray.lastOption == Some('b'.toByte))
    assert(ByteSeq.empty.lastOption == None)
  }

  "indexOf" in {
    val byteArray = ByteSeq("aba")
    assert(byteArray.indexOf('a') == 0)
    assert(byteArray.indexOf('a', 0) == 0)
    assert(byteArray.indexOf('a', 1) == 2)
    assert(byteArray.indexOf('a', 2) == 2)
    assert(byteArray.indexOf('a', 3) == -1)
    assert(byteArray.indexOf('x') == -1)
  }

  "drop" in {
    val byteSeq = ByteSeq("abc")
    assert(byteSeq.drop(0) == byteSeq)
    assert(byteSeq.drop(1) == ByteSeq("bc"))
    assert(byteSeq.drop(3) == ByteSeq.empty)
    assert(byteSeq.drop(99) == ByteSeq.empty)
  }

  "slice" in {
    val byteArray = ByteSeq("abc")
    assert(byteArray.slice(0, 3) == byteArray)
    assert(byteArray.slice(0, 99) == byteArray)
    assert(byteArray.slice(1, 0) == ByteSeq.empty)
    assert(byteArray.slice(1, 1) == ByteSeq.empty)
    assert(byteArray.slice(1, 2) == ByteSeq("b"))
    assert(byteArray.slice(99, 99) == ByteSeq.empty)
  }

  "utf8String" in {
    assert(ByteSeq("aå").utf8String == "aå")
    assert(ByteSeq('A', 'B').utf8String == "AB")
    assert(ByteSeq('A', 0x80, 'B').utf8String == "A\ufffdB")
  }

  "utf8StringTruncateAt" in {
    assert(ByteSeq("").utf8StringTruncateAt(0) == "")
    assert(ByteSeq("ABCDE").utf8StringTruncateAt(0) == "...")
    assert(ByteSeq("ABCD").utf8StringTruncateAt(4) == "ABCD")
    assert(ByteSeq("ABCDE").utf8StringTruncateAt(4) == "A...")
  }

  "combine" in {
    assert(ByteSeq("ab").combine(ByteSeq("123")) == ByteSeq("ab123"))
  }

  //"toInputStream" in {
  //  for (size <- Iterator(0, 1, 10000, 100000)) {
  //    val byteVector = ByteSeq.random(size)
  //    assert(inputStreamToByteSeq(byteVector.toInputStream) == byteVector)
  //  }
  //}

  "ByteSeqInputStream mark and reset (.toInputStream)" in {
    val byteSeq = ByteSequence[ByteSeq].random(100000)
    val in = byteSeq.toInputStream
    assert(in.markSupported)
    val array1, array2 = new Array[Byte](10000)
    in.read(array1)
    in.mark(Int.MaxValue)
    in.read(array1)
    in.reset()
    in.read(array2)
    assert(array1 sameElements array2)
    assert(ByteSeq.unsafeWrap(array1) == byteSeq.slice(10000, 20000))
  }

  "parseJsonAs" in {
    final case class A(value: Int)
    implicit val jsonCodec = deriveCodec[A]

    val byteSeq = ByteSeq("""{ "value": 7 }""")
    assert(byteSeq.parseJsonAs[A] == Right(A(7)))

    val wrongByteSeq = ByteSeq("""{ "alien": 7 }""")
    assert(wrongByteSeq.parseJsonAs[A] == Left(Problem("JSON DecodingFailure at .value: Attempt to decode value on failed cursor")))

    val noJsonByteSeq = ByteSeq("""XXX""")
    assert(noJsonByteSeq.parseJsonAs[A] == Left(Problem("JSON ParsingFailure: expected json value got 'XXX' (line 1, column 1)")))
  }
}
