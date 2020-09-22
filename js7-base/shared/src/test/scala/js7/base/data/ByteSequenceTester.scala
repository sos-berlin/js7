package js7.base.data

import cats.syntax.monoid._
import java.io.ByteArrayOutputStream
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.data.ByteSequence.ops._
import js7.base.problem.Problem
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random

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

  "equality" in {
    assert(ByteArray.empty == ByteArray(""))
    assert(ByteArray.empty === ByteArray(""))
    assert(ByteArray(0x31, 0x32, 0x33) ==  ByteArray("123"))
    assert(ByteArray(0x31, 0x32, 0x33) === ByteArray("123"))
    assert(ByteArray(0x31, 0x32, 0x33) !=  ByteArray("12X"))
    assert(ByteArray(0x31, 0x32, 0x33) !== ByteArray("12X"))
    assert(ByteArray(0x31, 0x32, 0xFF) !=  ByteArray("123"))
    assert(ByteArray(0x31, 0x32, 0xFF) !== ByteArray("123"))
  }

  "toArray" in {
    val a = Array(1.toByte)
    assert(ByteSeq.unsafeWrap(a).toArray ne a)
    assert(ByteSeq.unsafeWrap(a).toArray sameElements a)
  }

  "toByteArray" in {
    assert(ByteSeq(1.toByte, 2.toByte).toByteArray == ByteArray(1.toByte, 2.toByte))
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
    val byteSeq = ByteSeq("ab")
    assert(byteSeq(0) == 'a'.toByte)
    assert(byteSeq(1) == 'b'.toByte)
    intercept[RuntimeException](byteSeq(-1))
    intercept[RuntimeException](byteSeq(2))
  }

  "headOption" in {
    val byteSeq = ByteSeq("ab")
    assert(byteSeq.headOption == Some('a'.toByte))
    assert(ByteSeq.empty.headOption == None)
  }

  "lastOption" in {
    val byteSeq = ByteSeq("ab")
    assert(byteSeq.lastOption == Some('b'.toByte))
    assert(ByteSeq.empty.lastOption == None)
  }

  "indexOf" in {
    val byteSeq = ByteSeq("aba")
    assert(byteSeq.indexOf('a') == 0)
    assert(byteSeq.indexOf('a', 0) == 0)
    assert(byteSeq.indexOf('a', 1) == 2)
    assert(byteSeq.indexOf('a', 2) == 2)
    assert(byteSeq.indexOf('a', 3) == -1)
    assert(byteSeq.indexOf('x') == -1)
  }

  "drop" in {
    val byteSeq = ByteSeq("abc")
    assert(byteSeq.drop(0) == byteSeq)
    assert(byteSeq.drop(1) == ByteSeq("bc"))
    assert(byteSeq.drop(3) == ByteSeq.empty)
    assert(byteSeq.drop(99) == ByteSeq.empty)
  }

  "slice" in {
    val byteSeq = ByteSeq("abc")
    assert(byteSeq.slice(0, 3) == byteSeq)
    assert(byteSeq.slice(0, 99) == byteSeq)
    assert(byteSeq.slice(1, 0) == ByteSeq.empty)
    assert(byteSeq.slice(1, 1) == ByteSeq.empty)
    assert(byteSeq.slice(1, 2) == ByteSeq("b"))
    assert(byteSeq.slice(99, 99) == ByteSeq.empty)
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

  "writeToStream" in {
    val byteArray = ByteArray(Random.nextString(10001))
    val out = new ByteArrayOutputStream
    byteArray.writeToStream(out)
    assert(ByteArray(out.toByteArray) == byteArray)
  }

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
