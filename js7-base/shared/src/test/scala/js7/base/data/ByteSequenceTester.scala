package js7.base.data

import cats.syntax.monoid._
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.ReadOnlyBufferException
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.data.ByteSequence.ops._
import js7.base.data.ByteSequenceTester._
import js7.base.problem.Problem
import js7.base.utils.IOUtils
import js7.base.utils.SyncResource.syntax.RichResource
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random

abstract class ByteSequenceTester[ByteSeq](implicit ByteSeq: ByteSequence[ByteSeq])
extends AnyFreeSpec
{
  "empty" in {
    assert(ByteSeq.empty.length == 0)
  }

  "apply" in {
    val byteSeq = ByteSeq("ab")
    assert(byteSeq(0) == 'a'.toByte)
    assert(byteSeq(1) == 'b'.toByte)
    intercept[RuntimeException](byteSeq(-1))
    intercept[RuntimeException](byteSeq(2))
  }

  "fromString" in {
    assert(ByteSeq.fromString("å").unsafeArray sameElements Array(0xc3.toByte, 0xa5.toByte))
    assert(ByteSeq.fromString("A") == ByteSeq("A"))
  }

  "fromArray" in {
    val a = Array('a'.toByte, 'b'.toByte, 'c'.toByte)
    assert(ByteSeq.fromArray(Array.empty).isEmpty)
    assert(ByteSeq.fromArray(a).unsafeArray ne a)
    assert(ByteSeq.fromArray(a).unsafeArray sameElements a)
    assert(ByteSeq.fromArray(a) == ByteSeq(a))
    assert(ByteSeq.fromArray(a) == ByteSeq(a))
    assert(ByteSeq.fromArray(a, -99, 99) == ByteSeq(a))
    assert(ByteSeq.fromArray(a, 99, -99) == ByteSeq.empty)
    assert(ByteSeq.fromArray(a, 0, 3) == ByteSeq(a))
    assert(ByteSeq.fromArray(a, 0, 3) == ByteSeq("abc"))
    assert(ByteSeq.fromArray(a, 0, 2) == ByteSeq("ab"))
    assert(ByteSeq.fromArray(a, 0, 1) == ByteSeq("a"))
    assert(ByteSeq.fromArray(a, 0, 0) == ByteSeq.empty)
    assert(ByteSeq.fromArray(a, 2, 2) == ByteSeq.empty)
    assert(ByteSeq.fromArray(a, 3, 2) == ByteSeq.empty)
  }

  "fromSeq" in {
    val a = Seq[Byte](1, 2)
    assert(ByteSeq.fromSeq(a).unsafeArray sameElements a)
  }

  "fromMimeBase64" in {
    assert(ByteArray.fromMimeBase64(mimeBase64string) == Right(ByteArray(mimeByte64Bytes)))
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

  "toByteSequence" in {
    assert(ByteSeq(1.toByte, 2.toByte).toByteSequence[ByteArray] == ByteArray(1.toByte, 2.toByte))
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

  "show" in {
    assert(ByteSeq.empty.show == "»«")
    assert(ByteSeq("SHORT").show == "»SHORT«")
    assert(ByteSeq("SHORT\n").show == "»SHORT␊«")
    assert(ByteSeq("SHORT\r\n").show == "»SHORT␍␊«")
    assert(ByteSeq(0x41, 0xf0, 0xf1, 0xf2, 0xf3).show == ByteSeq.typeName + "(»A����« 41f0f1f2 f3)")
    assert(ByteSeq("abcdefghijklmnopqrstuvwxyzÄÖ\nABCDEFGHIJKLMNOPQRSTUVWXYZ").show ==
      ByteSeq.typeName + "(" +
        "»abcdefghijklmnopqrstuvwxyz����␊A…« " +
        "61626364 65666768 696a6b6c 6d6e6f70 71727374 75767778 797ac384 c3960a41..., 57 bytes)")
  }

  "toStringAndHexRaw" in {
    assert(ByteSeq("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ").toStringAndHexRaw() ==
      "»abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ« " +
        "61626364 65666768 696a6b6c 6d6e6f70 71727374 75767778 797a4142 43444546 4748494a 4b4c4d4e 4f505152 53545556 5758595a")
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
    assert(byteSeq.indexOf('b', 0, 0) == -1)
    assert(byteSeq.indexOf('b', 0, 1) == -1)
    assert(byteSeq.indexOf('b', 0, 2) == 1)
    assert(byteSeq.indexOf('x') == -1)
  }

  "startsWith" in {
    assert(ByteSeq.empty startsWith ByteSeq.empty)
    assert(!ByteSeq.empty.startsWith(ByteSeq("xy")))
    assert(ByteSeq("a") startsWith ByteSeq.empty)
    assert(ByteSeq("ab") startsWith ByteSeq.empty)
    assert(ByteSeq("abc") startsWith ByteSeq("a"))
    assert(ByteSeq("abc") startsWith ByteSeq("ab"))
    assert(ByteSeq("abc") startsWith ByteSeq("abc"))
    assert(!ByteSeq("abc").startsWith(ByteSeq("abx")))
    assert(!ByteSeq("abc").startsWith(ByteSeq("x")))
    assert(!ByteSeq("abc").startsWith(ByteSeq("abcd")))
    assert(!ByteSeq("abc").startsWith(ByteSeq("-abc")))
  }

  "endsWith" in {
    assert(ByteSeq.empty endsWith ByteSeq.empty)
    assert(!ByteSeq.empty.endsWith(ByteSeq("xy")))
    assert(ByteSeq("a") endsWith ByteSeq.empty)
    assert(ByteSeq("ab") endsWith ByteSeq.empty)
    assert(ByteSeq("abc") endsWith ByteSeq("c"))
    assert(ByteSeq("abc") endsWith ByteSeq("bc"))
    assert(ByteSeq("abc") endsWith ByteSeq("abc"))
    assert(!ByteSeq("abc").endsWith(ByteSeq("xbc")))
    assert(!ByteSeq("abc").endsWith(ByteSeq("x")))
    assert(!ByteSeq("abc").endsWith(ByteSeq("abcd")))
    assert(!ByteSeq("abc").endsWith(ByteSeq("-abc")))
  }

  "take" in {
    val byteSeq = ByteSeq("abc")
    assert(byteSeq.take(0) == ByteSeq.empty)
    assert(byteSeq.take(1) == ByteSeq("a"))
    assert(byteSeq.take(3) == byteSeq)
    assert(byteSeq.take(99) == byteSeq)
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

  "chunk" in {
    val byteSeq = ByteSeq("abcd")
    assert(byteSeq.chunk(1) == Seq(ByteSeq("a"), ByteSeq("b"), ByteSeq("c"), ByteSeq("d")))
    assert(byteSeq.chunk(2) == Seq(ByteSeq("ab"), ByteSeq("cd")))
    assert(byteSeq.chunk(3) == Seq(ByteSeq("abc"), ByteSeq("d")))
    assert(byteSeq.chunk(4) == Seq(ByteSeq("abcd")))
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

  "toByteBuffer" in {
    val byteBuffer = ByteSeq("abc").toByteBuffer
    assert(byteBuffer.get() == 'a')
    intercept[ReadOnlyBufferException] {
      byteBuffer.put('x'.toByte)
    }
  }

  "fromInputStreamUnlimited" in {
    for (size <- Iterator(0, 1, 10000, 100001)) {
      val byteSeq = ByteSequence[ByteSeq].random(size)
      assert(ByteSeq.fromInputStreamUnlimited(new ByteArrayInputStream(byteSeq.toArray)) == byteSeq)
    }
  }

  "fromInputStreamLimited" in {
    val byteSeq = ByteSequence[ByteSeq].random(101)
    def newInputStream = new ByteArrayInputStream(byteSeq.toArray)
    assert(ByteSeq.fromInputStreamLimited(newInputStream, 99) == Left(byteSeq.take(99)))
    assert(ByteSeq.fromInputStreamLimited(newInputStream, 100) == Left(byteSeq.take(100)))
    assert(ByteSeq.fromInputStreamLimited(newInputStream, 101) == Right(byteSeq))
    assert(ByteSeq.fromInputStreamLimited(newInputStream, 102) == Right(byteSeq))
  }

  "fromInputStreamLimited at inputStreamBufferSize" in {
    for (diff <- -2 to 2) {
      val size = 2 * ByteArray.inputStreamBufferSize + diff
      val byteSeq = ByteSequence[ByteSeq].random(size)
      def newInputStream = new ByteArrayInputStream(byteSeq.toArray)
      assert(ByteSeq.fromInputStreamLimited(newInputStream, size - 2) == Left(byteSeq.take(size - 2)))
      assert(ByteSeq.fromInputStreamLimited(newInputStream, size - 1) == Left(byteSeq.take(size - 1)))
      assert(ByteSeq.fromInputStreamLimited(newInputStream, size + 1) == Right(byteSeq))
      assert(ByteSeq.fromInputStreamLimited(newInputStream, size + 2) == Right(byteSeq))
    }
  }

  "toInputStream" in {
    for (size <- Iterator(0, 1, 10000, 100001)) {
      val byteSeq = ByteSequence[ByteSeq].random(size)
      assert(ByteSeq.fromInputStreamUnlimited(byteSeq.toInputStream) == byteSeq)
    }
  }

  "toInputStreamResource" in {
    val byteSeq = ByteSequence[ByteSeq].random(100001)
    val x = byteSeq.toInputStreamResource.useSync { in =>
      val out = new ByteArrayOutputStream
      IOUtils.copyStream(in, out)
      ByteSeq.unsafeWrap(out.toByteArray)
    }
    assert(x == byteSeq)
  }

  "toInputStream mark and reset (ByteSequenceInputStream)" in {
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

object ByteSequenceTester
{
  val mimeByte64Bytes: Array[Byte] = (0 to 0xff).map(_.toByte).toArray
  val mimeBase64string =
   "AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEyMzQ1Njc4\r\n" +
   "OTo7PD0+P0BBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWltcXV5fYGFiY2RlZmdoaWprbG1ub3Bx\r\n" +
   "cnN0dXZ3eHl6e3x9fn+AgYKDhIWGh4iJiouMjY6PkJGSk5SVlpeYmZqbnJ2en6ChoqOkpaanqKmq\r\n" +
   "q6ytrq+wsbKztLW2t7i5uru8vb6/wMHCw8TFxsfIycrLzM3Oz9DR0tPU1dbX2Nna29zd3t/g4eLj\r\n" +
   "5OXm5+jp6uvs7e7v8PHy8/T19vf4+fr7/P3+/w=="
}
