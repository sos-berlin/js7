package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowableEither
import com.sos.jobscheduler.base.utils.ScodecUtils._
import scodec.bits.ByteVector
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ScodecUtilsTest extends AnyFreeSpec
{
  "utf8StringTruncateAt" in {
    assert(ByteVector.encodeUtf8("").orThrow.utf8StringTruncateAt(0) == "")
    assert(ByteVector.encodeUtf8("ABCDE").orThrow.utf8StringTruncateAt(0) == "...")
    assert(ByteVector.encodeUtf8("ABCD").orThrow.utf8StringTruncateAt(4) == "ABCD")
    assert(ByteVector.encodeUtf8("ABCDE").orThrow.utf8StringTruncateAt(4) == "A...")
  }

  "utf8String" in {
    assert(ByteVector('A', 'B').utf8String == "AB")
    assert(ByteVector('A', 0x80, 'B').utf8String == "A\ufffdB")
  }

  "indexOf" in {
    assert(ByteVector.empty.indexOf('*') == -1)
    assert(ByteVector('A', 'B').indexOf('*') == -1)
    assert(ByteVector('A', '*', 'B', '*').indexOf('*') == 1)
    assert(ByteVector('A', '*', 'B', '*').indexOf('*'.toByte, 1) == 1)
    assert(ByteVector('A', '*', 'B', '*').indexOf('*'.toByte, 2) == 3)
  }

  "parseJsonAs" in {
    final case class A(value: Int)
    implicit val jsonCodec = deriveCodec[A]

    val byteVector = ByteVector.encodeUtf8("""{ "value": 7 }""").orThrow
    assert(byteVector.parseJsonAs[A] == Right(A(7)))

    val wrongByteVector = ByteVector.encodeUtf8("""{ "alien": 7 }""").orThrow
    assert(wrongByteVector.parseJsonAs[A] == Left(Problem("JSON DecodingFailure at .value: Attempt to decode value on failed cursor")))

    val noJsonByteVector = ByteVector.encodeUtf8("""XXX""").orThrow
    assert(noJsonByteVector.parseJsonAs[A] == Left(Problem("JSON ParsingFailure: expected json value got 'XXX' (line 1, column 1)")))
  }
}
