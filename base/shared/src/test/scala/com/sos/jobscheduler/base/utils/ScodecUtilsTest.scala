package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowableEither
import com.sos.jobscheduler.base.utils.ScodecUtils._
import org.scalatest.FreeSpec
import scodec.bits.ByteVector

/**
  * @author Joacim Zschimmer
  */
final class ScodecUtilsTest extends FreeSpec
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
}
