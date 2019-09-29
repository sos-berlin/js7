package com.sos.jobscheduler.common.http

import akka.util.ByteString
import com.sos.jobscheduler.common.http.AkkaHttpUtils._
import org.scalatest.FreeSpec
import scodec.bits.ByteVector

/**
  * @author Joacim Zschimmer
  */
final class AkkaHttpUtilsTest extends FreeSpec
{
  "Akka ByteString and Scodec ByteVector" in {
    assert((ByteString('A', 'B')).toByteVector == ByteVector('A', 'B'))
    assert((ByteString('A', 'B') ++ ByteString('C')).toByteVector == ByteVector('A', 'B', 'C'))

    assert((ByteVector('A', 'B')).toByteString == ByteString('A', 'B'))
    assert((ByteVector('A', 'B') ++ ByteVector('C')).toByteString == ByteString('A', 'B', 'C'))
  }
}
