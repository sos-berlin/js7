package js7.common.http

import akka.http.scaladsl.model.{Uri => AkkaUri}
import akka.util.ByteString
import js7.base.data.ByteArray
import js7.base.web.Uri
import js7.common.http.AkkaHttpUtils._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AkkaHttpUtilsTest extends AnyFreeSpec
{
  "Akka ByteString and ByteArray" in {
    assert((ByteString('A', 'B')).toByteArray == ByteArray('A', 'B'))
    assert((ByteString('A', 'B') ++ ByteString('C')).toByteArray == ByteArray('A', 'B', 'C'))

    assert((ByteArray('A', 'B')).toByteString == ByteString('A', 'B'))
    assert((ByteArray('A', 'B') ++ ByteArray('C')).toByteString == ByteString('A', 'B', 'C'))

    assert((ByteArray('A', 'B')).toByteString == ByteString('A', 'B'))
    assert((ByteArray('A', 'B') ++ ByteArray('C')).toByteString == ByteString('A', 'B', 'C'))
  }

  "Uri asAkka, asUri" in {
    val uri = "http://example.com:1/path/?query=value#hash"
    assert(Uri(uri).asAkka == AkkaUri(uri))
    assert(AkkaUri(uri).asUri == Uri(uri))
  }
}
