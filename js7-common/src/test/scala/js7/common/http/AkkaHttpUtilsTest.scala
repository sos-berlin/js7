package js7.common.http

import akka.http.scaladsl.model.{Uri => AkkaUri}
import js7.base.web.Uri
import js7.common.http.AkkaHttpUtils.*
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AkkaHttpUtilsTest extends AnyFreeSpec
{

  "Uri asAkka, asUri" in {
    val uri = "https://example.com:1/path/?query=value#hash"
    assert(Uri(uri).asAkka == AkkaUri(uri))
    assert(AkkaUri(uri).asUri == Uri(uri))
  }
}
