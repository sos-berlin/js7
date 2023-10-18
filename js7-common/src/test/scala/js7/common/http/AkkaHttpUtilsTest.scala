package js7.common.http

import js7.base.test.OurTestSuite
import js7.base.web.Uri
import js7.common.http.PekkoHttpUtils.*
import org.apache.pekko.http.scaladsl.model.Uri as PekkoUri

/**
  * @author Joacim Zschimmer
  */
final class PekkoHttpUtilsTest extends OurTestSuite
:

  "Uri asPekko, asUri" in:
    val uri = "https://example.com:1/path/?query=value#hash"
    assert(Uri(uri).asPekko == PekkoUri(uri))
    assert(PekkoUri(uri).asUri == Uri(uri))
