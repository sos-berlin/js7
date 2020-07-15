package js7.base.web

import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class UriTest extends AnyFreeSpec
{
  "/" in {
    assert(Uri("/") / "" == Uri("/"))
    assert(Uri("/") / "/" == Uri("/"))
    assert(Uri("/") / "B" == Uri("/B"))
    assert(Uri("/") / "B/" == Uri("/B/"))
    assert(Uri("http://example.com") / "B" == Uri("http://example.com/B"))
    assert(Uri("http://example.com/") / "/B" == Uri("http://example.com/B"))
    assert(Uri("http://example.com/") / "/B/" == Uri("http://example.com/B/"))
    assert(Uri("http://example.com") / "B" == Uri("http://example.com/B"))
  }

  "/?" in {
    assert(Uri("/") /? "" == Uri("/"))
    assert(Uri("/") /? "B" == Uri("/B"))
    assert(Uri("/") /? "B/" == Uri("/B/"))
    assert(Uri("http://example.com") /? "B" == Uri("http://example.com/B"))
    assert(Uri("http://example.com/") /? "/B" == Uri("http://example.com/B"))
    assert(Uri("http://example.com/") /? "/B/" == Uri("http://example.com/B/"))
    assert(Uri("http://example.com") /? "B" == Uri("http://example.com/B"))
  }
}
