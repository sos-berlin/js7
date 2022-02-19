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

  "stripPath" in {
    assert(Uri("/").stripPath == Uri("/"))

    assert(Uri("http://host/").stripPath == Uri("http://host/"))
    assert(Uri("http://host/PATH").stripPath == Uri("http://host/"))
    assert(Uri("http://host/PATH/MORE").stripPath == Uri("http://host/"))
    assert(Uri("http://host/?QUERY").stripPath == Uri("http://host/"))

    assert(Uri("//host/").stripPath == Uri("//host/"))
    assert(Uri("//host/PATH").stripPath == Uri("//host/"))
    assert(Uri("//host/PATH/MORE").stripPath == Uri("//host/"))
    assert(Uri("//host/?QUERY").stripPath == Uri("//host/"))
  }
}
