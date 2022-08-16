package js7.base.web

import js7.base.test.Test

/**
  * @author Joacim Zschimmer
  */
final class UriTest extends Test
{
  "/" in {
    assert(Uri("/") / "" == Uri("/"))
    assert(Uri("/") / "/" == Uri("/"))
    assert(Uri("/") / "B" == Uri("/B"))
    assert(Uri("/") / "B/" == Uri("/B/"))
    assert(Uri("https://example.com") / "B" == Uri("https://example.com/B"))
    assert(Uri("https://example.com/") / "/B" == Uri("https://example.com/B"))
    assert(Uri("https://example.com/") / "/B/" == Uri("https://example.com/B/"))
    assert(Uri("https://example.com") / "B" == Uri("https://example.com/B"))
  }

  "/?" in {
    assert(Uri("/") /? "" == Uri("/"))
    assert(Uri("/") /? "B" == Uri("/B"))
    assert(Uri("/") /? "B/" == Uri("/B/"))
    assert(Uri("https://example.com") /? "B" == Uri("https://example.com/B"))
    assert(Uri("https://example.com/") /? "/B" == Uri("https://example.com/B"))
    assert(Uri("https://example.com/") /? "/B/" == Uri("https://example.com/B/"))
    assert(Uri("https://example.com") /? "B" == Uri("https://example.com/B"))
  }

  "stripPath" in {
    assert(Uri("/").stripPath == Uri("/"))

    assert(Uri("https://host/").stripPath == Uri("https://host/"))
    assert(Uri("https://host/PATH").stripPath == Uri("https://host/"))
    assert(Uri("https://host/PATH/MORE").stripPath == Uri("https://host/"))
    assert(Uri("https://host/?QUERY").stripPath == Uri("https://host/"))

    assert(Uri("//host/").stripPath == Uri("//host/"))
    assert(Uri("//host/PATH").stripPath == Uri("//host/"))
    assert(Uri("//host/PATH/MORE").stripPath == Uri("//host/"))
    assert(Uri("//host/?QUERY").stripPath == Uri("//host/"))
  }
}
