package js7.base.web

import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class UriTest extends AnyFreeSpec
{
  "/" in {
    assert(Uri("/") / "/" == Uri("/"))
    assert(Uri("/") / "B" == Uri("/B"))
    assert(Uri("/") / "B/" == Uri("/B/"))
    assert(Uri("/A") / "B" == Uri("/A/B"))
    assert(Uri("/A/") / "/B" == Uri("/A/B"))
    assert(Uri("/A/") / "/B/" == Uri("/A/B/"))
    assert(Uri("/A") / "B" == Uri("/A/B"))
  }
}
