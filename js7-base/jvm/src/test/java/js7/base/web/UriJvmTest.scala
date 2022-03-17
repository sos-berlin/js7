package js7.base.web

import js7.base.problem.Problem
import org.scalatest.freespec.AnyFreeSpec

final class UriJvmTest extends AnyFreeSpec
{
  "port" in {
    assert(Uri("http://").port.left.toOption.get.toString.contains("URI"))
    assert(Uri("https://example.com/test").port == Left(Problem("URI has no port number")))
    assert(Uri("https://example.com:1234/test").port == Right(1234))
  }
}
