package js7.data.session

import js7.base.problem.Problem
import org.scalatest.freespec.AnyFreeSpec

final class HttpSessionApiTest extends AnyFreeSpec
{
  "logNonMatchingVersion (manual test)" in {
    assert(check(None) == Left(Problem("Server did not return its version")))
    assert(check(Some("1.2.4")) == Right(()))
    assert(check(Some("1.2.3")) == Right(()))
    assert(check(Some("1.2.2")) == Right(()))
    assert(check(Some("1.2.0")) == Right(()))
    assert(check(Some("1.1.2")) == Left(Problem("Server version 1.1.2 does not match this client version 1.2.3")))
    assert(check(Some("1.3.2")) == Left(Problem("Server version 1.3.2 does not match this client version 1.2.3")))
    assert(check(Some("2.0.0")) == Left(Problem("Server version 2.0.0 does not match this client version 1.2.3")))

    def check(v: Option[String]) =
      HttpSessionApi.checkNonMatchingVersion(v, "1.2.3")
  }
}
