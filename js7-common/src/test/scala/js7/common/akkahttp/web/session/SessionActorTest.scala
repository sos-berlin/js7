package js7.common.akkahttp.web.session

import js7.base.auth.UserId
import js7.base.problem.Problem
import org.scalatest.freespec.AnyFreeSpec

final class SessionActorTest extends AnyFreeSpec
{
  "logNonMatchingVersion (manual test)" in {
    assert(check(None) == Left(Problem("User:USER did not send its version")))
    assert(check(Some("1.2.4")) == Right(()))
    assert(check(Some("1.2.3")) == Right(()))
    assert(check(Some("1.2.2")) == Right(()))
    assert(check(Some("1.2.0")) == Right(()))
    assert(check(Some("1.1.2")) == Left(Problem("User:USER client version 1.1.2 does not match this server version 1.2.3")))
    assert(check(Some("1.3.2")) == Left(Problem("User:USER client version 1.3.2 does not match this server version 1.2.3")))
    assert(check(Some("2.0.0")) == Left(Problem("User:USER client version 2.0.0 does not match this server version 1.2.3")))

    def check(v: Option[String]) =
      SessionActor.checkNonMatchingVersion(UserId("USER"), v, "1.2.3")
  }
}
