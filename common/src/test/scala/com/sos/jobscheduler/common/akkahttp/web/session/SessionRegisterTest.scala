package com.sos.jobscheduler.common.akkahttp.web.session

import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.auth.{HashedPassword, SessionToken, SimpleUser, UserId}
import com.sos.jobscheduler.base.generic.{Completed, SecretString}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.akkahttp.web.session.SessionRegisterTest._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.duration._
import shapeless.tag

/**
  * @author Joacim Zschimmer
  */
final class SessionRegisterTest extends FreeSpec with ScalatestRouteTest
{
  override def executor = super.executor  // Not implicit

  private val unknownSessionToken = SessionToken(SecretString("UNKNOWN"))
  private lazy val actor = tag[SessionActor[MySession]](
    system.actorOf(SessionActor.props[MySession](MySession.apply)))
  private lazy val sessionRegister = new SessionRegister[MySession](actor, akkaAskTimeout = 99.seconds)
  private var sessionToken = SessionToken(SecretString("INVALID"))

  "Logout unknown SessionToken" in {
    assert(sessionRegister.logout(unknownSessionToken).await(99.seconds) == Completed)
  }

  "session(unknown)" in {
    assert(sessionRegister.session(unknownSessionToken, None).await(99.seconds).isInvalid)
    assert(sessionRegister.session(unknownSessionToken, Some(AUser.id)).await(99.seconds).isInvalid)
  }

  "login" in {
    sessionToken = sessionRegister.login(AUser).await(99.seconds)
  }

  "session" in {
    val someToken = SessionToken(SecretString("X"))
    for (userId ← List(Some(AUser.id), None)) {
      assert(sessionRegister.session(sessionToken, userId).await(99.seconds).map(o ⇒ o.copy(sessionInit = o.sessionInit.copy(sessionToken = someToken))) ==
        Valid(MySession(SessionInit(someToken, 1, AUser))))
    }
  }

  "Changed UserId is rejected" in {
    assert(sessionRegister.session(sessionToken, Some(BUser.id)).await(99.seconds) == Invalid(Problem("Invalid session token")))
  }

  "logout" in {
    assert(sessionRegister.logout(sessionToken).await(99.seconds) == Completed)
    assert(sessionRegister.session(sessionToken, None).await(99.seconds).isInvalid)
  }
}

private object SessionRegisterTest
{
  private val AUser = SimpleUser(UserId("A"), HashedPassword.newEmpty)
  private val BUser = SimpleUser(UserId("B"), HashedPassword.newEmpty)

  final case class MySession(sessionInit: SessionInit[SimpleUser]) extends LoginSession {
    type User = SimpleUser
  }
}
