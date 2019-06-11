package com.sos.jobscheduler.common.akkahttp.web.session

import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.auth.{HashedPassword, SessionToken, SimpleUser, UserId}
import com.sos.jobscheduler.base.generic.{Completed, SecretString}
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problems.InvalidSessionTokenProblem
import com.sos.jobscheduler.common.akkahttp.web.session.SessionRegisterTest._
import com.sos.jobscheduler.common.akkautils.Akkas.newActorSystem
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import monix.execution.Scheduler.Implicits.global
import monix.execution.schedulers.TestScheduler
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class SessionRegisterTest extends FreeSpec with ScalatestRouteTest
{
  override def executor = super.executor  // Not implicit

  private val testScheduler = TestScheduler()
  private val unknownSessionToken = SessionToken(SecretString("UNKNOWN"))
  private lazy val sessionRegister = SessionRegister.start[MySession](system, MySession.apply, SessionRegister.TestConfig)(testScheduler)
  private var sessionToken = SessionToken(SecretString("INVALID"))

  "Logout unknown SessionToken" in {
    assert(sessionRegister.logout(unknownSessionToken).await(99.seconds) == Completed)
  }

  "session(unknown)" in {
    assert(sessionRegister.session(unknownSessionToken, None).await(99.seconds).isInvalid)
    assert(sessionRegister.session(unknownSessionToken, Some(AUser)).await(99.seconds).isInvalid)
  }

  "login" in {
    sessionToken = sessionRegister.login(AUser).await(99.seconds)
  }

  "session" in {
    val someToken = SessionToken(SecretString("X"))
    for (user <- List(Some(AUser), None)) {
      assert(sessionRegister.session(sessionToken, user).await(99.seconds).map(o => o.copy(sessionInit = o.sessionInit.copy(sessionToken = someToken))) ==
        Valid(MySession(SessionInit(1, someToken, AUser))))
    }
  }

  "Changed UserId is rejected" in {
    assert(sessionRegister.session(sessionToken, Some(BUser)).await(99.seconds) == Invalid(InvalidSessionTokenProblem))
  }

  "But late authentication is allowed, changing from anonymous to non-anonymous User" in {
    val mySystem = newActorSystem("SessionRegisterTest")
    val mySessionRegister = SessionRegister.start[MySession](mySystem, MySession.apply, SessionRegister.TestConfig)(testScheduler)
    val sessionToken = mySessionRegister.login(SimpleUser.Anonymous).await(99.seconds)

    mySessionRegister.session(sessionToken, None).runSyncUnsafe(99.seconds).orThrow
    assert(mySessionRegister.session(sessionToken, None).runSyncUnsafe(99.seconds).toOption.get.currentUser == SimpleUser.Anonymous)

    // Late authentication: change session's user from SimpleUser.Anonymous to AUser
    assert(mySessionRegister.session(sessionToken, Some(AUser)).await(99.seconds) == Valid(MySession(SessionInit(1, sessionToken, loginUser = SimpleUser.Anonymous))))
    assert(mySessionRegister.session(sessionToken, None).runSyncUnsafe(99.seconds).toOption.get.currentUser == AUser/*changed*/)

    assert(mySessionRegister.session(sessionToken, Some(AUser)).await(99.seconds) == Valid(MySession(SessionInit(1, sessionToken, loginUser = SimpleUser.Anonymous))))
    assert(mySessionRegister.session(sessionToken, Some(BUser)).await(99.seconds) == Invalid(InvalidSessionTokenProblem))
    assert(mySessionRegister.session(sessionToken, None).runSyncUnsafe(99.seconds).toOption.get.currentUser == AUser)

    mySystem.terminate() await 99.seconds
  }

  "logout" in {
    assert(sessionRegister.logout(sessionToken).await(99.seconds) == Completed)
    assert(sessionRegister.session(sessionToken, None).await(99.seconds).isInvalid)
  }

  "Session timeout" in {
    assert(sessionRegister.count.await(99.seconds) == 0)

    sessionToken = sessionRegister.login(AUser).await(99.seconds)
    val eternal = sessionRegister.login(BUser, isEternalSession = true).await(99.seconds)
    assert(sessionRegister.count.await(99.seconds) == 2)
    assert(sessionRegister.session(sessionToken, None).await(99.seconds).isValid)

    testScheduler.tick(TestSessionTimeout + 1.second)
    assert(sessionRegister.session(sessionToken, None).await(99.seconds).isInvalid)
    assert(sessionRegister.session(eternal, None).await(99.seconds).isValid)
    assert(sessionRegister.count.await(99.seconds) == 1)
  }
}

private object SessionRegisterTest
{
  private val TestSessionTimeout = 1.hour
  private val AUser = SimpleUser(UserId("A"), HashedPassword.newEmpty)
  private val BUser = SimpleUser(UserId("B"), HashedPassword.newEmpty)

  final case class MySession(sessionInit: SessionInit[SimpleUser]) extends Session {
    type User = SimpleUser
  }
}
