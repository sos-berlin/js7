package js7.common.akkahttp.web.session

import akka.http.scaladsl.testkit.ScalatestRouteTest
import js7.base.auth.{HashedPassword, SessionToken, SimpleUser, UserId}
import js7.base.generic.{Completed, SecretString}
import js7.base.problem.Checked.Ops
import js7.base.problem.Problems.InvalidSessionTokenProblem
import js7.base.time.ScalaTime._
import js7.common.akkahttp.web.session.SessionRegisterTest._
import js7.common.akkautils.Akkas
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.scalautil.MonixUtils.syntax._
import monix.execution.Scheduler.Implicits.global
import monix.execution.schedulers.TestScheduler
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class SessionRegisterTest extends AnyFreeSpec with ScalatestRouteTest
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
    assert(sessionRegister.session(unknownSessionToken, Right(Anonymous)).await(99.seconds).isLeft)
    assert(sessionRegister.session(unknownSessionToken, Right(AUser)).await(99.seconds).isLeft)
  }

  "login anonymously" in {
    sessionToken = sessionRegister.login(Anonymous).await(99.seconds)
  }

  "login and update User" in {
    val originalSession = sessionRegister.session(sessionToken, Right(Anonymous)).await(99.seconds).orThrow
    val updatedSession = sessionRegister.session(sessionToken, Right(AUser)).await(99.seconds).orThrow
    assert(updatedSession.sessionToken == sessionToken)
    assert(updatedSession.sessionInit == originalSession.sessionInit)
    assert(updatedSession.currentUser == AUser)
  }

  "Session use does not match HTTPS distinguished name's UserIds" in {
    assert(sessionRegister.session(sessionToken, Left(Set(AUser.id))).await(99.seconds).map(_.sessionToken) == Right(sessionToken))
    assert(sessionRegister.session(sessionToken, Left(Set(BUser.id))).await(99.seconds).map(_.sessionToken) == Left(InvalidSessionTokenProblem))
  }

  "Changed UserId is rejected" in {
    assert(sessionRegister.session(sessionToken, Right(BUser)).await(99.seconds) == Left(InvalidSessionTokenProblem))
  }

  "session returns always the same Session" in {
    val session = sessionRegister.session(sessionToken, Right(Anonymous)).await(99.seconds).orThrow
    assert(session eq sessionRegister.session(sessionToken, Right(AUser)).await(99.seconds).orThrow)
    assert(session eq sessionRegister.session(sessionToken, Left(Set(AUser.id))).await(99.seconds).orThrow)
  }

  "But late authentication is allowed, changing from anonymous to non-anonymous User" in {
    val mySystem = newActorSystem("SessionRegisterTest")
    val mySessionRegister = SessionRegister.start[MySession](mySystem, MySession.apply, SessionRegister.TestConfig)(testScheduler)
    val sessionToken = mySessionRegister.login(SimpleUser.TestAnonymous).await(99.seconds)

    mySessionRegister.session(sessionToken, Right(Anonymous)).runSyncUnsafe(99.seconds).orThrow
    assert(mySessionRegister.session(sessionToken, Right(Anonymous)).runSyncUnsafe(99.seconds).toOption.get.currentUser == SimpleUser.TestAnonymous)

    // Late authentication: change session's user from SimpleUser.Anonymous to AUser
    assert(mySessionRegister.session(sessionToken, Right(AUser)).await(99.seconds) == Right(MySession(SessionInit(1, sessionToken, loginUser = SimpleUser.TestAnonymous))))
    assert(mySessionRegister.session(sessionToken, Right(Anonymous)).runSyncUnsafe(99.seconds).toOption.get.currentUser == AUser/*changed*/)

    assert(mySessionRegister.session(sessionToken, Right(AUser)).await(99.seconds) == Right(MySession(SessionInit(1, sessionToken, loginUser = SimpleUser.TestAnonymous))))
    assert(mySessionRegister.session(sessionToken, Right(BUser)).await(99.seconds) == Left(InvalidSessionTokenProblem))
    assert(mySessionRegister.session(sessionToken, Right(Anonymous)).runSyncUnsafe(99.seconds).toOption.get.currentUser == AUser)

    Akkas.terminateAndWait(mySystem, 10.s)
  }

  "logout" in {
    assert(sessionRegister.logout(sessionToken).await(99.seconds) == Completed)
    assert(sessionRegister.session(sessionToken, Right(Anonymous)).await(99.seconds).isLeft)
  }

  "Session timeout" in {
    assert(sessionRegister.count.await(99.seconds) == 0)

    sessionToken = sessionRegister.login(AUser).await(99.seconds)
    val eternal = sessionRegister.login(BUser, isEternalSession = true).await(99.seconds)
    assert(sessionRegister.count.await(99.seconds) == 2)
    assert(sessionRegister.session(sessionToken, Right(Anonymous)).await(99.seconds).isRight)

    testScheduler.tick(TestSessionTimeout + 1.s)
    assert(sessionRegister.session(sessionToken, Right(Anonymous)).await(99.seconds).isLeft)
    assert(sessionRegister.session(eternal, Right(Anonymous)).await(99.seconds).isRight)
    assert(sessionRegister.count.await(99.seconds) == 1)
  }
}

private object SessionRegisterTest
{
  private val TestSessionTimeout = 1.hour
  private val Anonymous = SimpleUser(UserId.Anonymous, HashedPassword.newEmpty())
  private val AUser = SimpleUser(UserId("A"), HashedPassword.newEmpty())
  private val BUser = SimpleUser(UserId("B"), HashedPassword.newEmpty())

  final case class MySession(sessionInit: SessionInit[SimpleUser]) extends Session {
    type User = SimpleUser
  }
}
