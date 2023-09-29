package js7.common.akkahttp.web.session

import akka.http.scaladsl.testkit.ScalatestRouteTest
import js7.base.Js7Version
import js7.base.auth.{HashedPassword, SessionToken, SimpleUser, UserId}
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.generic.{Completed, SecretString}
import js7.base.problem.Checked.Ops
import js7.base.problem.Problems.InvalidSessionTokenProblem
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.version.Version
import js7.common.akkahttp.web.session.RouteProviderTest.MySession
import js7.common.akkahttp.web.session.SessionRegisterTest.*
import js7.common.akkautils.Akkas
import js7.common.akkautils.Akkas.newActorSystem
import monix.execution.Scheduler.Implicits.traced
import monix.execution.schedulers.TestScheduler
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class SessionRegisterTest extends OurTestSuite with ScalatestRouteTest:
  override def testConfig = config"akka.loglevel = warning"
    .withFallback(super.testConfig)

  override def executor = super.executor  // Not implicit

  private val testScheduler = TestScheduler()
  private val unknownSessionToken = SessionToken(SecretString("UNKNOWN"))
  private lazy val sessionRegister =
    implicit val scheduler = testScheduler
    SessionRegister.forTest(system, MySession.apply, SessionRegister.TestConfig)
  private var sessionToken = SessionToken(SecretString("INVALID"))

  "Logout unknown SessionToken" in:
    assert(sessionRegister.logout(unknownSessionToken).await(99.s) == Completed)

  "session(unknown)" in:
    assert(sessionRegister.session(unknownSessionToken, Right(Anonymous)).await(99.s).isLeft)
    assert(sessionRegister.session(unknownSessionToken, Right(AUser)).await(99.s).isLeft)

  "login anonymously" in:
    sessionToken = sessionRegister.login(Anonymous, Some(Js7Version)).await(99.s).orThrow

  "login and update User" in:
    val originalSession = sessionRegister.session(sessionToken, Right(Anonymous)).await(99.s).orThrow
    val updatedSession = sessionRegister.session(sessionToken, Right(AUser)).await(99.s).orThrow
    assert(updatedSession.sessionToken == sessionToken)
    assert(updatedSession.sessionInit == originalSession.sessionInit)
    assert(updatedSession.currentUser == AUser)

  "Session use does not match HTTPS distinguished name's UserIds" in:
    assert(sessionRegister.session(sessionToken, Left(Set(AUser.id))).await(99.s).map(_.sessionToken) == Right(sessionToken))
    assert(sessionRegister.session(sessionToken, Left(Set(BUser.id))).await(99.s).map(_.sessionToken) == Left(InvalidSessionTokenProblem))

  "Changed UserId is rejected" in:
    assert(sessionRegister.session(sessionToken, Right(BUser)).await(99.s) == Left(InvalidSessionTokenProblem))

  "session returns always the same Session" in:
    val session = sessionRegister.session(sessionToken, Right(Anonymous)).await(99.s).orThrow
    assert(session eq sessionRegister.session(sessionToken, Right(AUser)).await(99.s).orThrow)
    assert(session eq sessionRegister.session(sessionToken, Left(Set(AUser.id))).await(99.s).orThrow)

  "But late authentication is allowed, changing from anonymous to non-anonymous User" in:
    val mySystem = newActorSystem("SessionRegisterTest")
    val mySessionRegister =
      implicit val scheduler = testScheduler
      SessionRegister.forTest(mySystem, MySession.apply, SessionRegister.TestConfig)
    val sessionToken = mySessionRegister.login(SimpleUser.TestAnonymous, Some(Js7Version))
      .await(99.s).orThrow

    mySessionRegister.session(sessionToken, Right(Anonymous)).runSyncUnsafe(99.s).orThrow
    assert(mySessionRegister.session(sessionToken, Right(Anonymous)).runSyncUnsafe(99.s).toOption.get.currentUser == SimpleUser.TestAnonymous)

    // Late authentication: change session's user from SimpleUser.Anonymous to AUser
    assert(mySessionRegister.session(sessionToken, Right(AUser)).await(99.s) == Right(MySession(SessionInit(sessionToken, loginUser = SimpleUser.TestAnonymous))))
    assert(mySessionRegister.session(sessionToken, Right(Anonymous)).runSyncUnsafe(99.s).toOption.get.currentUser == AUser/*changed*/)

    assert(mySessionRegister.session(sessionToken, Right(AUser)).await(99.s) == Right(MySession(SessionInit(sessionToken, loginUser = SimpleUser.TestAnonymous))))
    assert(mySessionRegister.session(sessionToken, Right(BUser)).await(99.s) == Left(InvalidSessionTokenProblem))
    assert(mySessionRegister.session(sessionToken, Right(Anonymous)).runSyncUnsafe(99.s).toOption.get.currentUser == AUser)

    Akkas.terminateAndWait(mySystem, 10.s)

  "logout" in:
    assert(sessionRegister.logout(sessionToken).await(99.s) == Completed)
    assert(sessionRegister.session(sessionToken, Right(Anonymous)).await(99.s).isLeft)

  "Session timeout" in:
    assert(sessionRegister.count.await(99.s) == 0)

    sessionToken = sessionRegister.login(AUser, Some(Js7Version)).await(99.s).orThrow
    val eternal = sessionRegister.login(BUser, Some(Js7Version), isEternalSession = true)
      .await(99.s).orThrow
    assert(sessionRegister.count.await(99.s) == 2)
    assert(sessionRegister.session(sessionToken, Right(Anonymous)).await(99.s).isRight)

    testScheduler.tick(TestSessionTimeout + 1.s)
    assert(sessionRegister.session(sessionToken, Right(Anonymous)).await(99.s).isLeft)
    assert(sessionRegister.session(eternal, Right(Anonymous)).await(99.s).isRight)
    assert(sessionRegister.count.await(99.s) == 1)

  "Non-matching version are still not checked" in:
    assert(sessionRegister.login(AUser, Some(Version("2.2.0"))).await(99.s).isRight)
      //Left(Problem(s"Client's version 2.2.0 does not match JS7 TEST version $Js7Version")))

private object SessionRegisterTest:
  private val TestSessionTimeout = 1.hour
  private val Anonymous = SimpleUser(UserId.Anonymous, HashedPassword.newEmpty())
  private val AUser = SimpleUser(UserId("A"), HashedPassword.newEmpty())
  private val BUser = SimpleUser(UserId("B"), HashedPassword.newEmpty())

  final case class MySession(sessionInit: SessionInit) extends Session
