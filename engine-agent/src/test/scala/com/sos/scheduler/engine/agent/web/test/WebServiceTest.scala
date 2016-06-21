package com.sos.scheduler.engine.agent.web.test

import com.sos.scheduler.engine.agent.configuration.Akkas.newActorSystem
import com.sos.scheduler.engine.agent.web.auth.UnknownUserPassAuthenticator
import com.sos.scheduler.engine.common.auth.Account
import com.sos.scheduler.engine.common.scalautil.HasCloser
import org.scalatest.{BeforeAndAfterAll, Suite}
import scala.concurrent.duration._
import spray.routing.authentication.UserPassAuthenticator
import spray.testkit.ScalatestRouteTest

/**
  * @author Joacim Zschimmer
  */
trait WebServiceTest extends HasCloser with BeforeAndAfterAll with ScalatestRouteTest {
  this: Suite ⇒

  implicit val routeTestTimeout = RouteTestTimeout(5.seconds)

  protected implicit final lazy val actorRefFactory = newActorSystem(getClass.getSimpleName)(closer)

  override protected def afterAll() = closer.close()

  protected val authenticator: UserPassAuthenticator[Account] = UnknownUserPassAuthenticator
}
