package com.sos.scheduler.engine.agent.web.test

import com.sos.scheduler.engine.agent.configuration.Akkas.newActorSystem
import com.sos.scheduler.engine.common.scalautil.HasCloser
import org.scalatest.{BeforeAndAfterAll, Suite}
import spray.testkit.ScalatestRouteTest

/**
  * @author Joacim Zschimmer
  */
trait WebServiceTest extends HasCloser with BeforeAndAfterAll with ScalatestRouteTest {
  this: Suite ⇒

  protected implicit final lazy val actorRefFactory = newActorSystem(getClass.getSimpleName)(closer)

  override protected def afterAll() = closer.close()
}
