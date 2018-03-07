package com.sos.jobscheduler.tests

import com.sos.jobscheduler.common.akkautils.Akkas
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._

/**
  * @author Joacim Zschimmer
  */
final class SimpleAkkaHttpClient(label: String) extends AkkaHttpClient
{
  protected val actorSystem = Akkas.newActorSystem(label)

  protected implicit def executionContext = actorSystem.dispatcher

  protected def userAndPassword = None

  override def close() = {
    super.close()
    actorSystem.terminate() await 99.s
  }
}
