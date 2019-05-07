package com.sos.jobscheduler.tests

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.common.akkautils.Akkas.newActorSystem
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.base.time.ScalaTime._

/**
  * @author Joacim Zschimmer
  */
final class SimpleAkkaHttpClient(
  label: String,
  protected val baseUri: Uri,
  protected val uriPrefixPath: String) extends AkkaHttpClient
{
  protected val actorSystem = newActorSystem(label)

  protected def userAndPassword = None

  protected def sessionToken = None

  override def close() = {
    super.close()
    actorSystem.terminate() await 99.s
  }
}
