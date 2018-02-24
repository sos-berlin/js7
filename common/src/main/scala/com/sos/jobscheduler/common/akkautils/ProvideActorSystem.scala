package com.sos.jobscheduler.common.akkautils

import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.common.akkautils.Akkas.newActorSystem
import com.sos.jobscheduler.common.akkautils.ProvideActorSystem._
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAny
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.{HasCloser, Logger}
import com.sos.jobscheduler.common.time.ScalaTime._

/**
  * @author Joacim Zschimmer
  */
trait ProvideActorSystem extends HasCloser
{
  protected def actorSystemName: String = getClass.simpleScalaName

  protected lazy val actorSystem = newActorSystem(actorSystemName) withCloser { o ⇒
    if (!o.whenTerminated.isCompleted) {
      logger.debug(s"ActorSystem('${o.name}') terminate")
      o.terminate() await TerminationTimeout
    }
  }
}

object ProvideActorSystem {
  private val logger = Logger(getClass)
  private val TerminationTimeout = 60.s
}
