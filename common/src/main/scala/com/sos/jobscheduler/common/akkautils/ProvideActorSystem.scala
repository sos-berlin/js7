package com.sos.jobscheduler.common.akkautils

import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.common.akkautils.Akkas.newActorSystem
import com.sos.jobscheduler.common.akkautils.ProvideActorSystem._
import com.sos.jobscheduler.common.scalautil.Closer.ops._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.{HasCloser, Logger}
import com.sos.jobscheduler.common.time.ScalaTime._
import com.typesafe.config.Config

/**
  * @author Joacim Zschimmer
  */
trait ProvideActorSystem extends HasCloser
{
  protected def actorSystemName: String = getClass.simpleScalaName
  protected def config: Config

  protected lazy val actorSystem = newActorSystem(actorSystemName, config) withCloser { o =>
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
