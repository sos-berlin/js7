package js7.common.akkautils

import js7.base.time.ScalaTime._
import js7.base.utils.Closer.syntax._
import js7.base.utils.HasCloser
import js7.base.utils.ScalaUtils.RichJavaClass
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.akkautils.ProvideActorSystem._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.Logger
import com.typesafe.config.Config
import scala.concurrent.duration.Deadline.now

/**
  * @author Joacim Zschimmer
  */
trait ProvideActorSystem extends HasCloser
{
  protected def actorSystemName: String = getClass.simpleScalaName
  protected def config: Config

  protected final lazy val actorSystem = newActorSystem(actorSystemName, config)
    .withCloser { o =>
      if (!o.whenTerminated.isCompleted) {
        val since = now
        logger.debug(s"ActorSystem('${o.name}') terminate ...")
        o.terminate() await TerminationTimeout
        logger.debug(s"ActorSystem('${o.name}') terminated (${since.elapsed.pretty})")
      }
    }
}

object ProvideActorSystem {
  private val logger = Logger(getClass)
  private val TerminationTimeout = 60.s
}
