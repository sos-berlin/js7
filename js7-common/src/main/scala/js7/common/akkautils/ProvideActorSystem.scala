package js7.common.akkautils

import com.typesafe.config.Config
import js7.base.time.ScalaTime._
import js7.base.utils.Closer.syntax._
import js7.base.utils.HasCloser
import js7.base.utils.ScalaUtils.syntax._
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.akkautils.ProvideActorSystem._

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
        Akkas.terminateAndWait(o, TerminationTimeout)
      }
    }
}

object ProvideActorSystem
{
  private val TerminationTimeout = 60.s
}
