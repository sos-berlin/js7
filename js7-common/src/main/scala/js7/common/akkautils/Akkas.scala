package js7.common.akkautils

import akka.actor.{ActorContext, ActorPath, ActorSystem, ChildActorPath, RootActorPath, Terminated}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri
import akka.util.ByteString
import cats.effect.Resource
import com.typesafe.config.{Config, ConfigException, ConfigFactory}
import js7.base.log.Logger
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.time.JavaTimeConverters._
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.common.configuration.JobSchedulerConfiguration
import monix.eval.Task
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
object Akkas
{
  private val logger = Logger(getClass)

  def newActorSystem(
    name: String,
    config: Config = ConfigFactory.empty,
    defaultExecutionContext: ExecutionContext = ExecutionContext.global)
  = {
    logger.debug(s"new ActorSystem('$name')")
    val myConfig = ConfigFactory.systemProperties
      .withFallback(config)
      .withFallback(JobSchedulerConfiguration.defaultConfig)
      .resolve
    ActorSystem(
      name,
      Some(myConfig),
      Some(getClass.getClassLoader),
      myConfig.getBoolean("js7.akka.use-js7-thread-pool") ? defaultExecutionContext)
  }

  def terminateAndWait(actorSystem: ActorSystem): Unit =
    terminateAndWait(actorSystem,
      actorSystem.settings.config.getDuration("js7.akka.shutdown-timeout").toFiniteDuration)

  def terminateAndWait(actorSystem: ActorSystem, timeout: FiniteDuration): Unit = {
    logger.debug(s"ActorSystem('${actorSystem.name}') terminate ...")
    try {
      val since = now
      terminate(actorSystem).await(timeout)
      logger.debug(s"ActorSystem('${actorSystem.name}') terminated (${since.elapsed.pretty})")
    } catch {
      case NonFatal(t) =>
        logger.warn(s"ActorSystem('${actorSystem.name}').terminate(): ${t.toStringWithCauses}")
    }
  }

  /** Shut down connection pool and terminate ActorSystem.
    * Only once callable.
    */
  def terminate(actorSystem: ActorSystem): Future[Terminated] = {
    import actorSystem.dispatcher  // The ExecutionContext will be shut down here !!!
    val poolShutdownTimeout =
      try actorSystem.settings.config.getDuration("js7.akka.http.connection-pool-shutdown-timeout").toFiniteDuration
      catch { case _: ConfigException.Missing => 100.ms }
    val timeoutPromise = Promise[Unit]()
    val timer = actorSystem.scheduler.scheduleOnce(poolShutdownTimeout) {
      timeoutPromise.success(())
    }
    Future.firstCompletedOf(Seq(
      shutDownHttpConnectionPools(actorSystem),  // May block a long time (>99s)
      timeoutPromise.future)
    ).flatMap { _ =>
      if (timeoutPromise.isCompleted) {
        logger.debug(s"ActorSystem('${actorSystem.name}') shutdownAllConnectionPools() timed out after ${poolShutdownTimeout.pretty}")
      }
      timer.cancel()
      actorSystem.terminate()
    }
  }

  def shutDownHttpConnectionPools(actorSystem: ActorSystem): Future[Unit] =
    if (actorSystem.hasExtension(Http)) {
      logger.debug(s"ActorSystem('${actorSystem.name}') shutdownAllConnectionPools()")
      Http(actorSystem).shutdownAllConnectionPools()
    } else
      Future.successful(())

  def byteStringToTruncatedString(byteString: ByteString, size: Int = 100) =
    s"${byteString.size} bytes " + (byteString.take(size).map(c => f"$c%02x") mkString " ") + ((byteString.sizeIs > size) ?? " ...")

  def encodeAsActorName(o: String): String = {
    val a = Uri.Path.Segment(o, Uri.Path.Empty).toString
    encodeAsActorName2(
      if (a startsWith "$") "%24" + a.tail
      else a)
  }

  private val ValidSymbols = "%" + """-_.*$+:@&=,!~';""" // See ActorPath.ValidSymbols (private)
  private val toHex = "0123456789ABCDEF"

  private def isValidChar(c: Char): Boolean =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || (ValidSymbols.indexOf(c) != -1)

  private def encodeAsActorName2(string: String): String = {
    val sb = new StringBuilder(string.length + 10*3)
    for (c <- string) {
      if (isValidChar(c)) {
        sb += c
      } else {
        if (c >= 0x80) {
          sb += '%'
          sb += toHex(c.toInt >> 12)
          sb += toHex((c.toInt >> 8) & 0x0f)
        }
        sb += '%'
        sb += toHex((c.toInt >> 4) & 0x0f)
        sb += toHex(c.toInt & 0x0f)
      }
    }
    sb.toString
  }

  /** When an actor name to be re-used, the previous actor may still terminate, occupying the name. */
  def uniqueActorName(name: String)(implicit context: ActorContext): String = {
    var _name = name
    if (context.child(name).isDefined) {
      _name = Iterator.from(2).map(i => s"$name~$i").find { nam => context.child(nam).isEmpty }.get
      logger.debug(s"Duplicate actor name. Replacement actor name is ${context.self.path.pretty}/$name")
    }
    _name
  }

  def decodeActorName(o: String): String =
    Uri.Path(o).head.toString

  implicit final class RichActorPath(private val underlying: ActorPath) extends AnyVal
  {
    /** Returns a non-unique readable string. "/" and "%2F" are both returns as "/". */
    def pretty: String =
      underlying match {
        case RootActorPath(address, name) => address.toString + name
        case child: ChildActorPath => child.parent.pretty.stripSuffix("/") + "/" + decodeActorName(child.name)
      }
  }

  def actorSystemResource(name: String, config: Config = ConfigFactory.empty)
  : Resource[Task, ActorSystem] =
    Resource.make(
      acquire = Task.deferAction(scheduler => Task(newActorSystem(name, config, scheduler))))(
      release = actorSystem =>
        Task.deferFutureAction { implicit s =>
          logger.debug(s"ActorSystem('$name') terminate ...")
          val since = now
          terminate(actorSystem)
            .map { _ =>
              logger.debug(s"ActorSystem('$name') terminated (${since.elapsed.pretty})")
              ()
            }
        })
}
