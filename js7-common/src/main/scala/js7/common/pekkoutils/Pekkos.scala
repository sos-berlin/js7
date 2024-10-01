package js7.common.pekkoutils

import cats.effect.{IO, Resource, ResourceIO, Sync}
import com.typesafe.config.{Config, ConfigException, ConfigFactory}
import js7.base.catsutils.CatsEffectExtensions.fromFutureDummyCancelable
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.time.JavaTimeConverters.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.configuration.Js7Configuration
import org.apache.pekko.actor.{ActorContext, ActorPath, ActorRef, ActorRefFactory, ActorSystem, ChildActorPath, Props, RootActorPath, Terminated}
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.Uri
import org.apache.pekko.util.ByteString
import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
object Pekkos:
  private val logger = Logger[this.type]

  def newActorSystem(
    name: String,
    config: Config = ConfigFactory.empty,
    executionContext: ExecutionContext)
  : ActorSystem =
    logger.debugCall("newActorSystem", name):
      val myConfig = ConfigFactory.systemProperties
        .withFallback(config)
        .withFallback(Js7Configuration.defaultConfig)
        .resolve

      ActorSystem(
        name,
        Some(myConfig),
        Some(getClass.getClassLoader),
        myConfig.getBoolean("js7.pekko.use-js7-thread-pool") ? executionContext)

  def terminateAndWait(actorSystem: ActorSystem): Unit =
    terminateAndWait(actorSystem,
      actorSystem.settings.config.getDuration("js7.pekko.shutdown-timeout").toFiniteDuration)

  def terminateAndWait(actorSystem: ActorSystem, timeout: FiniteDuration): Unit =
    logger.debugCall(s"ActorSystem:${actorSystem.name} terminateAndWait"):
      try terminateFuture(actorSystem).await(timeout)
      catch case NonFatal(t) =>
        logger.warn(s"ActorSystem:${actorSystem.name} .terminate(): ${t.toStringWithCauses}")

  def terminate(actorSystem: ActorSystem): IO[Unit] =
    logger.debugIO(s"ActorSystem:${actorSystem.name} terminate"):
      IO.fromFutureDummyCancelable(IO:
        terminateFuture(actorSystem)
      ).void

  /** Shut down connection pool and terminate ActorSystem.
    * Only once callable.
    */
  private def terminateFuture(actorSystem: ActorSystem): Future[Terminated] =
    if actorSystem.whenTerminated.isCompleted then
      actorSystem.whenTerminated
    else
      import actorSystem.dispatcher  // The ExecutionContext may be shut down here !!!
      val poolShutdownTimeout =
        try actorSystem.settings.config.getDuration("js7.pekko.http.connection-pool-shutdown-timeout").toFiniteDuration
        catch { case _: ConfigException.Missing => 100.ms }
      val timeoutPromise = Promise[Unit]()

      val timer = actorSystem.scheduler.scheduleOnce(poolShutdownTimeout):
        timeoutPromise.success(())
      Future.firstCompletedOf(Seq(
        shutDownHttpConnectionPools(actorSystem),  // May block a long time (>99s)
        timeoutPromise.future)
      ).flatMap { _ =>
        if timeoutPromise.isCompleted then
          logger.debug(s"ActorSystem:${actorSystem.name
          } shutdownAllConnectionPools() ⏰ timed out after ${poolShutdownTimeout.pretty}")
        timer.cancel()
        actorSystem.terminate()
      }

  def shutDownHttpConnectionPools(actorSystem: ActorSystem): Future[Unit] =
    if actorSystem.hasExtension(Http) then
      logger.debug(s"ActorSystem:${actorSystem.name} shutdownAllConnectionPools()")
      Http(actorSystem).shutdownAllConnectionPools()
    else
      Future.successful(())

  def byteStringToTruncatedString(byteString: ByteString, size: Int = 100): String =
    s"${byteString.size} bytes " + (byteString.take(size).map(c => f"$c%02x") mkString " ") + ((byteString.sizeIs > size) ?? " ...")

  def encodeAsActorName(o: String): String =
    val a = Uri.Path.Segment(o, Uri.Path.Empty).toString
    encodeAsActorName2(
      if a startsWith "$" then "%24" + a.tail
      else a)

  private val ValidSymbols = "%" + """-_.*$+:@&=,!~';""" // See ActorPath.ValidSymbols (private)
  private val toHex = "0123456789ABCDEF"

  private def isValidChar(c: Char): Boolean =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || (ValidSymbols.indexOf(c) != -1)

  private def encodeAsActorName2(string: String): String =
    val sb = new StringBuilder(string.length + 10*3)
    for c <- string do
      if isValidChar(c) then
        sb += c
      else
        if c >= 0x80 then
          sb += '%'
          sb += toHex(c.toInt >> 12)
          sb += toHex((c.toInt >> 8) & 0x0f)
        sb += '%'
        sb += toHex((c.toInt >> 4) & 0x0f)
        sb += toHex(c.toInt & 0x0f)
    sb.toString

  /** When an actor name to be re-used, the previous actor may still terminate, occupying the name. */
  def uniqueActorName(name: String)(implicit context: ActorContext): String =
    var _name = name
    if context.child(name).isDefined then
      _name = Iterator.from(2).map(i => s"$name~$i").find { nam => context.child(nam).isEmpty }.get
      logger.debug(s"Duplicate actor name. Replacement actor name is ${context.self.path.pretty}/$name")
    _name

  def decodeActorName(o: String): String =
    Uri.Path(o).head.toString

  implicit final class RichActorPath(private val underlying: ActorPath) extends AnyVal:
    /** Returns a non-unique readable string. "/" and "%2F" are both returns as "/". */
    def pretty: String =
      underlying match
        case RootActorPath(address, name) => address.toString + name
        case child: ChildActorPath => child.parent.pretty.stripSuffix("/") + "/" + decodeActorName(child.name)

  def actorSystemResource(name: String, config: Config = ConfigFactory.empty)
  : ResourceIO[ActorSystem] =
    for
      ec <- Resource.eval(IO.executionContext)
      r <- actorSystemResource1(name, config, ec)
    yield r

  private def actorSystemResource1(name: String, config: Config, ec: ExecutionContext)
  : ResourceIO[ActorSystem] =
    Resource.make(
      acquire = IO(newActorSystem(name, config, ec)))(
      release = terminate)

  def actorResource[F[_]](props: Props, name: String)
    (implicit arf: ActorRefFactory, F: Sync[F])
  : Resource[F, ActorRef] =
    Resource.make(
      acquire = F.delay(arf.actorOf(props, name)))(
      release = a => F.delay(arf.stop(a)))
