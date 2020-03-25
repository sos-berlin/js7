package com.sos.jobscheduler.common.akkautils

import akka.actor.ActorSystem.Settings
import akka.actor.{ActorContext, ActorPath, ActorSystem, Cancellable, ChildActorPath, RootActorPath}
import akka.http.scaladsl.model.Uri
import akka.util.{ByteString, Timeout}
import cats.effect.Resource
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.configutils.Configs
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.JavaResource
import com.typesafe.config.{Config, ConfigFactory}
import monix.eval.Task
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

/**
 * @author Joacim Zschimmer
 */
object Akkas
{
  private val logger = Logger(getClass)
  private val ConfigResource = JavaResource(getClass.getClassLoader, "com/sos/jobscheduler/common/akkautils/akka.conf")

  def newActorSystem(name: String, config: Config = ConfigFactory.empty) = {
    logger.debug(s"new ActorSystem('$name')")
    val myConfig = ConfigFactory.systemProperties
      .withFallback(config)
      .withFallback(Configs.loadResource(ConfigResource, internal = true))
      .resolve
    ActorSystem(name, myConfig, getClass.getClassLoader)
  }

  /**
   * Returns the a Timeout accepted for HTTP request, dependent on Akkas configuration akka.scheduler.tick-duration.
   * <ul>
   * <li>68 years for akka.scheduler.tick-duration = 1s</li>
   * <li>8 month for akka.scheduler.tick-duration = 10ms (the default)</li>
   * <li>24 days for akka.scheduler.tick-duration = 1ms</li>
   * </ul>
   *
   * @see https://github.com/typesafehub/akka-contrib-extra/issues/21
   * @param settings = actorSystem.settings
   * @return A Timeout which Akka http accepts
   */
  def maximumTimeout(settings: Settings) =
    tickDurationToMaximumTimeout(tickMillis = settings.config.getDuration("akka.scheduler.tick-duration", MILLISECONDS))

  def tickDurationToMaximumTimeout(tickMillis: Long): Timeout = {
    // 68 years, maximum for scheduler.tick-duration = 1s, 8 months when tick-duration = 10ms
    Timeout(1000L * Int.MaxValue / (1000 / tickMillis) - 2000, MILLISECONDS)
  }

  def byteStringToTruncatedString(byteString: ByteString, size: Int = 100, name: String = "ByteString") =
    s"${byteString.size} bytes " + (byteString take size map { c => f"$c%02x" } mkString " ") + (if (byteString.size > size) " ..." else "")

  final class DummyCancellable extends Cancellable {
    private var _isCancelled = false

    def cancel() = {
      if (_isCancelled)
        false
      else {
        _isCancelled = true
        true
      }
    }

    def isCancelled = _isCancelled
  }

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
        case RootActorPath(address, name) => address + name
        case child: ChildActorPath => child.parent.pretty.stripSuffix("/") + "/" + decodeActorName(child.name)
      }
  }

  def actorSystemResource(name: String, config: Config = ConfigFactory.empty): Resource[Task, ActorSystem] =
    Resource.make(
      acquire = Task { newActorSystem(name, config) }
    )(release =
      actorSystem =>
        Task.deferFutureAction { implicit s =>
          logger.debug(s"ActorSystem('$name') terminate")
          val since = now
          actorSystem.terminate()
            .map { _ =>
              logger.debug(s"ActorSystem('$name') terminated (${since.elapsed.pretty})")
              ()
            }
        })
}
