package com.sos.scheduler.engine.common.akkautils

import akka.actor.ActorSystem.Settings
import akka.actor.Cancellable
import akka.util.{ByteString, Timeout}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import spray.http.Uri

/**
 * @author Joacim Zschimmer
 */
object Akkas {

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
    tickDurationToMaximumTimeout(tickMillis = settings.config.getDuration("akka.scheduler.tick-duration", TimeUnit.MILLISECONDS))

  def tickDurationToMaximumTimeout(tickMillis: Long): Timeout = {
    // 68 years, maximum for scheduler.tick-duration = 1s, 8 months when tick-duration = 10ms
    Timeout(1000L * Int.MaxValue / (1000 / tickMillis) - 2000, MILLISECONDS)
  }

  def byteStringToTruncatedString(byteString: ByteString, size: Int = 100, name: String = "ByteString") =
    s"${byteString.size} bytes " + (byteString take size map { c ⇒ f"$c%02x" } mkString " ") + (if (byteString.size > size) " ..." else "")

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

  private val ValidSymbols = "%" + """-_.*$+:@&=,!~';""" // See ActorRef.ValidSymbols (private)
  private val toHex = "0123456789ABCDEF"

  private def isValidChar(c: Char): Boolean =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || (ValidSymbols.indexOf(c) != -1)

  private def encodeAsActorName2(string: String): String = {
    val sb = new StringBuilder(string.length + 10*3)
    for (c ← string) {
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

  def decodeActorName(o: String): String =
    Uri.Path(o).head.toString
}
