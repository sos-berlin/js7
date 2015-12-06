package com.sos.scheduler.engine.common.akkautils

import akka.actor.ActorSystem.Settings
import akka.actor.Cancellable
import akka.util.{ByteString, Timeout}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._

/**
 * @author Joacim Zschimmer
 */
object Akkas {

  /**
   * 8 months, assuming the Akka confinguration default akka.scheduler.tick-duration = 10.ms
   */
  val MaximumTimeout = tickDurationToMaximumTimeout(tickMillis = 10)

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
}
