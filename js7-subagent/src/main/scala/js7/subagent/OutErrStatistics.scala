package js7.subagent

import cats.effect.{IO, Resource, ResourceIO}
import js7.base.io.process.{Stderr, Stdout, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.system.MBeanUtils.registerStaticMBean
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.ByteUnits.toKBGB
import js7.subagent.OutErrStatistics.*
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now

private final class OutErrStatistics:
  private val runningSince = now
  private var persistDuraton = ZeroDuration
  private var eventTotal = 0
  private var charTotal = 0L

  def count[A](n: Int = 1, charCount: Int)(body: IO[A]): IO[A] =
    eventTotal += n
    charTotal += charCount
    body.timed.map: (duration, a) =>
      persistDuraton += duration
      Bean.persistNanos += duration.toNanos
      Bean.eventTotal += n
      Bean.charTotal += charCount
      a

  def isRelevant: Boolean =
    persistDuraton >= RelevantDuration

  override def toString =
    s"$eventTotal OrderStdWritten events" + locally:
      if charTotal == 0 then
        ""
      else locally:
        val duration = persistDuraton
        val totalDuration = runningSince.elapsed
        val percentage = if totalDuration.isZero then 100.0 else 100 * totalDuration / totalDuration
        s" (${toKBGB(charTotal)}), blocked output ${duration.pretty} " +
          s"⏱️ ${(duration / eventTotal).pretty}/chunk ($percentage%)"
        // This is the time an unbuffered stdout/stderr pipe is blocked

private object OutErrStatistics:
  private val RelevantDuration = 1.s

  private val logger = Logger[this.type]

  /** Logs some stdout and stderr statistics. */
  def stdouterrToStatisticsResource: ResourceIO[Map[StdoutOrStderr, OutErrStatistics]] =
    for
      result <- Resource
        .make(
          acquire = IO:
            Map[StdoutOrStderr, OutErrStatistics](
              Stdout -> new OutErrStatistics,
              Stderr -> new OutErrStatistics))(
          release = outErrStatistics => IO:
            if outErrStatistics(Stdout).isRelevant || outErrStatistics(Stderr).isRelevant then
              logger.debug:
                s"stdout: ${outErrStatistics(Stdout)}, stderr: ${outErrStatistics(Stderr)}")
    yield
      result


  def registerMXBean: ResourceIO[Unit] =
    registerStaticMBean("OrderStdWrittenStatistics", Bean).map(_ => ())

  private sealed trait OrderStdWrittenStatisticsMXBean:
    this: Bean.type =>
    def getBlockedSeconds: java.math.BigDecimal = java.math.BigDecimal.valueOf(persistNanos.get, 9)
    /** Number of Java characters (16bit characters, longer characters count twice). */
    def getCharacterTotal: Long = charTotal.get
    def getEventTotal: Long = eventTotal.get

  private object Bean extends OrderStdWrittenStatisticsMXBean:
    val persistNanos = Atomic(0L)
    val eventTotal = Atomic(0L)
    val charTotal = Atomic(0L)
