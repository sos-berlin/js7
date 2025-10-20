package js7.base.metering

import cats.effect.{IO, Ref, ResourceIO}
import com.typesafe.config.Config
import js7.base.configutils.Configs.{ConvertibleConfig, RichConfig}
import js7.base.log.Logger.syntax.*
import js7.base.log.{LogLevel, Logger}
import js7.base.metering.Responsivenessmeter.*
import js7.base.problem.Checked
import js7.base.problem.Checked.catchNonFatal
import js7.base.service.Service
import js7.base.system.MBeanUtils
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import scala.annotation.threadUnsafe
import scala.concurrent.duration.*
import scala.math.Ordering.Implicits.infixOrderingOps

final class Responsivenessmeter private(conf: Responsivenessmeter.Conf)
extends Service.StoppableByCancel:

  import conf.{initialDelay, logLevel, meterInterval, slowThreshold}

  private val callback = Ref.unsafe[IO, MeterCallback](meterCallbackDefault)
  private var lastDelay = ZeroDuration
  private var stickUntil = Deadline.now
  private var isSlow = false

  val bean: ResponsivenessMXBean =
    new ResponsivenessMXBean:
      /** Maximum delay since the last read, not longer than stickDuration ago. */
      def getDelaySeconds =
        stickUntil = Deadline.now
        lastDelay.toDoubleSeconds

  /** Because Journal is not available in js7-base, we use a callback. */
  def onMetered(callback: MeterCallback): IO[Unit] =
    this.callback.set(callback)

  protected def start =
    startService:
      IO.sleep(initialDelay) *>
        MBeanUtils.registerMBean[IO]("InternalResponsiveness", bean).surround:
          meter.foreverM

  private val meter =
    for
      start <- IO.monotonic
      _ <- IO.sleep(meterInterval)
      end <- IO.monotonic
      _ <-
        val delay = end - (start + meterInterval)
        if Deadline.now < stickUntil then // bean not read and value not expired?
          if lastDelay < delay then
            lastDelay = delay // maximum
            stickUntil = Deadline.now + stickDuration
        else
          lastDelay = delay
          stickUntil = Deadline.now + stickDuration
        onMetered(delay)
    yield ()

  private def onMetered(delay: FiniteDuration): IO[Unit] =
    IO.defer:
      val wasSlow = isSlow
      isSlow = slowThreshold < delay
      IO.whenA(isSlow | wasSlow):
        callback.get.flatMap(_(delay, isSlow, conf))

  @threadUnsafe lazy
  val meterCallbackDefault: MeterCallback = (delay, slow, conf) =>
    IO:
      if slow then
        logger.log(logLevel, s"🐌 Internal response time was ${delay.pretty}")
      else
        logger.log(logLevel min LogLevel.Info, s"✔ InternalResponseTime was ${delay.pretty}")


object Responsivenessmeter:
  private val logger = Logger[this.type]
  private val stickDuration = 1.minute

  def service(config: Config): ResourceIO[Responsivenessmeter] =
    service(Conf.fromConfig(config).orThrow)

  def service(conf: Conf): ResourceIO[Responsivenessmeter] =
    for
      service <- Service(Responsivenessmeter(conf))
    yield
      service


  final case class Conf(
    initialDelay: FiniteDuration,
    meterInterval: FiniteDuration,
    slowThreshold: FiniteDuration,
    logLevel: LogLevel,
    emitEvents: Boolean,
    eventInterval: Option[FiniteDuration])

  object Conf:
    def fromConfig(config: Config): Checked[Conf] =
      for
        initialDelay <- config.finiteDuration("js7.responsive.initialDelay")
        meterInterval <- config.finiteDuration("js7.responsive.meter-interval")
        threshold <- config.finiteDuration("js7.responsive.slow-threshold")
        logLevel <- catchNonFatal(config.as[LogLevel]("js7.responsive.log-level"))
        emitEvents <- catchNonFatal(config.getBoolean("js7.responsive.emit-events"))
        eventInterval <- config.maybeFiniteDuration("js7.responsive.event-interval")
      yield
        Conf(
          initialDelay = initialDelay,
          meterInterval = meterInterval,
          slowThreshold = threshold,
          logLevel = logLevel,
          emitEvents = emitEvents,
          eventInterval = eventInterval)


  type MeterCallback = (delay: FiniteDuration, tooLong: Boolean, conf: Conf) => IO[Unit]


  sealed trait ResponsivenessMXBean:
    def getDelaySeconds: Double
