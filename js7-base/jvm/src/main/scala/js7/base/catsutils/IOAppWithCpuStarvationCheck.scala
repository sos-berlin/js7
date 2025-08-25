package js7.base.catsutils

import cats.effect.metrics.CpuStarvationWarningMetrics
import cats.effect.unsafe.IORuntimeConfig
import cats.effect.{IO, IOApp}
import js7.base.catsutils.IOAppWithCpuStarvationCheck.*
import js7.base.log.Logger.syntax.*
import js7.base.log.{LogLevel, Logger}
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.{Duration, FiniteDuration}

private transparent trait IOAppWithCpuStarvationCheck extends IOApp:

  override def runtimeConfig: IORuntimeConfig =
    super.runtimeConfig.copy(
      cpuStarvationCheckInterval = interval,
      cpuStarvationCheckThreshold = threshold / interval,
      cpuStarvationCheckInitialDelay = initialDelay)

  override protected def onCpuStarvationWarn(metrics: CpuStarvationWarningMetrics): IO[Unit] =
    // See https://typelevel.org/cats-effect/docs/core/starvation-and-tuning
    val d = metrics.starvationInterval * metrics.starvationThreshold
    IO:
      if Logger.isInitialized then
        logger.log(starvingLogLevel, s"🐌 Responsiveness was longer than ${d.pretty}")


object IOAppWithCpuStarvationCheck:
  /** Use only after Logger.initialize! */
  private lazy val logger = Logger[this.type]
  private val ThresholdDefault = 3.s
  private val IntervalDefault = 6.s
  private val InitialDelayDefault = 60.s

  private val starvingLogLevel: LogLevel =
    sys.props.get("js7.cats.effect.cpuStarvationCheckLogLevel").fold(LogLevel.Info)(LogLevel(_))

  private val initialDelay: Duration =
    if starvingLogLevel == LogLevel.None then
      Duration.Inf
    else
      sys.props.get("js7.cats.effect.cpuStarvationCheckInitialDelay")
        .fold(InitialDelayDefault)(StringAsDuration)

  private val threshold: FiniteDuration =
    sys.props.get("js7.cats.effect.cpuStarvationCheckThreshold")
      .fold(ThresholdDefault)(StringAsDuration)

  private val interval: FiniteDuration =
    sys.props.get("js7.cats.effect.cpuStarvationCheckInterval")
      .fold(IntervalDefault)(StringAsDuration)
      .max(threshold)
