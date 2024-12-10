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
      cpuStarvationCheckInterval = cpuStarvationCheckInterval,
      cpuStarvationCheckThreshold = cpuStarvationCheckThreshold / cpuStarvationCheckInterval,
      cpuStarvationCheckInitialDelay = cpuStarvationCheckInitialDelay)

  override protected def onCpuStarvationWarn(metrics: CpuStarvationWarningMetrics): IO[Unit] =
    // See https://typelevel.org/cats-effect/docs/core/starvation-and-tuning
    val d = metrics.starvationInterval * metrics.starvationThreshold
    IO:
      if Logger.isInitialized then
        logger.log(starvingLogLevel, s"🐌 Responsiveness was longer than ${d.pretty}")


object IOAppWithCpuStarvationCheck:
  /** Use only after Logger.initialize! */
  private lazy val logger = Logger[this.type]
  private val CpuStarvationCheckThresholdDefault = 500.ms
  private val CpuStarvationCheckIntervalDefault = 1.s

  private val starvingLogLevel: LogLevel =
    sys.props.get("js7.cats.effect.cpuStarvationCheckLogLevel").fold(LogLevel.Info)(LogLevel(_))

  private val cpuStarvationCheckInitialDelay: Duration =
    if starvingLogLevel == LogLevel.None then
      Duration.Inf
    else
      sys.props.get("js7.cats.effect.cpuStarvationCheckInitialDelay").fold(60.s)(StringAsDuration)

  private val cpuStarvationCheckThreshold: FiniteDuration =
    sys.props.get("js7.cats.effect.cpuStarvationCheckThreshold")
      .fold(CpuStarvationCheckThresholdDefault)(StringAsDuration)

  private val cpuStarvationCheckInterval: FiniteDuration =
    sys.props.get("js7.cats.effect.cpuStarvationCheckInterval")
      .fold(CpuStarvationCheckIntervalDefault)(StringAsDuration)
      .max(cpuStarvationCheckThreshold)
