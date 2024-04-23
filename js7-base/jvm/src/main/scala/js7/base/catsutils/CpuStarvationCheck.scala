package js7.base.catsutils

import cats.effect.metrics.CpuStarvationWarningMetrics
import cats.effect.{IO, IOApp}
import js7.base.catsutils.CpuStarvationCheck.*
import js7.base.log.Logger.syntax.*
import js7.base.log.{LogLevel, Logger}
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.{Duration, FiniteDuration}

transparent trait CpuStarvationCheck extends IOApp:

  override def runtimeConfig =
    super.runtimeConfig.copy(
      cpuStarvationCheckInterval = cpuStarvationCheckInterval,
      cpuStarvationCheckThreshold = cpuStarvationCheckThreshold / cpuStarvationCheckInterval,
      cpuStarvationCheckInitialDelay = cpuStarvationCheckInitialDelay)

  override protected def onCpuStarvationWarn(metrics: CpuStarvationWarningMetrics): IO[Unit] =
    // See https://typelevel.org/cats-effect/docs/core/starvation-and-tuning
    val d = metrics.starvationInterval * metrics.starvationThreshold
    IO(logger.log(cpuStarvationCheckLogLevel,
      s"🐌 Responsiveness was slower than ${d.pretty}"))



object CpuStarvationCheck:
  /** Use only after Logger.initialize! */
  private lazy val logger = Logger[this.type]

  private val cpuStarvationCheckLogLevel: LogLevel =
    sys.props.get("js7.cats.effect.cpuStarvationCheckLogLevel").fold(LogLevel.Info)(LogLevel(_))

  private val cpuStarvationCheckInitialDelay: Duration =
    if cpuStarvationCheckLogLevel == LogLevel.None then
      Duration.Inf
    else
      sys.props.get("js7.cats.effect.cpuStarvationCheckInitialDelay").fold(60.s)(StringAsDuration)

  private val cpuStarvationCheckThreshold: FiniteDuration =
    sys.props.get("js7.cats.effect.cpuStarvationCheckThreshold").fold(100.ms)(StringAsDuration)

  private val cpuStarvationCheckInterval: FiniteDuration =
    sys.props.get("js7.cats.effect.cpuStarvationCheckInterval").fold(1.s)(StringAsDuration)
      .max(cpuStarvationCheckThreshold)
