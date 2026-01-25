package js7.base.metering

import cats.effect.{IO, ResourceIO}
import com.typesafe.config.Config
import js7.base.configutils.Configs.RichConfig
import js7.base.fs2utils.StreamExtensions.interruptWhenF
import js7.base.log.LogLevel
import js7.base.metering.CallMeterLoggingService.*
import js7.base.service.Service
import scala.collection.mutable
import scala.concurrent.duration.*

final class CallMeterLoggingService private[CallMeterLoggingService](conf: Conf)
  extends Service.StoppableByRequest:

  private val lastMeasurements = mutable.Map.empty[String, Measurement]

  protected def start =
    startService:
      fs2.Stream.fixedRate[IO](conf.logEvery).evalTap: _ =>
        IO:
          logAndStartNewDiff(LogLevel.Trace)
      .interruptWhenF(untilStopRequested)
      .compile.drain
      .guarantee:
        IO(logAndStartNewDiff(LogLevel.Debug))

  /** Logs the difference to last measurement, then safes the current measurement as the last one. */
  private def logAndStartNewDiff(logLevel: LogLevel): Unit =
    CallMeter.log(logLevel): m =>
      val name = m.callMeter.name
      val diff = lastMeasurements.get(name).fold(m)(m.diff)
      lastMeasurements(name) = m
      diff

  override def toString = "CallMeterLoggingService"


object CallMeterLoggingService:

  def service(config: Config): ResourceIO[CallMeterLoggingService] =
    service(Conf.fromConfig(config))

  def service(logEvery: FiniteDuration): ResourceIO[CallMeterLoggingService] =
    service(Conf(logEvery = logEvery))

  private def service(conf: Conf): ResourceIO[CallMeterLoggingService] =
    Service.resource(IO(CallMeterLoggingService(conf)))


  final case class Conf(logEvery: FiniteDuration)

  object Conf:
    def fromConfig(config: Config): Conf =
      Conf(
        logEvery = config.finiteDuration("js7.metering.log-every") getOrElse 1.minute)
