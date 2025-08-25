package js7.base.metering

import cats.effect.{IO, ResourceIO}
import com.typesafe.config.Config
import js7.base.configutils.Configs.RichConfig
import js7.base.fs2utils.StreamExtensions.interruptWhenF
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.metering.CallMeterLoggingService.*
import js7.base.service.Service
import js7.base.utils.MultipleLinesBracket.Square
import js7.base.utils.ScalaUtils.syntax.foreachWithBracket
import scala.collection.mutable
import scala.concurrent.duration.*

final class CallMeterLoggingService private[CallMeterLoggingService](conf: Conf)
  extends Service.StoppableByRequest:

  private val lastMeasurements = mutable.Map.empty[String, Measurement]

  protected def start =
    startService:
      fs2.Stream.fixedRate[IO](conf.logEvery).evalTap: _ =>
        IO:
          logAndStartNewDiff()
      .interruptWhenF(untilStopRequested)
      .compile.drain
      .guarantee:
        IO(logAndStartNewDiff())

  /** Logs the difference to last measurement, then safes the current measurement as the last one. */
  private def logAndStartNewDiff(): Unit =
    if logger.isTraceEnabled then
      CallMeter.callMeters.view.filter(_.total > 0).map: callMeter =>
        val m = callMeter.measurement()
        val diff = lastMeasurements.get(callMeter.name).fold(m)(m.diff)
        lastMeasurements(callMeter.name) = m
        diff
      .filter(_.total > 0)
      .map(_.asString)
      .toVector // Compute first, then log quickly:
      .foreachWithBracket(Square): (measurementString, bracket) =>
        logger.trace(s"$bracket $measurementString")

  override def toString = "CallMeterLoggingService"


object CallMeterLoggingService:

  private val logger = Logger[this.type]

  def resource(config: Config): ResourceIO[CallMeterLoggingService] =
    resource(Conf.fromConfig(config))

  def resource(logEvery: FiniteDuration): ResourceIO[CallMeterLoggingService] =
    resource(Conf(logEvery = logEvery))

  def resource(conf: Conf): ResourceIO[CallMeterLoggingService] =
    Service.resource(IO(CallMeterLoggingService(conf)))

  final case class Conf(
    logEvery: FiniteDuration)

  object Conf:
    def fromConfig(config: Config): Conf =
      Conf(
        logEvery =
          config.finiteDuration("js7.metering.log-every") getOrElse 1.minute)
