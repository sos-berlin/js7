package js7.base.metering

import cats.effect.{IO, ResourceIO}
import com.typesafe.config.Config
import js7.base.configutils.Configs.{ConvertibleConfig, RichConfig}
import js7.base.convert.As
import js7.base.convert.As.StringAsPercentage
import js7.base.fs2utils.StreamExtensions.interruptWhenF
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.metering.CallMeter.*
import js7.base.service.Service
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.*
import scala.quoted.{Expr, Quotes, Type}
import sourcecode.Enclosing

// Must only be used in (static) object variables !!!
final class CallMeter(val name: String):

  private var _count = 0
  private var _nanos = 0L
  private var _sinceNano: Long = System.nanoTime()
  //private val current = Array.fill(PeriodLengths.head)(null: HistoryEntry)
  //private var currentLength = 0
  //private val history: Array[List[HistoryEntry]] = Array.fill(PeriodLengths.length - 1)(Nil)

  register(this) // Never unregister

  inline def apply[T](inline body: => T): T =
    ${ CallMeterMacros.applyMacro[T]('this, 'body) }

  private[metering] def call[A](body: => A): A =
    val t = System.nanoTime()
    try
      body
    finally
      add(t)

  private[metering] def io[A](io: IO[A]): IO[A] =
    IO.defer:
      val t = System.nanoTime()
      io.guarantee:
        IO:
          add(t)

  //private[metering] def sync[F[_], A](Fa: F[A])(using F: Sync[F]): F[A] =
  //  F.defer:
  //    val t = System.nanoTime()
  //    Fa.guarantee:
  //      F.delay:
  //        add(t)

  protected def add(startedAt: Long): Unit =
    synchronized:
      _count += 1
      _nanos += System.nanoTime() - startedAt

  def count: Int =
    _count

  def duration: FiniteDuration =
    _nanos.ns

  def quote: Double =
    _nanos.toDouble / (System.nanoTime() - _sinceNano)

  private def asStringAndReset: String =
    val (sinceNano, n, nanos) = synchronized:
      val r = (_sinceNano, _count, _nanos)
      reset()
      r
    mkString(sinceNano, n, nanos)

  def asString: String =
    val (sinceNano, n, nanos) = synchronized((_sinceNano, _count, _nanos))
    mkString(sinceNano, n, nanos)

  private def mkString(sinceNano: Long, n: Int, nanos: Long): String =
    val duration = nanos.ns
    val period = (System.nanoTime() - sinceNano).ns
    val pct = 100 * duration / period
    f"$n%9d×${(n > 0) ?? s" ${(duration / n).pretty}"}%8s ${duration.pretty}%8s/${period.pretty}%-8s $pct%6.1f%% $name"

  private def reset(): Unit =
    synchronized:
      _count = 0
      _nanos = 0
      _sinceNano = System.nanoTime()

  override def toString =
    s"CallMeter($asString)"

  //def startNewEntry(): Unit =
  //  synchronized:
  //    val t = System.currentTimeMillis()
  //    val e = HistoryEntry(
  //      startMilli = t,
  //      periodMillis = (t - _sinceNano).toInt,
  //      n = _count,
  //      nanos = _nanos)
  //    //if currentLength >= PeriodLengths.head then
  //    //  historize()
  //    current(currentLength) = e
  //    currentLength += 1
  //    _sinceNano = t
  //
  //private def historize(): Unit =
  //  HistoryEntry(
  //    startMilli = current.head.startMilli,
  //    periodMillis = current.iterator.map(_.periodMillis).sum,
  //    n = current.iterator.map(_.n).sum,
  //    nanos = current.iterator.map(_.nanos).sum)
  //
  //final case class HistoryEntry(startMilli: Long, periodMillis: Int, n: Int, nanos: Long):
  //  def periodStart: Timestamp =
  //    Timestamp.ofEpochMilli(startMilli)
  //
  //  def period: FiniteDuration =
  //    periodMillis.ms
  //
  //  def duration: FiniteDuration =
  //    nanos.ns


object CallMeter:

  private val logger = Logger[this.type]
  //private val SmallestPeriod = 1.min
  //private val PeriodLengths = Seq(
  //  5,
  //  12,
  //  24,
  //  3)
  private val _callMeters = Atomic[List[CallMeter]](Nil)

  def apply()(using enc: Enclosing): CallMeter =
    new CallMeter(name = enc.value)

  private def register(callMeter: CallMeter): Unit =
    _callMeters.getAndUpdate(callMeter :: _)

  def logAndResetAll(): Unit =
    if logger.isTraceEnabled then
      for callMeter <- _callMeters.get().view.filter(_.count > 0).toVector.sortBy(_.name) do
        logger.trace(s"${callMeter.asStringAndReset}")

  def logAndResetAbove(minimumQuote: Double): Unit =
    if logger.isTraceEnabled then
      val callMeters = _callMeters.get().view
        .filter(_.quote >= minimumQuote)
        .toVector
        .sortBy(_.name)
      for callMeter <- callMeters do
        logger.trace(s"${callMeter.asStringAndReset}")

  def loggingService(conf: Conf): ResourceIO[LoggingService] =
    Service.resource(IO(LoggingService(conf)))

  final class LoggingService private[CallMeter](conf: Conf)
  extends Service.StoppableByRequest:
    protected def start =
      startService:
        val shortPerLong = (conf.longPeriod / conf.shortPeriod).toInt max 1
        fs2.Stream.fixedRate[IO](conf.shortPeriod).zipWithIndex.evalTap: (_, i) =>
          IO:
            if (i + 1) % shortPerLong == 0 then
              logAndResetAll()
            else
              // TODO Start of period is random. Count short and period separately!
              logAndResetAbove(conf.shortMinimumQuote)
        .interruptWhenF(untilStopRequested)
        .compile.drain
        .guarantee:
          IO(logAndResetAll())

    override def toString = "CallMeter.LoggingService"


  final case class Conf(
    shortPeriod: FiniteDuration,
    shortMinimumQuote: Double,
    longPeriod: FiniteDuration)
  object Conf:
    def fromConfig(config: Config): Conf =
      Conf(
        shortPeriod =
          config.finiteDuration("js7.metering.short-period") getOrElse 1.minutes,
        shortMinimumQuote =
          config.optionAs("js7.metering.short-period-minimum-quote")(StringAsPercentage)
            .fold(0.1)(_.toDouble),
        longPeriod =
          config.finiteDuration("js7.metering.long-period") getOrElse 10.minutes)


private object CallMeterMacros:

  def applyMacro[T: Type](callMeter: Expr[CallMeter], body: Expr[T])(using Quotes): Expr[T] =
    Type.of[T] match
      case '[IO[a]] => '{
        $callMeter
          .io:
            $body.asInstanceOf[IO[a]]
          .asInstanceOf[T]
      }
      case _ => '{
        $callMeter.call($body)
      }