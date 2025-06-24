package js7.base.metering

import cats.effect.IO
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.metering.CallMeter.*
import js7.base.utils.Atomic
import js7.base.utils.MultipleLinesBracket.Square
import js7.base.utils.ScalaUtils.syntax.*
import scala.quoted.{Expr, Quotes, Type}
import sourcecode.Enclosing

// Instances register themselves. Must only be used in (static) object variables !!!
final class CallMeter private(val name: String):

  private val _sinceNano = System.nanoTime()
  // We don't synchronize _count and _nanos, to be a little faster.
  // With big numbers, the error gets small.
  // With small numbers, concurrent access should occur seldom.
  private val _count = Atomic(0L)
  private val _nanos = Atomic(0L)

  register(this) // Never unregister

  inline def apply[T](inline body: => T): T =
    ${ CallMeterMacros.applyMacro[T]('this, 'body) }

  private[metering] def meterCall[A](body: => A): A =
    val t = System.nanoTime()
    try
      body
    finally
      addNanosInline(System.nanoTime() - t)

  private[metering] def meterIO[A](io: IO[A]): IO[A] =
    IO.defer:
      val t = System.nanoTime()
      io.guarantee:
        IO:
          addNanos(System.nanoTime() - t)

  //private[metering] def meterSync[F[_], A](Fa: F[A])(using F: Sync[F]): F[A] =
  //  F.defer:
  //    val t = System.nanoTime()
  //    Fa.guarantee:
  //      F.delay:
  //        addNanos(t)

  //def addMeasurement(startedAt: Deadline): Unit =
  //  addNanos(startedAt.elapsed.toNanos)

  def addNanos(nanos: Long): Unit =
    addNanosInline(nanos)

  private inline def addNanosInline(inline nanos: Long): Unit =
    _count.getAndIncrement()
    _nanos.getAndAdd(nanos)

  def measurement(): Measurement =
    Measurement(this,
      elapsedNanos = System.nanoTime() - _sinceNano,
      count = count,
      countedNanos = _nanos.get())

  def count: Long =
    _count.get()

  override def toString = s"CallMeter(${measurement().asString})"


object CallMeter:

  private lazy val logger = Logger[this.type]
  private val _callMeters = Atomic[Vector[CallMeter]](Vector.empty)

  given Ordering[CallMeter] = Ordering.by(_.name)

  def apply()(using enc: Enclosing): CallMeter =
    new CallMeter(name = enc.value)

  def apply(name: String): CallMeter =
    new CallMeter(name)

  private def register(callMeter: CallMeter): Unit =
    _callMeters.getAndUpdate(_.insertOrdered(callMeter))

  private[metering] def callMeters: Vector[CallMeter] =
    _callMeters.get

  def logAll(): Unit =
    if logger.isTraceEnabled then
      _callMeters.get().iterator
        .map(_.measurement())
        .filter(_.count > 0)
        .foreachWithBracket(Square): (callMeter, bracket) =>
          logger.trace(s"$bracket ${callMeter.asString}")

  private def logAndResetAbove(minimumQuote: Double): Unit =
    if logger.isTraceEnabled then
      val callMeters = _callMeters.get().view.map(_.measurement())
        .filter(_.quote >= minimumQuote)
        .toVector
        .sortBy(_.name)
      for callMeter <- callMeters do
        logger.trace(s"${callMeter.asString}")


private object CallMeterMacros:

  def applyMacro[T: Type](callMeter: Expr[CallMeter], body: Expr[T])(using Quotes): Expr[T] =
    Type.of[T] match
      case '[IO[a]] => '{
        $callMeter.meterIO:
          $body.asInstanceOf[IO[a]]
        .asInstanceOf[T]
      }
      case _ => '{
        $callMeter.meterCall:
          $body
      }
