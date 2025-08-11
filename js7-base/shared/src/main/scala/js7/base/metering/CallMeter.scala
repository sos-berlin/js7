package js7.base.metering

import cats.effect.{IO, SyncIO}
import java.util.concurrent.atomic.AtomicBoolean
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.metering.CallMeter.*
import js7.base.system.MBeanUtils
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.MultipleLinesBracket.Square
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.Deadline
import scala.quoted.{Expr, Quotes, Type}
import sourcecode.Enclosing

// Instances register themselves. Must only be used in (static) object variables !!!
final class CallMeter private(val name: String)
extends CallMeter.CallMeterMXBean:

  private val _sinceNano = System.nanoTime()
  // We don't synchronize _total and _nanos, to be a little faster.
  // With big numbers, the error gets small.
  // With small numbers, concurrent access should occur seldom.
  private val _running = Atomic(0)
  private val _total = Atomic(0L)
  private val _nanos = Atomic(0L)

  register(this) // Never unregister
  registerAsMBean(this) // Never unregister

  inline def apply[T](inline body: => T): T =
    ${ CallMeterMacros.applyMacro[T]('this, 'body) }

  private[metering] def meterCall[A](body: => A): A =
    _running += 1
    val t = startMetering_()
    try
      body
    finally
      stopMetering_(t)

  private[metering] def meterIO[A](io: IO[A]): IO[A] =
    IO.defer:
      val t = startMetering_()
      io.guarantee:
        IO:
          stopMetering_(t)

  //private[metering] def meterSync[F[_], A](Fa: F[A])(using F: Sync[F]): F[A] =
  //  F.defer:
  //    val t = startMetering()
  //    Fa.guarantee:
  //      F.delay:
  //        stopMetering(t)

  /** @return Metering, must be stopped with stopMetering to restore _running! */
  def startMetering(): Metering =
    _running += 1
    System.nanoTime().asInstanceOf[Metering]

  /** Must be called once and only once. */
  def stopMetering(metering: Metering): Unit =
    addNanos(System.nanoTime() - metering.asInstanceOf[Long])
    _running -= 1

  /** @return MeteringN, must be closed to restore _running! */
  def startMetering(n: Int): MeteringN =
    _running += n
    MeteringN(this, sinceNano = System.nanoTime(), n = n)

  private def stopMetering(metering: MeteringN, n: Int): Unit =
    addNanos(metering.elapsedNanos)
    _running -= n

  /** MUST be followed by stopMetering. */
  private def startMetering_(): Long =
    _running += 1
    System.nanoTime()

  /** Call only once for each metering!. */
  private def stopMetering_(sinceNano: Long): Unit =
    addNanos(System.nanoTime() - sinceNano)
    _running -= 1

  def addNanos(nanos: Long): Unit =
    addNanosInline(nanos)

  private inline def addNanosInline(inline nanos: Long): Unit =
    _total += 1
    _nanos += nanos

  def measurement(): Measurement =
    Measurement(this,
      total = total,
      running = _running.get(),
      meteredNanos = _nanos.get(),
      elapsedNanos = System.nanoTime() - _sinceNano)

  def total: Long =
    _total.get()

  override def getRunning = _running.get()
  override def getTotal = _total.get()
  override def getSeconds = _nanos.get() / 1_000_000_000.0

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

  private def registerAsMBean(callMeter: CallMeter): Unit =
    MBeanUtils.registerMBean(s"CallMeter_${callMeter.name}")(SyncIO(callMeter))
      .allocated.unsafeRunSync()  // Never unregister !!!

  private[metering] def callMeters: Vector[CallMeter] =
    _callMeters.get

  def logAll(): Unit =
    if logger.isTraceEnabled then
      _callMeters.get().iterator
        .map(_.measurement())
        .filter(_.total > 0)
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


  type Metering = Long // Nanoseconds

  /** Metering of a batch of operations.
    * <p>This is a slower variant of `Metering`. */
  final class MeteringN private[CallMeter](callMeter: CallMeter, sinceNano: Long, n: Int)
    extends AtomicBoolean(false), AutoCloseable:
    def close(): Unit =
      if compareAndSet(false, true) then
        callMeter.stopMetering(this, n)

    def elapsedNanos: Long =
      System.nanoTime() - sinceNano

    override def toString = s"$callMeter $n× ${Deadline(sinceNano.ns).elapsed}"

  object MeteringN:
    val Dummy = MeteringN(CallMeter("Dummy"), 0, 0)


  sealed trait CallMeterMXBean:
    def getRunning: Int
    def getTotal: Long
    def getSeconds: Double


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
