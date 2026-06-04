package js7.base.time

import cats.syntax.traverse.*
import com.typesafe.config.Config
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import js7.base.configutils.Configs.RichConfig
import js7.base.problem.Checked
import js7.base.time.ScalaTime.RichDuration
import js7.base.time.Throttle.*
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

trait Throttle:

  protected type Self <: Throttle

  def isRecorder: Boolean =
    true

  final def tryRecord(time: FiniteDuration, weight: Double): Either[TooFast, Self] =
    tryRecord(Record(time, weight))

  def tryRecord(record: Record): Either[TooFast, Self]

  final def record(time: FiniteDuration, weight: Double): Self =
    record(Record(time, weight))

  def record(record: Record): Self


object Throttle:

  def fromConfig(config: Config): Checked[Throttle] =
    val unit = SpeedUnit("AddOrder", "AddOrders")
    if !config.hasPath("js7.instruction.addOrder.throttle") then
      Right(Unlimited)
    else
      config.getConfigList("js7.instruction.addOrder.throttle").asScala.toSeq.traverse: cnf =>
        for
          limit = cnf.getDouble("limit")
          period <- cnf.finiteDuration("period")
        yield
          Speed(limit, period, unit)
      .map: speeds =>
        StandardThrottle(speeds, unit = unit)

  type Zero = Zero.type

  object Zero extends Throttle:
    protected type Self = Zero

    private val tooFast: TooFast =
      val period = 24.days * 365
      TooFast(
        throttle = Speed(0, period),
        delay = period /*could be infinite*/)

    override def isRecorder = false // This Throttle does not record

    def tryRecord(record: Record): Either[TooFast, Zero] =
      if record.weight > 0 then
        Left(tooFast)
      else
        Right(this)

    def record(record: Record): Zero =
      this


  type Unlimited = Unlimited.type

  object Unlimited extends Throttle:
    protected type Self = Unlimited

    override def isRecorder = false // This Throttle does not record

    def tryRecord(record: Record): Right[Nothing, Unlimited] =
      Right(this)

    def record(record: Record): Unlimited =
      this


  final case class Record(time: FiniteDuration, weight: Double):
    override def toString = s"Throttle.Record(${time.show}, $weight)"

  object Record:
    given Codec[Record] = deriveCodec[Record]


  /**
    * @param throttle the breached speed limit
    * @param delay duration until the speed no longer is breached (assuming weight is a tiny amount)
    */
  final case class TooFast(throttle: Speed, delay: FiniteDuration):
    override def toString = s"TooFast($throttle delay=${delay.show})"
