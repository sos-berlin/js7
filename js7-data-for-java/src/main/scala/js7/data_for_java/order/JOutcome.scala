package js7.data_for_java.order

import io.vavr.control.Either as VEither
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.OrderOutcome
import js7.data.value.Value
import js7.data_for_java.common.JJsonable
import scala.jdk.CollectionConverters.*

sealed trait JOutcome extends JJsonable[JOutcome]:
  type AsScala = OrderOutcome
  def companion: JJsonable.Companion[JOutcome] = JOutcome


object JOutcome extends JJsonable.Companion[JOutcome]:
  type AsScala = OrderOutcome

  @javaApi
  val succeeded: Succeeded =
    JOutcome.Succeeded(OrderOutcome.Succeeded.empty)

  @javaApi @Nonnull
  def succeeded(@Nonnull namedValues: java.util.Map[String, Value]): Succeeded =
    JOutcome.Succeeded(OrderOutcome.Succeeded(namedValues.asScala.toMap))

  @javaApi
  val failed: Failed =
    JOutcome.Failed(OrderOutcome.failed)

  @javaApi @Nonnull
  def failed(@Nonnull message: String): Failed =
    JOutcome.Failed(OrderOutcome.Failed(message.nonEmpty ? message))

  @javaApi @Nonnull
  def failed(@Nonnull message: String, @Nonnull namedValues: java.util.Map[String, Value]): Failed =
    JOutcome.Failed(OrderOutcome.Failed(message.nonEmpty ? message, namedValues.asScala.toMap))

  def apply(asScala: OrderOutcome): JOutcome =
    asScala match
      case asScala: OrderOutcome.Succeeded => Succeeded(asScala)
      case asScala: OrderOutcome.Caught => Caught(asScala)
      case asScala: OrderOutcome.Failed => Failed(asScala)
      case asScala: OrderOutcome.TimedOut => TimedOut(asScala)
      case asScala: OrderOutcome.Killed => Killed(asScala)
      case asScala: OrderOutcome.Disrupted => Disrupted(asScala)

  @Nonnull
  override def fromJson(@Nonnull jsonString: String): VEither[Problem, JOutcome] =
    super.fromJson(jsonString)

  sealed trait Completed extends JOutcome:
    def asScala: OrderOutcome.Completed

    @Nonnull
    final def namedValues: java.util.Map[String, Value] =
      asScala.namedValues.asJava

  sealed trait IsSuccess extends Completed:
    def asScala: OrderOutcome.IsSucceeded


  final case class Succeeded(asScala: OrderOutcome.Succeeded) extends IsSuccess
  final case class Caught(asScala: OrderOutcome.Caught) extends IsSuccess
  final case class Failed(asScala: OrderOutcome.Failed) extends Completed
  final case class TimedOut(asScala: OrderOutcome.TimedOut) extends JOutcome
  final case class Killed(asScala: OrderOutcome.Killed) extends JOutcome
  final case class Disrupted(asScala: OrderOutcome.Disrupted) extends JOutcome

  protected def jsonEncoder = OrderOutcome.jsonEncoder
  protected def jsonDecoder = OrderOutcome.jsonDecoder
