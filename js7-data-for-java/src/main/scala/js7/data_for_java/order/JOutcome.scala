package js7.data_for_java.order

import io.vavr.control.Either as VEither
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.Outcome
import js7.data.value.Value
import js7.data_for_java.common.JJsonable
import scala.jdk.CollectionConverters.*

sealed trait JOutcome extends JJsonable[JOutcome]
{
  type AsScala = Outcome
  def companion = JOutcome
}

object JOutcome extends JJsonable.Companion[JOutcome]
{
  type AsScala = Outcome

  @javaApi
  val succeeded: Succeeded =
    JOutcome.Succeeded(Outcome.Succeeded.empty)

  @javaApi @Nonnull
  def succeeded(@Nonnull namedValues: java.util.Map[String, Value]) =
    JOutcome.Succeeded(Outcome.Succeeded(namedValues.asScala.toMap))

  @javaApi
  val failed: Failed =
    JOutcome.Failed(Outcome.failed)

  @javaApi @Nonnull
  def failed(@Nonnull message: String) =
    JOutcome.Failed(Outcome.Failed(message.nonEmpty ? message))

  @javaApi @Nonnull
  def failed(@Nonnull message: String, @Nonnull namedValues: java.util.Map[String, Value]) =
    JOutcome.Failed(Outcome.Failed(message.nonEmpty ? message, namedValues.asScala.toMap))

  def apply(asScala: Outcome): JOutcome =
    asScala match {
      case asScala: Outcome.Succeeded => Succeeded(asScala)
      case asScala: Outcome.Failed => Failed(asScala)
      case asScala: Outcome.TimedOut => TimedOut(asScala)
      case asScala: Outcome.Killed => Killed(asScala)
      case asScala: Outcome.Disrupted => Disrupted(asScala)
    }

  @Nonnull
  override def fromJson(@Nonnull jsonString: String): VEither[Problem, JOutcome] =
    super.fromJson(jsonString)

  sealed trait Completed extends JOutcome {
    def asScala: Outcome.Completed

    @Nonnull
    final def namedValues: java.util.Map[String, Value] =
      asScala.namedValues.asJava
  }

  final case class Succeeded(asScala: Outcome.Succeeded) extends Completed
  final case class Failed(asScala: Outcome.Failed) extends Completed
  final case class TimedOut(asScala: Outcome.TimedOut) extends JOutcome
  final case class Killed(asScala: Outcome.Killed) extends JOutcome
  final case class Disrupted(asScala: Outcome.Disrupted) extends JOutcome

  protected def jsonEncoder = Outcome.jsonEncoder
  protected def jsonDecoder = Outcome.jsonDecoder
}
