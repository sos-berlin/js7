package  js7.data_for_java.order

import io.vavr.control.{Either => VEither}
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.order.Outcome
import js7.data.value.Value
import js7.data_for_java.common.JJsonable
import scala.jdk.CollectionConverters._

sealed trait JOutcome extends JJsonable[JOutcome]
{
  type AsScala = Outcome
  def companion = JOutcome
}

object JOutcome extends JJsonable.Companion[JOutcome]
{
  @javaApi
  val succeeded = JOutcome(Outcome.succeeded)

  @javaApi @Nonnull
  def succeeded(@Nonnull namedValues: java.util.Map[String, Value]) =
    JOutcome(Outcome.Succeeded(namedValues.asScala.toMap))

  @javaApi
  val failed = JOutcome(Outcome.failed)

  @javaApi @Nonnull
  def failed(@Nonnull namedValues: java.util.Map[String, Value]) =
    JOutcome(Outcome.Failed(namedValues.asScala.toMap))

  def apply(asScala: Outcome) = asScala match {
    case asScala: Outcome.Succeeded => new Succeeded(asScala)
    case asScala: Outcome.Failed => new Failed(asScala)
    case asScala: Outcome.Killed => new Killed(asScala)
    case asScala: Outcome.Disrupted => new Disrupted(asScala)
  }

  @Nonnull
  override def fromJson(@Nonnull jsonString: String): VEither[Problem, JOutcome] =
    super.fromJson(jsonString)

  sealed trait Completed extends JOutcome {
    def asScala: Outcome.Completed

    @Nonnull
    final def namedValues: java.util.Map[String, Value] = asScala.namedValues.asJava
  }

  final case class Succeeded(asScala: Outcome.Succeeded) extends Completed
  final case class Failed(asScala: Outcome.Failed) extends Completed
  final case class Killed(asScala: Outcome.Killed) extends JOutcome
  final case class Disrupted(asScala: Outcome.Disrupted) extends JOutcome

  protected def jsonEncoder = Outcome.jsonEncoder
  protected def jsonDecoder = Outcome.jsonDecoder
}
