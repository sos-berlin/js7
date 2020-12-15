package js7.proxy.javaapi.data.problem

import io.vavr.control.{Either => VEither}
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.problem.{Problem, ProblemCode}
import js7.proxy.javaapi.data.common.JJsonable
import scala.jdk.OptionConverters._

final case class JProblem(asScala: Problem)
extends JJsonable[JProblem]
{
  protected type AsScala = Problem
  def companion = JProblem

  @Nonnull
  def maybeCode: Optional[ProblemCode] =
    asScala.maybeCode.toJava

  @Nonnull
  def argumentAsString(@Nonnull key: String): Optional[String] =
    asScala match {
      case problem: Problem.HasCode => problem.arguments.get(key).toJava
      case _ => Optional.empty()
    }
}

object JProblem extends JJsonable.Companion[JProblem]
{
  override def fromJson(jsonString: String): VEither[Problem, JProblem] =
    super.fromJson(jsonString)

  protected def jsonEncoder = Problem.jsonEncoder
  protected def jsonDecoder = Problem.jsonDecoder
}
