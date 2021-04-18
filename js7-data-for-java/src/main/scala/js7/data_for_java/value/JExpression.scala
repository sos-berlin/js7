package js7.data_for_java.value

import io.vavr.control.{Either => VEither}
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.value.expression.{Expression, ExpressionParser}
import js7.data_for_java.common.JJsonable
import js7.data_for_java.vavr.VavrConverters._

final case class JExpression(asScala: Expression) extends JJsonable[JExpression]
{
  protected type AsScala = Expression

  protected def companion = JExpression
}

object JExpression extends JJsonable.Companion[JExpression]
{
  @Nonnull
  def parse(expression: String): VEither[Problem, JExpression] =
    ExpressionParser
      .parse(expression)
      .map(JExpression(_))
      .toVavr

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JExpression] =
    super.fromJson(jsonString)

  protected def jsonEncoder = Expression.jsonEncoder
  protected def jsonDecoder = Expression.jsonDecoder
}
