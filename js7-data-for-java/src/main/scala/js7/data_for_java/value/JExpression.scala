package js7.data_for_java.value

import io.vavr.control.Either as VEither
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.value.expression.Expression
import js7.data.value.expression.Expression.{BooleanConstant, ListExpression, NumericConstant, ObjectExpression, StringConstant}
import js7.data.value.expression.FastparseExpressionParser.parseExpression
import js7.data_for_java.common.JJsonable
import js7.data_for_java.vavr.VavrConverters.*
import scala.jdk.CollectionConverters.*

final case class JExpression(asScala: Expression) extends JJsonable[JExpression]
{
  protected type AsScala = Expression

  protected def companion = JExpression
}

object JExpression extends JJsonable.Companion[JExpression]
{
  @Nonnull
  def parse(expression: String): VEither[Problem, JExpression] =
    parseExpression(expression)
      .map(JExpression(_))
      .toVavr

  /** Returns the string as a StringConstant Expression. */
  @Nonnull
  def fromString(@Nonnull string: String): JExpression =
    JExpression(StringConstant(string))

  /** Returns the string as a NumericConstant Expression. */
  @Nonnull
  def fromNumber(@Nonnull number: BigDecimal): JExpression =
    JExpression(NumericConstant(number))

  /** Returns the string as a BooleanConstant Expression. */
  @Nonnull
  def fromBoolean(@Nonnull boolean: Boolean): JExpression =
    JExpression(BooleanConstant(boolean))

  /** Returns the string as a ListExpression. */
  @Nonnull
  def fromIterable(@Nonnull values: java.lang.Iterable[JExpression]): JExpression =
    JExpression(ListExpression(values.asScala.view.map(_.asScala).toList))

  /** Returns the string as an ObjectExpression. */
  @Nonnull
  def fromMap(@Nonnull keyToExpr: java.util.Map[String, JExpression]): JExpression =
    JExpression(ObjectExpression(keyToExpr.asScala.view.mapValues(_.asScala).toMap))

  /** Quotes the string, usable to build a expression. */
  @Nonnull
  def quoteString(@Nonnull string: String): String =
    StringConstant.quote(string)

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JExpression] =
    super.fromJson(jsonString)

  protected def jsonEncoder = Expression.jsonEncoder
  protected def jsonDecoder = Expression.jsonDecoder
}
