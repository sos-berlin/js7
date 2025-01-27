package js7.data_for_java.value

import io.vavr.control.Either as VEither
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.value.expression.ExpressionParser.parseFunction
import js7.data.value.expression.{ExprFunction, ExpressionParser}
import js7.data_for_java.common.JJsonable
import js7.data_for_java.vavr.VavrConverters.*
import scala.jdk.CollectionConverters.*

final case class JExprFunction(asScala: ExprFunction) extends JJsonable[JExprFunction]:

  type AsScala = ExprFunction

  protected def companion = JExprFunction


object JExprFunction extends JJsonable.Companion[JExprFunction]:
  type AsScala = ExprFunction

  val knownSymbols: java.util.Set[String] =
    ExpressionParser.knownSymbols.asJava

  @Nonnull
  @throws[RuntimeException]
  def apply(expression: String): JExprFunction =
    parseFunction(expression)
      .map(JExprFunction(_))
      .orThrow

  @Nonnull
  def parse(expression: String): VEither[Problem, JExprFunction] =
    parseFunction(expression)
      .map(JExprFunction(_))
      .toVavr

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JExprFunction] =
    super.fromJson(jsonString)

  protected def jsonEncoder = ExprFunction.jsonEncoder
  protected def jsonDecoder = ExprFunction.jsonDecoder
