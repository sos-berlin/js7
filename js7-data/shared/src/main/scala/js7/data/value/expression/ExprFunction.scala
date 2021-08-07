package js7.data.value.expression

import cats.syntax.semigroup._
import io.circe.{Decoder, Encoder, Json}
import js7.base.circeutils.CirceUtils._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichString
import js7.data.value.expression.scopes.NamedValueScope
import js7.data.value.{ListValue, MissingValue, Value}
import scala.language.implicitConversions

final case class ExprFunction private(
  parameters: Seq[VariableDeclaration],
  expression: Expression)
{
  def eval(arguments: ListValue)(implicit scope: Scope): Checked[Value] =
    evalAllowMissing(arguments).flatMap {
      case MissingValue(problem) => Left(problem)
      case o => Right(o)
    }

  private def evalAllowMissing(arguments: ListValue)(implicit scope: Scope): Checked[Value] =
    if (arguments.elements.length != parameters.size)
      Left(Problem(s"Number of arguments does not match number of function parameters in: ${toString.truncateWithEllipsis(50)}"))
    else {
      val argScope = NamedValueScope(
        parameters
          .view
          .zip(arguments.elements)
          .map { case (p, a) => p.name -> a }
          .toMap)
      expression.eval(argScope |+| scope)
    }

  override def toString = parameters.mkString("(", ",", ")") + "=>" + expression
}

object ExprFunction
{
  def checked(parameters: Seq[VariableDeclaration], expression: Expression)
  : Checked[ExprFunction] =
    for (_ <- parameters.checkUniqueness) yield
      ExprFunction(parameters, expression)

  object testing {
    implicit def fromPair(pair: (String, Expression)): ExprFunction =
      ExprFunction(Seq(VariableDeclaration(pair._1)), pair._2)
  }

  implicit val jsonEncoder: Encoder[ExprFunction] = o => Json.fromString(o.toString)
  implicit val jsonDecoder: Decoder[ExprFunction] =
    c => c.as[String]
      .flatMap(o => ExpressionParser.parseFunction(o).toDecoderResult(c.history))
}

final case class VariableDeclaration(name: String/*, valueType: Option[ValueType] = None*/)
{
  override def toString = name //+ (valueType.fold("")(o => s": $o"))
}
