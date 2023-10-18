package js7.data.value.expression

import cats.syntax.semigroup.*
import io.circe.{Decoder, Encoder, Json}
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichString
import js7.data.value.Value
import js7.data.value.expression.ExpressionParser.parseFunction
import js7.data.value.expression.scopes.NamedValueScope
import org.jetbrains.annotations.TestOnly
import scala.language.implicitConversions

final case class ExprFunction(
  parameters: Seq[VariableDeclaration],
  expression: Expression)
  (maybeName: Option[String] = None,
    minimumArgumentCount: Int = 0,
    maximumArgumentCount: Option[Int] = None):

  private def name = maybeName getOrElse toString.truncateWithEllipsis(50)

  def restrict(name: String, minimum: Int, maximum: Int): Checked[ExprFunction] =
    if parameters.size < minimum || parameters.size > maximum then
      Left(Problem(
        if minimum == maximum then
          s"The '$name' function is expected to accept exactly $minimum parameters"
        else
          s"The '$name' function is expected to accept between $minimum and $maximum parameters"))
    else
      Right(copy()(
        maybeName = Some(name),
        minimumArgumentCount = minimum,
        maximumArgumentCount = Some(maximum)))

  def eval(arguments: Iterable[Value])(implicit scope: Scope): Checked[Value] =
    if arguments.sizeIs < minimumArgumentCount
      || maximumArgumentCount.exists(arguments.sizeIs > _) then
      Left(Problem(
        s"Number of arguments=${arguments.size} does not match " +
          "required number of function parameters=" +
          minimumArgumentCount +
          maximumArgumentCount.filter(_ != minimumArgumentCount).fold("")("..." + _) +
          s" in '$name' function"))
    else
      val argScope = NamedValueScope(
        parameters
          .view
          .zip(arguments)
          .map { case (p, a) => p.name -> a }
          .toMap)
      expression.eval(argScope |+| scope)

  override def toString = parameters.mkString("(", ",", ")") + "=>" + expression


object ExprFunction:
  def apply(
    parameters: Seq[VariableDeclaration],
    expression: Expression)
  =
    new ExprFunction(parameters, expression)()

  def checked(parameters: Seq[VariableDeclaration], expression: Expression)
  : Checked[ExprFunction] =
    for _ <- parameters.checkUniqueness yield
      new ExprFunction(parameters, expression)()

  @TestOnly
  object testing:
    implicit def fromPair(pair: (String, Expression)): ExprFunction =
      ExprFunction(Seq(VariableDeclaration(pair._1)), pair._2)

    implicit final class FunctionExprSyntax(private val name: String) extends AnyVal:
      def |=>(expr: Expression): ExprFunction =
        ExprFunction(Seq(VariableDeclaration(name)), expr)

  implicit val jsonEncoder: Encoder[ExprFunction] = o => Json.fromString(o.toString)
  implicit val jsonDecoder: Decoder[ExprFunction] =
    c => c.as[String]
      .flatMap(o => parseFunction(o).toDecoderResult(c.history))

final case class VariableDeclaration(name: String/*, valueType: Option[ValueType] = None*/):
  override def toString = name //+ (valueType.fold("")(o => s": $o"))
