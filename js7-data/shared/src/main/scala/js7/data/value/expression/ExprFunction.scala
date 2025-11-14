package js7.data.value.expression

import io.circe.{Decoder, Encoder, Json}
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichString
import js7.data.value.Value
import js7.data.value.expression.ExpressionParser.parseFunction
import js7.data.value.expression.scopes.NamedValueScope
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable.ArraySeq
import scala.language.implicitConversions

final case class ExprFunction(
  parameters: Seq[VariableDeclaration],
  expression: Expression,
  maybeName: Option[String] = None,
  minimumArgumentCount: Int = 0,
  maximumArgumentCount: Option[Int] = None):

  private val parameterNames = parameters.map(_.name).to(ArraySeq)

  override def equals(o: Any) =
    o match
      case o: ExprFunction =>
        parameters == o.parameters &&
          expression == o.expression
          //? maybeName == o.maybeName &&
          //? minimumArgumentCount == o.minimumArgumentCount &&
          //? maximumArgumentCount == o.maximumArgumentCount
      case _ => false

  private def name = maybeName getOrElse toString.truncateWithEllipsis(50)

  def restrict(name: String, minimum: Int, maximum: Int): Checked[ExprFunction] =
    if parameters.size < minimum || parameters.size > maximum then
      Left(Problem(
        if minimum == maximum then
          s"The '$name' function is expected to accept exactly $minimum parameters"
        else
          s"The '$name' function is expected to accept between $minimum and $maximum parameters"))
    else
      Right(copy(
        maybeName = Some(name),
        minimumArgumentCount = minimum,
        maximumArgumentCount = Some(maximum)))

  inline def eval(arguments: Value*)(using scope: Scope): Checked[Value] =
    eval(arguments: Iterable[Value])(using scope)

  def eval(arguments: Iterable[Value])(using scope: Scope): Checked[Value] =
    if arguments.sizeIs < minimumArgumentCount
      || maximumArgumentCount.exists(arguments.sizeIs > _)
    then
      Left(Problem:
        s"Number of arguments=${arguments.size} does not match " +
          "required number of function parameters=" +
          minimumArgumentCount +
          maximumArgumentCount.filter(_ != minimumArgumentCount).fold("")("..." + _) +
          s" in '$name' function")
    else
      val argScope = NamedValueScope.simple:
        parameterNames.zip(arguments).toMap
      expression.eval(using argScope |+| scope)

  override def toString =
    parameters.match
      case Seq(VariableDeclaration(name)) => name
      case _ => parameters.mkString("(", ",", ")")
    + " => " + expression


object ExprFunction:
  def apply(parameterNames: String*)(expression: Expression): ExprFunction =
    new ExprFunction(parameterNames.map(VariableDeclaration(_)), expression)

  def checked(parameters: Seq[VariableDeclaration], expression: Expression)
  : Checked[ExprFunction] =
    for _ <- parameters.checkUniqueness yield
      new ExprFunction(parameters, expression)

  @TestOnly
  object testing:
    implicit def fromPair(pair: (String, Expression)): ExprFunction =
      new ExprFunction(Seq(VariableDeclaration(pair._1)), pair._2)

    extension (name: String)
      def |=>(expr: Expression): ExprFunction =
        new ExprFunction(Seq(VariableDeclaration(name)), expr)

  implicit val jsonEncoder: Encoder[ExprFunction] = o => Json.fromString(o.toString)
  implicit val jsonDecoder: Decoder[ExprFunction] =
    c => c.as[String]
      .flatMap(o => parseFunction(o).toDecoderResult(c.history))

final case class VariableDeclaration(name: String/*, valueType: Option[ValueType] = None*/):
  override def toString = name //+ (valueType.fold("")(o => s": $o"))
