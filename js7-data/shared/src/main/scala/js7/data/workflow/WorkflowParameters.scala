package js7.data.workflow

import cats.instances.vector._
import cats.syntax.semigroup._
import cats.syntax.traverse._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, JsonObject}
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.RightUnit
import js7.base.utils.ScalaUtils.syntax._
import js7.data.value.{BooleanValue, NamedValues, NumberValue, StringValue, Value, ValueType}
import js7.data.workflow.WorkflowParameters._

final case class WorkflowParameters private(nameToParameter: Map[String, WorkflowParameter])
{
  def checkNamedValues(namedValues: NamedValues): Checked[Unit] =
  {
    val checkedAllExpected = (namedValues.keySet -- nameToParameter.values.map(_.name).toSet)
      .map[Problem](name => UnexpectedOrderArgumentProblem(name))
      .reduceOption(_ |+| _)
      .toLeft(())

    val checked = nameToParameter.values
      .map(param =>
        (namedValues.get(param.name), param.default) match {
          case (None, None) => Left(MissingOrderArgumentProblem(param): Problem)
          case (None, Some(_/*default*/)) => RightUnit
          case (Some(value), _) =>
            if (value.valueType != param.valueType)
              Left(WrongOrderArgumentTypeProblem(param, value.valueType))
            else
              RightUnit
        })
      .reduceLeftEither

    checkedAllExpected.combineLeft(checked).rightAs(())
  }

  def defaultArgument(name: String): Option[Value] =
    nameToParameter.get(name)
      .flatMap(_.default)

  lazy val defaultArguments: NamedValues =
    nameToParameter.values.view
      .collect { case WorkflowParameter(name, _, Some(default)) => name -> default }
      .toMap
}

object WorkflowParameters
{
  private val SupportedTypes: Set[ValueType] = Set(StringValue, BooleanValue, NumberValue)

  def apply(parameters: WorkflowParameter*): WorkflowParameters =
    apply(parameters)

  def apply(parameters: Iterable[WorkflowParameter]): WorkflowParameters =
    checked(parameters).orThrow

  def checked(parameters: Iterable[WorkflowParameter]): Checked[WorkflowParameters] =
    parameters.toCheckedKeyedMap(_.name)
      .flatMap(nameToParam =>
        nameToParam.values
          .map(p =>
            if (!SupportedTypes(p.valueType))
              Left(Problem(s"Unsupported type of parameter '${p.name}': ${p.valueType}"))
            else
              RightUnit)
          .reduceLeftEither
          .map(_ => new WorkflowParameters(nameToParam)))

  implicit val jsonEncoder: Encoder.AsObject[WorkflowParameters] =
    o => JsonObject.fromIterable(
      o.nameToParameter.values.map(param =>
        param.name -> Json.fromJsonObject(JsonObject(
          "type" -> param.valueType.asJson,
          "default" -> param.default.asJson))))

  implicit val jsonDecoder: Decoder[WorkflowParameters] =
    cursor =>
      (cursor.value.asObject match {
        case None => Left(Problem("WorkflowParameters expected")).toDecoderResult(cursor.history)
        case Some(obj) => obj
          .toVector
          .traverse { case (name, json) =>
            val c = json.hcursor
            for {
              typ <- c.get[ValueType]("type")
              default <- c.get[Option[Value]]("default")
              w <- WorkflowParameter.checked(name, typ, default).toDecoderResult(c.history)
            } yield w
          }
          .flatMap(w => WorkflowParameters.checked(w).toDecoderResult(cursor.history))
      })

  final case class MissingOrderArgumentProblem(parameter: WorkflowParameter) extends Problem.Coded {
    def arguments = Map(
      "name" -> parameter.name,
      "type" -> parameter.valueType.name)
  }

  final case class UnexpectedOrderArgumentProblem(name: String) extends Problem.Coded {
    def arguments = Map(
      "name" -> name)
  }

  final case class WrongOrderArgumentTypeProblem(parameter: WorkflowParameter, argumentType: ValueType)
  extends Problem.Coded {
    def arguments = Map(
      "name" -> parameter.name,
      "type" -> parameter.valueType.name,
      "argumentType" -> argumentType.name)
  }
}
