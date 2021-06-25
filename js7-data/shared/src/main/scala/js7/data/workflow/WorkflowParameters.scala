package js7.data.workflow

import cats.instances.vector._
import cats.syntax.semigroup._
import cats.syntax.traverse._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json, JsonObject}
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax._
import js7.data.job.JobResourcePath
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{NamedValues, Value, ValueType}
import js7.data.workflow.WorkflowParameter.{Optional, Required, WorkflowDefined}
import js7.data.workflow.WorkflowParameters._
import scala.collection.View

final case class WorkflowParameters private(
  nameToParameter: Map[String, WorkflowParameter],
  allowUndeclared: Boolean)
{
  def referencedJobResourcePaths: Iterable[JobResourcePath] =
    nameToParameter.values.view.flatMap(_.referencedJobResourcePaths)

  def prepareOrderArguments(arguments: NamedValues)(implicit scope: Scope): Checked[NamedValues] =
  {
    val checkedAllDeclared =
      if (allowUndeclared)
        Checked.unit
      else
        (arguments.keySet -- nameToParameter.keySet)
          .map[Problem](name => UndeclaredOrderArgumentProblem(name))
          .reduceOption(_ |+| _)
          .toLeft(())

    val declared: Checked[View[(String, Value)]] =
      nameToParameter.values.view
        .map(param =>
          (arguments.get(param.name), param) match {
            case (None, required: WorkflowParameter.Required) =>
              Left(MissingOrderArgumentProblem(required): Problem)

            case (None, WorkflowParameter.Optional(name, default)) =>
              Right(None)

            case (None, WorkflowParameter.WorkflowDefined(name, expr)) =>
              (!expr.isConstant ? expr.eval.map(name -> _))
                .sequence

            case (Some(value), parameter: WorkflowParameter.HasType) =>
              if (value.valueType != parameter.valueType)
                Left(WrongOrderArgumentTypeProblem(parameter, value.valueType))
              else
                Right(Some(param.name -> value))

            case (Some(_), _: WorkflowParameter.WorkflowDefined) =>
              Left(FixedOrderArgumentProblem(param.name))
          })
        .reduceLeftEither
        .map(_.flatten)

    checkedAllDeclared.combineLeft(declared)
      .map(_._2)
      .pipeIf(allowUndeclared)(_.map(arguments.view ++ _))
      .map(_.toMap)
  }

  def defaultArgument(name: String): Option[Value] =
    nameToParameter.get(name)
      .collect {
        case WorkflowParameter.HasValue(value) => value
      }

  lazy val defaultArguments: NamedValues =
    nameToParameter.view
      .collect {
        case (name, WorkflowParameter.HasValue(value)) => name -> value
      }
      .toMap
}

object WorkflowParameters
{
  val default = new WorkflowParameters(Map.empty, allowUndeclared = true)

  def apply(parameters: WorkflowParameter*): WorkflowParameters =
    apply(parameters)

  def apply(parameters: Iterable[WorkflowParameter] = Nil, allowUndeclared: Boolean = false)
  : WorkflowParameters =
    checked(parameters, allowUndeclared).orThrow

  def checked(parameters: Iterable[WorkflowParameter] = Nil, allowUndeclared: Boolean = false)
  : Checked[WorkflowParameters] =
    parameters.toCheckedKeyedMap(_.name)
      .map(new WorkflowParameters(_, allowUndeclared))

  // allowUndeclared serialized or deserialized separately (for compatibility)
  implicit val jsonEncoder: Encoder.AsObject[WorkflowParameters] =
    o => JsonObject.fromIterable(
      o.nameToParameter.values.map(param =>
        param.name -> Json.fromJsonObject(
          param match {
            case Required(_, valueType) =>
              JsonObject("type" -> valueType.asJson)
            case Optional(_, default) =>
              JsonObject("default" -> default.asJson)
            case WorkflowDefined(_, expression) =>
              JsonObject("expression" -> expression.asJson)
          })))

  implicit val jsonDecoder: Decoder[WorkflowParameters] =
    cursor =>
      (cursor.value.asObject match {
        case None => Left(Problem("WorkflowParameters expected")).toDecoderResult(cursor.history)
        case Some(obj) => obj
          .toVector
          .traverse { case (name, json) =>
            val c = json.hcursor
            for {
              p <- {
                val json = c.value.asObject.get
                if (json.contains("type"))
                  c.get[ValueType]("type").map(Required(name, _))
                else if (json.contains("default"))
                  c.get[Value]("default").map(Optional(name, _))
                else if (json.contains("expression"))
                  c.get[Expression]("expression").map(WorkflowDefined(name, _))
                else
                  Left(DecodingFailure("Missing type, default or value field", c.history))
              }
            } yield p
          }
          .flatMap(p => WorkflowParameters.checked(p).toDecoderResult(cursor.history))
      })

  final case class MissingOrderArgumentProblem(parameter: WorkflowParameter.Required)
  extends Problem.Coded {
    def arguments = Map(
      "name" -> parameter.name,
      "type" -> parameter.valueType.name)
  }

  final case class UndeclaredOrderArgumentProblem(name: String) extends Problem.Coded {
    def arguments = Map(
      "name" -> name)
  }

  final case class WrongOrderArgumentTypeProblem(
    parameter: WorkflowParameter.HasType,
    argumentType: ValueType)
  extends Problem.Coded {
    def arguments = Map(
      "name" -> parameter.name,
      "expectedType" -> parameter.valueType.name,
      "type" -> argumentType.name)
  }

  final case class FixedOrderArgumentProblem(name: String)
  extends Problem.Coded {
    def arguments = Map("name" -> name)
  }
}
