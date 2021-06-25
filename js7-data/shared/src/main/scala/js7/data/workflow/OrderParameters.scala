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
import js7.data.workflow.OrderParameter.{Final, Optional, Required}
import js7.data.workflow.OrderParameters._
import scala.collection.View

final case class OrderParameters private(
  nameToParameter: Map[String, OrderParameter],
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
            case (None, required: OrderParameter.Required) =>
              Left(MissingOrderArgumentProblem(required): Problem)

            case (None, p @ OrderParameter.HasExpression(expr)) =>
              (!expr.isConstant ? expr.eval.map(p.name -> _))
                .sequence

            case (Some(_), _: OrderParameter.Final) =>
              Left(FixedOrderArgumentProblem(param.name))

            case (Some(value), parameter: OrderParameter.HasType) =>
              if (value.valueType != parameter.valueType)
                Left(WrongOrderArgumentTypeProblem(parameter, value.valueType))
              else
                Right(Some(param.name -> value))
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
        case OrderParameter.HasValue(value) => value
      }

  lazy val defaultArguments: NamedValues =
    nameToParameter.view
      .collect {
        case (name, OrderParameter.HasValue(value)) => name -> value
      }
      .toMap
}

object OrderParameters
{
  val default = new OrderParameters(Map.empty, allowUndeclared = true)

  def apply(parameters: OrderParameter*): OrderParameters =
    apply(parameters)

  def apply(parameters: Iterable[OrderParameter] = Nil, allowUndeclared: Boolean = false)
  : OrderParameters =
    checked(parameters, allowUndeclared).orThrow

  def checked(parameters: Iterable[OrderParameter] = Nil, allowUndeclared: Boolean = false)
  : Checked[OrderParameters] =
    parameters.toCheckedKeyedMap(_.name)
      .map(new OrderParameters(_, allowUndeclared))

  // allowUndeclared serialized or deserialized separately (for compatibility)
  implicit val jsonEncoder: Encoder.AsObject[OrderParameters] =
    o => JsonObject.fromIterable(
      o.nameToParameter.values.map(param =>
        param.name -> Json.fromJsonObject(
          param match {
            case Required(_, valueType) =>
              JsonObject(
                "type" -> valueType.asJson)

            case Optional(_, valueType, expression) =>
              JsonObject(
                "type" -> valueType.asJson,
                "default" -> expression.asJson)

            case Final(_, expression) =>
              JsonObject(
                "final" -> expression.asJson)
          })))

  implicit val jsonDecoder: Decoder[OrderParameters] =
    cursor =>
      (cursor.value.asObject match {
        case None => Left(Problem("OrderParameters expected")).toDecoderResult(cursor.history)
        case Some(obj) => obj
          .toVector
          .traverse { case (name, json) =>
            val c = json.hcursor
            for {
              p <- {
                val json = c.value.asObject.get
                if (json.contains("default"))
                  for {
                    default <- c.get[Expression]("default")
                    typ <- c.get[ValueType]("type")
                  } yield Optional(name, typ, default)
                else if (json.contains("final"))
                  for {
                    expression <- c.get[Expression]("final")
                  } yield Final(name, expression)
                else if (json.contains("type"))
                  for {
                    typ <- c.get[ValueType]("type")
                  } yield Required(name, typ)
                else
                  Left(DecodingFailure("""Missing "type", "default" or "final" field""", c.history))
              }
            } yield p
          }
          .flatMap(p => OrderParameters.checked(p).toDecoderResult(cursor.history))
      })

  final case class MissingOrderArgumentProblem(parameter: OrderParameter.Required)
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
    parameter: OrderParameter.HasType,
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
