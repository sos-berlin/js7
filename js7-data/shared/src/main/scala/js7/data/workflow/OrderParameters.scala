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
import js7.data.value.{ListType, ListValue, NamedValues, ObjectType, ObjectValue, Value, ValueType}
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

            case (None, p: OrderParameter.HasExpression) =>
              (!p.expression.isConstant ? p.expression.eval.map(p.name -> _))
                .sequence

            case (Some(_), _: OrderParameter.Final) =>
              Left(FixedOrderArgumentProblem(param.name))

            case (Some(v), p: OrderParameter.HasType) =>
              for (_ <- checkType(v, p.valueType, p.name)) yield
                Some(param.name -> v)
          })
        .reduceLeftEither
        .map(_.flatten)

    checkedAllDeclared.combineLeft(declared)
      .map(_._2)
      .pipeIf(allowUndeclared)(_.map(arguments.view ++ _))
      .map(_.toMap)
  }

  private def checkType(v: Value, typ: ValueType, prefix: => String): Checked[Unit] =
    (v, typ) match {
      case (v: ListValue, typ: ListType) =>
        v.elements
          .view
          .zipWithIndex
          .map { case (v, i) => checkType(v, typ.elementType, s"$prefix[$i]") }
          .collectFirst { case Left(problem) => problem }
          .fold(Checked.unit)(Left(_))

      case (v: ObjectValue, typ: ObjectType) =>
        val missingNames = typ.nameToType.keySet -- v.nameToValue.keySet
        if (missingNames.nonEmpty)
          Left(Problem(s"Missing object fields in $prefix: ${missingNames.mkString(", ")}"))
        else {
          val undeclaredNames = v.nameToValue.keySet -- typ.nameToType.keySet
          if (undeclaredNames.nonEmpty)
            Left(Problem(s"Undeclared object fields in $prefix: ${undeclaredNames.mkString(", ")}"))
          else
            v.nameToValue
              .toVector
              .traverse { case (k, v) => checkType(v, typ.nameToType(k), s"$prefix.$k") }
              .rightAs(())
        }

      case _ =>
        if (v.valueType != typ)
          Left(WrongValueTypeProblem(prefix, v.valueType, typ))
        else
          Checked.unit
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
      for (param <- o.nameToParameter.values) yield
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
          }))

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

  final case class WrongValueTypeProblem(
    name: String,
    argumentType: ValueType,
    expectedType: ValueType)
  extends Problem.Coded {
    def arguments = Map(
      "name" -> name,
      "type" -> argumentType.name,
      "expectedType" -> expectedType.name)
  }

  final case class FixedOrderArgumentProblem(name: String)
  extends Problem.Coded {
    def arguments = Map("name" -> name)
  }
}
