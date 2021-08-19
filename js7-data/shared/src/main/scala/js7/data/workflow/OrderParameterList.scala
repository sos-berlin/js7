package js7.data.workflow

import cats.instances.vector._
import cats.syntax.semigroup._
import cats.syntax.traverse._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json, JsonObject}
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.CatsUtils.combine
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax._
import js7.data.Problems.EvaluationFailedProblem
import js7.data.controller.ControllerId
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.order.FreshOrder
import js7.data.value.expression.Scope.evalLazilyExpressions
import js7.data.value.expression.scopes.OrderScopes.{minimalJs7VariablesScope, scheduledScope}
import js7.data.value.expression.scopes.{EnvScope, JobResourceScope, NameToCheckedValueScope, NamedValueScope}
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{ListType, ListValue, NamedValues, ObjectType, ObjectValue, Value, ValueType}
import js7.data.workflow.OrderParameter.{Final, Optional, Required}
import js7.data.workflow.OrderParameterList._
import scala.collection.{MapView, View}

final case class OrderParameterList private(
  nameToParameter: Map[String, OrderParameter],
  allowUndeclared: Boolean)
{
  def referencedJobResourcePaths: Iterable[JobResourcePath] =
    nameToParameter.values.view.flatMap(_.referencedJobResourcePaths)

  def workflowOrderVariablesScope(
    freshOrder: FreshOrder,
    controllerId: ControllerId,
    pathToJobResource: PartialFunction[JobResourcePath, JobResource],
    nowScope: Scope)
  : Scope = {
    val nestedScope = combine(
      scheduledScope(freshOrder.scheduledFor),
      minimalJs7VariablesScope(freshOrder.id, freshOrder.workflowPath, controllerId),
      EnvScope,
      nowScope)
    combine(
      nestedScope,
      NamedValueScope(freshOrder.arguments),
      JobResourceScope(pathToJobResource, useScope = nestedScope))
  }

  def prepareOrderArguments(
    freshOrder: FreshOrder,
    controllerId: ControllerId,
    pathToJobResource: PartialFunction[JobResourcePath, JobResource],
    nowScope: Scope)
  : Checked[NamedValues] = {
    val nestedScope = combine(
      scheduledScope(freshOrder.scheduledFor),
      minimalJs7VariablesScope(freshOrder.id, freshOrder.workflowPath, controllerId),
      EnvScope,
      nowScope)
    prepareOrderArguments2(
      freshOrder.arguments,
      scope = combine(
        nestedScope,
        NamedValueScope(freshOrder.arguments),
        JobResourceScope(pathToJobResource, useScope = nestedScope))
    )
  }

  private def prepareOrderArguments2(arguments: NamedValues, scope: Scope)
  : Checked[NamedValues] =
  {
    lazy val recursiveScope: Scope = combine(
      scope,
      NameToCheckedValueScope(evalLazilyExpressions(nameToExpression)(
        recursiveScope)))

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
              (!p.expression.isConstant ? p.expression.eval(recursiveScope).map(p.name -> _))
                .sequence
                .left.map(EvaluationFailedProblem(p.name, p.expression, _))

            case (Some(_), _: OrderParameter.Final) =>
              Left(FinalOrderArgumentProblem(param.name))

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
          Left(MissingObjectFieldsProblem(prefix, missingNames))
        else {
          val undeclaredNames = v.nameToValue.keySet -- typ.nameToType.keySet
          if (undeclaredNames.nonEmpty)
            Left(UndeclaredObjectFieldsProblem(prefix, undeclaredNames))
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

  /** Add missing default and final values defined in OrderParameterList. */
  def addDefaults(arguments: Map[String, Value]): MapView[String, Value] =
    arguments
      .view
      .orElseMapView(nameToExpression.collectValues {
        case const: Expression.Constant => const.toValue
        // Expressions must have been evaluated with OrderAdded event.
        // The resulting values are expected to be in Order.arguments.
      })

  private[workflow] lazy val nameToExpression: MapView[String, Expression] =
    nameToParameter.view
      .collectValues {
        case OrderParameter.HasExpression(expr) => expr
      }
}

object OrderParameterList
{
  val default = new OrderParameterList(Map.empty, allowUndeclared = true)

  def apply(parameters: OrderParameter*): OrderParameterList =
    apply(parameters)

  def apply(parameters: Iterable[OrderParameter] = Nil, allowUndeclared: Boolean = false)
  : OrderParameterList =
    checked(parameters, allowUndeclared).orThrow

  def checked(parameters: Iterable[OrderParameter] = Nil, allowUndeclared: Boolean = false)
  : Checked[OrderParameterList] =
    parameters.toCheckedKeyedMap(_.name)
      .map(new OrderParameterList(_, allowUndeclared))

  // allowUndeclared serialized or deserialized separately (for compatibility)
  implicit val jsonEncoder: Encoder.AsObject[OrderParameterList] =
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

  implicit val jsonDecoder: Decoder[OrderParameterList] =
    cursor =>
      (cursor.value.asObject match {
        case None => Left(Problem("OrderParameterList expected")).toDecoderResult(cursor.history)
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
          .flatMap(p => OrderParameterList.checked(p).toDecoderResult(cursor.history))
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

  final case class FinalOrderArgumentProblem(name: String)
  extends Problem.Coded {
    def arguments = Map("name" -> name)
  }

  final case class MissingObjectFieldsProblem(path: String, name: Iterable[String])
  extends Problem.Coded {
    def arguments = Map(
      "path" -> path,
      "names" -> name.mkString(", "))
  }

  final case class UndeclaredObjectFieldsProblem(path: String, name: Iterable[String])
  extends Problem.Coded {
    def arguments = Map(
      "path" -> path,
      "names" -> name.mkString(", "))
  }
}