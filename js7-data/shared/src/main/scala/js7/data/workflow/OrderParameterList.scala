package js7.data.workflow

import cats.instances.vector.*
import cats.syntax.traverse.*
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, JsonObject}
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.CatsUtils.combine
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.Problems.EvaluationFailedProblem
import js7.data.controller.ControllerId
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.order.FreshOrder
import js7.data.value.expression.Scope.evalLazilyExpressions
import js7.data.value.expression.scopes.{EnvScope, JobResourceScope, NamedValueScope}
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{AnyValue, ListType, ListValue, NamedValues, ObjectType, ObjectValue, Value, ValueType}
import js7.data.workflow.OrderParameter.{Final, Optional, Required}
import js7.data.workflow.OrderParameterList.*
import scala.collection.{MapView, View}

final case class OrderParameterList(
  nameToParameter: Map[String, OrderParameter],
  allowUndeclared: Boolean):

  def referencedJobResourcePaths: View[JobResourcePath] =
    nameToParameter.values.view.flatMap(_.referencedJobResourcePaths)

  def ++(other: OrderParameterList): OrderParameterList =
    concat(other)

  def concat(other: OrderParameterList): OrderParameterList =
    OrderParameterList(
      nameToParameter ++ other.nameToParameter,
      allowUndeclared = allowUndeclared && other.allowUndeclared)

  def prepareOrderArguments(
    freshOrder: FreshOrder,
    controllerId: ControllerId,
    pathToJobResource: PartialFunction[JobResourcePath, JobResource],
    nowScope: Scope)
  : Checked[NamedValues] =
    val nestedScope = combine(
      freshOrder.minimumScope(controllerId),
      EnvScope,
      nowScope)
    prepareOrderArguments2(
      freshOrder.arguments,
      scope = combine(
        nestedScope,
        NamedValueScope.simple(freshOrder.arguments),
        JobResourceScope(pathToJobResource, useScope = nestedScope)))

  private def prepareOrderArguments2(arguments: NamedValues, scope: Scope): Checked[NamedValues] =
    lazy val recursiveScope: Scope = combine(
      scope,
      NamedValueScope:
        evalLazilyExpressions(nameToExpression)(recursiveScope))

    val checkedAllDeclared =
      if allowUndeclared then
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
              // A pure expression can be checked beforehand
              (!p.expression.isPure ? p.expression.eval(recursiveScope).map(p.name -> _))
                .sequence
                .left.map(EvaluationFailedProblem(p.name, p.expression, _))

            case (Some(_), _: OrderParameter.Final) =>
              Left(FinalOrderArgumentProblem(param.name))

            case (Some(v), p: OrderParameter.HasType) =>
              for _ <- checkType(v, p.valueType, p.name) yield
                Some(param.name -> v)
          })
        .reduceLeftEither
        .map(_.flatten)

    checkedAllDeclared.combineLeft(declared)
      .map(_._2)
      .pipeIf(allowUndeclared)(_.map(arguments.view ++ _))
      .map(_.toMap)

  private def checkType(v: Value, typ: ValueType, prefix: => String): Checked[Unit] =
    (v, typ) match
      case (_, AnyValue) => Checked.unit

      case (v: ListValue, typ: ListType) =>
        v.elements
          .view
          .zipWithIndex
          .map { case (v, i) => checkType(v, typ.elementType, s"$prefix[$i]") }
          .collectFirst { case Left(problem) => problem }
          .fold(Checked.unit)(Left(_))

      case (v: ObjectValue, typ: ObjectType) =>
        val missingNames = typ.nameToType.keySet -- v.nameToValue.keySet
        if missingNames.nonEmpty then
          Left(MissingObjectFieldsProblem(prefix, missingNames))
        else
          val undeclaredNames = v.nameToValue.keySet -- typ.nameToType.keySet
          if undeclaredNames.nonEmpty then
            Left(UndeclaredObjectFieldsProblem(prefix, undeclaredNames))
          else
            v.nameToValue
              .toVector
              .traverse { case (k, v) => checkType(v, typ.nameToType(k), s"$prefix.$k") }
              .rightAs(())

      case _ =>
        if v.valueType != typ then
          Left(WrongValueTypeProblem(prefix, v.valueType, typ))
        else
          Checked.unit

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
      .collectValues:
        case OrderParameter.HasExpression(expr) => expr


object OrderParameterList:
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

  def fromDefaultExpressions(defaultValues: Map[String, Expression]): OrderParameterList =
    new OrderParameterList(
      defaultValues.map { case (k, v) => k -> OrderParameter.Optional(k, AnyValue, v) },
      allowUndeclared = default.allowUndeclared)

  // allowUndeclared serialized or deserialized separately (for compatibility)
  implicit val jsonEncoder: Encoder.AsObject[OrderParameterList] =
    o => JsonObject.fromIterable(
      for param <- o.nameToParameter.values yield
        param.name -> Json.fromJsonObject(
          param match {
            case Required(_, valueType) =>
              JsonObject(
                "type" -> valueType.asJson)

            case Optional(_, AnyValue, expression) =>
              JsonObject(
                "default" -> expression.asJson)

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
            for
              p <- {
                val json = c.value.asObject.get
                if json.contains("default") then
                  for
                    default <- c.get[Expression]("default")
                    typ <- c.getOrElse[ValueType]("type")(AnyValue)
                  yield Optional(name, typ, default)
                else if json.contains("final") then
                  for expression <- c.get[Expression]("final") yield
                    Final(name, expression)
                else if json.contains("type") then
                  for typ <- c.get[ValueType]("type") yield
                    Required(name, typ)
                else
                  Right(Required(name, AnyValue))
              }
            yield p
          }
          .flatMap(p => OrderParameterList.checked(p).toDecoderResult(cursor.history))
      })

  final case class MissingOrderArgumentProblem(parameter: OrderParameter.Required)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "name" -> parameter.name,
      "type" -> parameter.valueType.name)

  final case class UndeclaredOrderArgumentProblem(name: String) extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "name" -> name)

  final case class WrongValueTypeProblem(
    name: String,
    argumentType: ValueType,
    expectedType: ValueType)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "name" -> name,
      "type" -> argumentType.name,
      "expectedType" -> expectedType.name)

  final case class FinalOrderArgumentProblem(name: String)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map("name" -> name)

  final case class MissingObjectFieldsProblem(path: String, name: Iterable[String])
  extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "path" -> path,
      "names" -> name.mkString(", "))

  final case class UndeclaredObjectFieldsProblem(path: String, name: Iterable[String])
  extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "path" -> path,
      "names" -> name.mkString(", "))
