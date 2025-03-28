package js7.data.workflow

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.Timestamp
import js7.base.utils.Collections.implicits.RichIterable
import js7.data.Problems.{EvaluationFailedProblem, RecursiveEvaluationProblem}
import js7.data.controller.ControllerId
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.plan.PlanId
import js7.data.value.expression.Expression.{NamedValue, StringConstant}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.expression.Scope
import js7.data.value.expression.scopes.NowScope
import js7.data.value.{AnyValue, BooleanValue, ListType, ListValue, NamedValues, NumberValue, ObjectType, ObjectValue, StringValue}
import js7.data.workflow.OrderParameterList.{MissingObjectFieldsProblem, MissingOrderArgumentProblem, UndeclaredObjectFieldsProblem, UndeclaredOrderArgumentProblem, WrongValueTypeProblem}
import js7.data.workflow.OrderParameterListTest.*
import js7.tester.CirceJsonTester.testJson
import scala.collection.View

final class OrderParameterListTest extends OurTestSuite:

  "JSON" in:
    testJson(orderParameterList,
      json"""{
        "myRequired": {
          "type": "Number"
        },
        "myRequiredAny": {},
        "myOptional": {
          "type": "String",
          "default": "'DEFAULT VALUE'"
        },
        "myOptionalAny": {
          "default": "$$myOptional"
        },
        "myFinal": {
          "final": "'FINAL VALUE'"
        },
        "myFinal2": {
          "final": "$$myRequired"
        },
        "myObject": {
          "type": {
            "TYPE": "Object",
            "a": "Number",
            "b": {
              "TYPE": "Object",
              "c": "Boolean"
            }
          }
        },
        "myList": {
          "type": {
            "TYPE": "List",
            "elementType": {
              "TYPE": "Object",
              "a": "Number",
              "b": {
                "TYPE": "Object",
                "c": "Boolean"
              }
            }
          }
        },
        "myControllerId": {
          "final": "controllerId"
        },
        "myScheduledFor": {
          "final": "scheduledOrEmpty('yyyy-MM-dd', 'Antarctica/Troll')"
        },
        "myResource": {
          "final": "jobResourceVariable('myJobResource', 'fromRequired')"
        }
      }""")

  private implicit val scope: Scope = Scope.empty

  "prepareOrderArguments" - {
    implicit val orderParameterList = OrderParameterListTest.orderParameterList

    "No arguments - parameters are missing" in:
      assert(prepareOrderArguments(NamedValues.empty) == Left(
        Problem.Combined(Set(
          MissingOrderArgumentProblem(myRequiredParameter).toSerialized,
          MissingOrderArgumentProblem(myRequiredAnyParameter).toSerialized,
          MissingOrderArgumentProblem(myObjectParameter).toSerialized,
          MissingOrderArgumentProblem(myListParameter).toSerialized,
          EvaluationFailedProblem("myFinal2", expr("$myRequired"),
            Problem("No such named value: myRequired")).toSerialized,
          EvaluationFailedProblem("myResource", myResourceParameter.expression,
            Problem("No such named value: myRequired")).toSerialized
      ))))

    "Undeclared argument" in:
      assert(prepareOrderArguments(NamedValues(
        "myRequired" -> myRequired,
        "myRequiredAny" -> myRequired,
        "myObject" -> myObject,
        "myList" -> myList,
        "UNEXPECTED" -> BooleanValue.True)) ==
        Left(UndeclaredOrderArgumentProblem("UNEXPECTED").toSerialized))

    "Wrong type" in:
      assert(prepareOrderArguments(NamedValues(
        "myRequired" -> BooleanValue.True,
        "myRequiredAny" -> BooleanValue.True,
        "myObject" -> ObjectValue(Map(
          "a" -> NumberValue(0),
          "b" -> ObjectValue(Map(
            "c" -> StringValue("WRONG"))))),
        "myList" -> StringValue("BLA"))) ==
        Left(Problem.Combined(Set(
          WrongValueTypeProblem("myRequired", BooleanValue, NumberValue).toSerialized,
          WrongValueTypeProblem("myObject.b.c", StringValue, BooleanValue).toSerialized,
          WrongValueTypeProblem("myList", StringValue, ListValue).toSerialized))))

    "Missing object field" in:
      assert(prepareOrderArguments(NamedValues(
        "myRequired" -> myRequired,
        "myRequiredAny" -> myRequired,
        "myObject" -> ObjectValue(Map(
          "a" -> NumberValue(0),
          "b" -> ObjectValue(Map.empty))),
        "myList" -> myList)) ==
        Left(MissingObjectFieldsProblem("myObject.b", Seq("c")).toSerialized))

    "Undeclared object fields" in:
      assert(prepareOrderArguments(NamedValues(
        "myRequired" -> myRequired,
        "myRequiredAny" -> myRequired,
        "myObject" -> myObject,
        "myList" -> ListValue(Seq(
          ObjectValue(Map(
            "a" -> NumberValue(0),
            "b" -> ObjectValue(Map(
              "c" -> BooleanValue(true),
              "UNDECLARED" -> BooleanValue(true))))))))) ==
        Left(UndeclaredObjectFieldsProblem("myList[0].b", Seq("UNDECLARED")).toSerialized))

    "Mixed problems" in:
      assert(prepareOrderArguments(NamedValues(
        "myRequired" -> BooleanValue.True,
        "myRequiredAny" -> myRequired,
        "myList" -> ListValue(Seq(
          ObjectValue(Map(
            "a" -> NumberValue(0),
            "b" -> ObjectValue(Map(
              "c" -> BooleanValue(true),
              "UNDECLARED" -> BooleanValue(true)))))
        )))) ==
        Left(Problem.Combined(Set(
          WrongValueTypeProblem("myRequired", BooleanValue, NumberValue).toSerialized,
          UndeclaredObjectFieldsProblem("myList[0].b", Seq("UNDECLARED")).toSerialized,
          MissingOrderArgumentProblem(myObjectParameter).toSerialized))))

    "Minimal arguments — defaults are recalculated with each access" - {
      lazy val minimalArgs = NamedValues(
        "myRequired" -> myRequired,
        "myRequiredAny" -> myRequired,
        "myObject" -> myObject,
        "myList" -> myList)

      "prepareOrderArguments (arguments copied to the order)"  in:
        // myOptionalAny and myFinal2 are copied to the order
        // because they are non-constant expressions.
        // TODO This may be detected, because both are actually pure
        assert(prepareOrderArguments(minimalArgs) == Right(
          minimalArgs ++ Map(
            "myOptionalAny" -> StringValue("DEFAULT VALUE"),
            "myFinal2" -> myRequired,
            "myControllerId" -> StringValue("CONTROLLER"),
            "myScheduledFor" -> StringValue("2021-12-12"),
            "myResource" -> myRequired)))

      "addDefaults (arguments calculated at each access)" in:
        assert(orderParameterList.addDefaults(minimalArgs).toMap ==
          minimalArgs ++ Map(
            "myOptional" -> StringValue("DEFAULT VALUE"),
            "myFinal" -> StringValue("FINAL VALUE")))
    }

    "Overriding arguments — only final variables are calculated with each access" - {
      lazy val overridingArgs = NamedValues(
        "myRequired" -> myRequired,
        "myRequiredAny" -> myRequired,
        "myOptional" -> StringValue("OVERRIDDEN OPTIONAL"),
        "myObject" -> myObject,
        "myList" -> myList)

      "prepareOrderArguments (arguments copied to the order)" in:
        assert(prepareOrderArguments(overridingArgs) == Right(
          overridingArgs ++ Map(
            "myOptionalAny" -> StringValue("OVERRIDDEN OPTIONAL"),
            "myFinal2" -> myRequired,
            "myControllerId" -> StringValue("CONTROLLER"),
            "myScheduledFor" -> StringValue("2021-12-12"),
            "myResource" -> myRequired)))

      "addDefaults (arguments calculated at each access)" in:
        assert(orderParameterList.addDefaults(overridingArgs).toMap ==
          overridingArgs ++ Map(
            "myFinal" -> StringValue("FINAL VALUE")))
    }

    "Detect circular reference" in:
      implicit val orderParameterList = OrderParameterList(Seq(
        OrderParameter.Optional("x", StringValue, StringConstant("OKAY")),
        OrderParameter.Optional("a", StringValue, expr("$b")),
        OrderParameter.Optional("b", StringValue, expr("$a"))))
      assert(prepareOrderArguments(Map.empty) ==
        Left(Problem.Combined(Set(
          EvaluationFailedProblem("a", expr("$b"), RecursiveEvaluationProblem),
          EvaluationFailedProblem("b", expr("$a"), RecursiveEvaluationProblem)))))
  }

  "nameToExpression" in:
    assert(orderParameterList.nameToExpression.toMap == Map(
      "myOptional" -> StringConstant("DEFAULT VALUE"),
      "myOptionalAny" -> NamedValue("myOptional"),
      "myFinal" -> StringConstant("FINAL VALUE"),
      "myFinal2" -> expr("$myRequired"),
      "myControllerId" -> expr("controllerId"),
      "myScheduledFor" -> expr("scheduledOrEmpty('yyyy-MM-dd', 'Antarctica/Troll')"),
      "myResource" -> expr("jobResourceVariable('myJobResource', 'fromRequired')")))

  "addDefaults() returns only values from constant expressions" in:
    assert(orderParameterList.addDefaults(Map.empty).toMap == Map(
      "myOptional" -> StringValue("DEFAULT VALUE"),
      "myFinal" -> StringValue("FINAL VALUE")))

private object OrderParameterListTest:

  private val myRequiredParameter = OrderParameter.Required("myRequired", NumberValue)
  private val myRequiredAnyParameter = OrderParameter.Required("myRequiredAny", AnyValue)
  private val objectType = ObjectType(Map(
    "a" -> NumberValue,
    "b" -> ObjectType(Map(
      "c" -> BooleanValue))))
  private val myObjectParameter = OrderParameter.Required("myObject", objectType)
  private val myListParameter = OrderParameter.Required("myList", ListType(objectType))
  private val myResourceParameter = OrderParameter.Final("myResource",
    expr("jobResourceVariable('myJobResource', 'fromRequired')"))

  val orderParameterList = OrderParameterList(Seq(
    myRequiredParameter,
    myRequiredAnyParameter,
    OrderParameter.Optional("myOptional", StringValue, expr("'DEFAULT VALUE'")),
    OrderParameter.Optional("myOptionalAny", AnyValue, expr("$myOptional")),
    OrderParameter.Final("myFinal", expr("'FINAL VALUE'")),
    OrderParameter.Final("myFinal2", expr("$myRequired")),
    OrderParameter.Final("myControllerId", expr("controllerId")),
    OrderParameter.Final("myScheduledFor", expr("scheduledOrEmpty('yyyy-MM-dd', 'Antarctica/Troll')")),
    myResourceParameter,
    myObjectParameter,
    myListParameter))

  private val myRequired = NumberValue(333)

  private val myObject = ObjectValue(Map(
    "a" -> NumberValue(0),
    "b" -> ObjectValue(Map(
      "c" -> BooleanValue(true)))))

  private val myList = ListValue(Seq(myObject))

  private def prepareOrderArguments(freshOrderArguments: NamedValues)
    (implicit orderParameterList: OrderParameterList)
  =
    orderParameterList.prepareOrderArguments(
      FreshOrder(OrderId("ORDER"), WorkflowPath("WORKFLOW"), freshOrderArguments,
        PlanId.Global,
        Some(Timestamp("2021-12-12T12:00:00Z"))),
      ControllerId("CONTROLLER"),
      View(
        JobResource(
          JobResourcePath("myJobResource"),
          variables = Map(
            "fromRequired" -> expr("$myRequired")))
      ).toKeyedMap(_.path),
      NowScope(Timestamp("2021-08-19T12:00:00Z")))
