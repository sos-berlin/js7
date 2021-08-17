package js7.data.workflow

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.problem.Problem
import js7.data.Problems.{EvaluationFailedProblem, RecursiveEvaluationProblem}
import js7.data.value.expression.Expression.{NamedValue, StringConstant}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.expression.Scope
import js7.data.value.{BooleanValue, ListType, ListValue, NamedValues, NumberValue, ObjectType, ObjectValue, StringValue}
import js7.data.workflow.OrderParameterList.{MissingObjectFieldsProblem, MissingOrderArgumentProblem, UndeclaredObjectFieldsProblem, UndeclaredOrderArgumentProblem, WrongValueTypeProblem}
import js7.data.workflow.OrderParameterListTest._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class OrderParameterListTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(orderParameterList,
      json"""{
        "myOptional": {
          "type": "String",
          "default": "'DEFAULT'"
        },
        "myOptional2": {
          "type": "String",
          "default": "$$myOptional"
        },
        "myRequired": {
          "type": "Number"
        },
        "myFinal": {
          "final": "'FINAL-VALUE'"
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
        }
      }""")
  }

  private implicit val scope = Scope.empty

  "prepareOrderArguments" - {
    "No arguments - parameters are missing" in {
      assert(orderParameterList.prepareOrderArguments(NamedValues.empty) == Left(
        Problem.Combined(Set(
          MissingOrderArgumentProblem(myRequiredParameter).toSerialized,
          MissingOrderArgumentProblem(myObjectParameter).toSerialized,
          MissingOrderArgumentProblem(myListParameter).toSerialized))))
    }

    "Undeclared argument" in {
      assert(orderParameterList.prepareOrderArguments(NamedValues(
        "myRequired" -> myRequired,
        "myObject" -> myObject,
        "myList" -> myList,
        "UNEXPECTED" -> BooleanValue.True)) ==
        Left(UndeclaredOrderArgumentProblem("UNEXPECTED").toSerialized))
    }

    "Wrong type" in {
      assert(orderParameterList.prepareOrderArguments(NamedValues(
        "myRequired" -> BooleanValue.True,
        "myObject" -> ObjectValue(Map(
          "a" -> NumberValue(0),
          "b" -> ObjectValue(Map(
            "c" -> StringValue("WRONG"))))),
        "myList" -> StringValue("BLA"))) ==
        Left(Problem.Combined(Set(
          WrongValueTypeProblem("myRequired", BooleanValue, NumberValue).toSerialized,
          WrongValueTypeProblem("myObject.b.c", StringValue, BooleanValue).toSerialized,
          WrongValueTypeProblem("myList", StringValue, ListValue).toSerialized))))
    }

    "Missing object field" in {
      assert(orderParameterList.prepareOrderArguments(NamedValues(
        "myRequired" -> myRequired,
        "myObject" -> ObjectValue(Map(
          "a" -> NumberValue(0),
          "b" -> ObjectValue(Map.empty))),
        "myList" -> myList)) ==
        Left(MissingObjectFieldsProblem("myObject.b", Seq("c")).toSerialized))
    }

    "Undeclared object fields" in {
      assert(orderParameterList.prepareOrderArguments(NamedValues(
        "myRequired" -> myRequired,
        "myObject" -> myObject,
        "myList" -> ListValue(Seq(
          ObjectValue(Map(
            "a" -> NumberValue(0),
            "b" -> ObjectValue(Map(
              "c" -> BooleanValue(true),
              "UNDECLARED" -> BooleanValue(true))))))))) ==
        Left(UndeclaredObjectFieldsProblem("myList[0].b", Seq("UNDECLARED")).toSerialized))
    }

    "Mixed problems" in {
      assert(orderParameterList.prepareOrderArguments(NamedValues(
        "myRequired" -> BooleanValue.True,
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
    }

    lazy val minimalArgs = NamedValues(
      "myRequired" -> myRequired,
      "myObject" -> myObject,
      "myList" -> myList)

    "Minimal arguments — defaults are recalculated with each access" in {
      // myOptional2 is included due a non-constant expression: $myOptional
      // TODO This may be detected, because myOptional2 is actually pure
      assert(orderParameterList.prepareOrderArguments(minimalArgs) == Right(minimalArgs
        + ("myOptional2" -> StringValue("DEFAULT"))))

      assert(orderParameterList.addDefaults(minimalArgs).toMap ==
        minimalArgs ++ Map(
          "myOptional" -> StringValue("DEFAULT"),
          "myFinal" -> StringValue("FINAL-VALUE")))
    }

    lazy val overridingArgs = NamedValues(
      "myOptional" -> StringValue("OVERRIDDEN OPTIONAL"),
      "myOptional2" -> StringValue("OVERRIDDEN OPTIONAL"),
      "myRequired" -> myRequired,
      "myObject" -> myObject,
      "myList" -> myList)

    "Overriding arguments — only final variables are calculated with each access" in {
      assert(orderParameterList.prepareOrderArguments(overridingArgs) == Right(overridingArgs))
      assert(orderParameterList.addDefaults(overridingArgs).toMap ==
        overridingArgs ++ Map(
          "myFinal" -> StringValue("FINAL-VALUE")))
    }

    "Detect circular reference" in {
      val orderParameterList = OrderParameterList(Seq(
        OrderParameter.Optional("x", StringValue, StringConstant("OKAY")),
        OrderParameter.Optional("a", StringValue, expr("$b")),
        OrderParameter.Optional("b", StringValue, expr("$a"))))
      assert(orderParameterList.prepareOrderArguments(Map.empty) ==
        Left(Problem.Combined(Set(
          EvaluationFailedProblem("a", expr("$b"), RecursiveEvaluationProblem),
          EvaluationFailedProblem("b", expr("$a"), RecursiveEvaluationProblem)))))
    }
  }

  "nameToExpression" in {
    assert(orderParameterList.nameToExpression.toMap == Map(
      "myOptional" -> StringConstant("DEFAULT"),
      "myOptional2" -> NamedValue("myOptional"),
      "myFinal" -> StringConstant("FINAL-VALUE")))
  }

  "addDefaults" - {
    "nameToCheckedValue" in {
      assert(orderParameterList.addDefaults(Map.empty).toMap == Map(
        "myOptional" -> StringValue("DEFAULT"),
        //"myOptional2" -> StringValue("DEFAULT"),
        "myFinal" -> StringValue("FINAL-VALUE")))
    }

    "nameToCheckedValue: default expressions use order defined value" in {
      val nameToValue = orderParameterList.addDefaults(
        Map("myOptional" -> StringValue("BY-ORDER")))
      assert(nameToValue.toMap == Map(
        "myOptional" -> StringValue("BY-ORDER"),
        //"myOptional2" -> StringValue("BY-ORDER"),
        "myFinal" -> StringValue("FINAL-VALUE")))
    }
  }
}

private object OrderParameterListTest
{
  private val myRequiredParameter = OrderParameter.Required("myRequired", NumberValue)
  private val objectType = ObjectType(Map(
    "a" -> NumberValue,
    "b" -> ObjectType(Map(
      "c" -> BooleanValue))))
  private val myObjectParameter = OrderParameter.Required("myObject", objectType)
  private val myListParameter = OrderParameter.Required("myList", ListType(objectType))
  val orderParameterList = OrderParameterList(Seq(
    OrderParameter.Optional("myOptional", StringValue, expr("'DEFAULT'")),
    OrderParameter.Optional("myOptional2", StringValue, expr("$myOptional")),
    myRequiredParameter,
    OrderParameter.Final("myFinal", expr("'FINAL-VALUE'")),
    myObjectParameter,
    myListParameter))

  private val myRequired = NumberValue(1)

  private val myObject = ObjectValue(Map(
    "a" -> NumberValue(0),
    "b" -> ObjectValue(Map(
      "c" -> BooleanValue(true)))))

  private val myList = ListValue(Seq(myObject))
}
