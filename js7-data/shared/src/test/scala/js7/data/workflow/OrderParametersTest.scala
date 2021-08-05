package js7.data.workflow

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.value.expression.Expression.{BooleanConstant, NumericConstant, StringConstant}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.expression.Scope
import js7.data.value.{BooleanValue, ListType, ListValue, NamedValues, NumberValue, ObjectType, ObjectValue, StringValue}
import js7.data.workflow.OrderParameters.{MissingOrderArgumentProblem, UndeclaredOrderArgumentProblem, WrongValueTypeProblem}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class OrderParametersTest extends AnyFreeSpec
{
  private val myStringParameter = OrderParameter.Required("myString", StringValue)
  private val myBooleanParameter = OrderParameter.Required("myBoolean", BooleanValue)
  private val myNumberParameter = OrderParameter.Optional("myNumber", NumberValue, NumericConstant(7))

  private val myNumberListParameter =
    OrderParameter.Required("myNumberList", ListType(NumberValue))

  private val myObjectParameter =
    OrderParameter.Required("myObject", ObjectType(Map(
      "a" -> StringValue,
      "b" -> NumberValue)))

  private val myObjectListParameter =
    OrderParameter.Optional(
      "myObjectList",
      ListType(ObjectType(Map(
        "a" -> NumberValue,
        "strings" -> ListType(StringValue)))),
      expr("[]"))

  private val myFinalVariable = OrderParameter.Final("myFinalVariable", StringConstant("EXPRESSION"))

  "JSON" in {
    testJson(
      OrderParameters(
        myStringParameter,
        myNumberParameter,
        myBooleanParameter,
        myNumberListParameter,
        myObjectParameter,
        myObjectListParameter,
        myFinalVariable),
      json"""{
        "myBoolean": {
          "type": "Boolean"
        },
        "myNumber": {
          "type": "Number",
          "default": "7"
        },
        "myString": {
          "type": "String"
        },
        "myNumberList": {
          "type": {
            "TYPE": "List",
            "elementType": "Number"
          }
        },
        "myObject": {
          "type": {
            "TYPE": "Object",
            "a": "String",
            "b": "Number"
          }
        },
        "myObjectList": {
          "default": "[]",
          "type": {
            "TYPE": "List",
            "elementType": {
              "TYPE": "Object",
              "a": "Number",
              "strings": {
                "TYPE": "List",
                "elementType": "String"
              }
            }
          }
        },
        "myFinalVariable": {
          "final": "'EXPRESSION'"
        }
      }""")
  }

  private val parameters = OrderParameters.checked(Seq(
    myStringParameter,
    OrderParameter("myNumber", NumberValue),
    myBooleanParameter,
    OrderParameter("string-default", StringConstant("DEFAULT")),
    OrderParameter("boolean-default", BooleanConstant(false)),
    OrderParameter("number-default", NumericConstant(-1)),
    //OrderParameter("numberList", xxx),
    myNumberListParameter,
    myObjectParameter,
    myObjectListParameter,
    OrderParameter.Final("myFinalVariable", StringConstant("EXPRESSION"))
  )).orThrow

  private val validArguments = NamedValues(
    "myString" -> StringValue("STRING"),
    "myNumber" -> NumberValue(1),
    "myBoolean" -> BooleanValue(true),
    "myNumberList" -> ListValue(Seq(NumberValue(1), NumberValue(2))),
    "myObject" -> ObjectValue(Map(
      "a" -> StringValue("STRING"),
      "b" -> NumberValue(2))),
    "myObjectList" -> ListValue.empty)

  "prepareOrderArguments" - {
    implicit val scope = Scope.empty
    val myNumberParameter = OrderParameter.Required("myNumber", NumberValue)

    "Missing names" in {
      assert(parameters.prepareOrderArguments(NamedValues.empty) == Left(Problem.Combined(Set(
        MissingOrderArgumentProblem(myStringParameter),
        MissingOrderArgumentProblem(myBooleanParameter),
        MissingOrderArgumentProblem(myNumberParameter),
        MissingOrderArgumentProblem(myObjectParameter),
        MissingOrderArgumentProblem(myNumberListParameter)))))
    }

    "Undefined names" in {
      val args = validArguments ++ Seq("UNEXPECTED" -> NumberValue(3), "X" -> NumberValue(7))
      assert(parameters.prepareOrderArguments(args) == Left(Problem.Combined(Set(
        UndeclaredOrderArgumentProblem("UNEXPECTED"),
        UndeclaredOrderArgumentProblem("X")))))
    }

    "Wrong type" in {
      val args = NamedValues(
        "myString" -> BooleanValue(true),
        "myNumber" -> StringValue("1"),
        "myBoolean" -> NumberValue(1),
        "myNumberList" -> ListValue(Seq(StringValue("WRONG TYPE"))),
        "myObject" -> ObjectValue(Map(
          "a" -> StringValue("WRONG TYPE"),
          "b" -> BooleanValue(true))))
      assert(parameters.prepareOrderArguments(args) == Left(Problem.Combined(Set(
        WrongValueTypeProblem("myString", BooleanValue, StringValue),
        WrongValueTypeProblem("myNumber", StringValue, NumberValue),
        WrongValueTypeProblem("myBoolean", NumberValue, BooleanValue),
        WrongValueTypeProblem("myNumberList[0]", StringValue, NumberValue),
        WrongValueTypeProblem("myObject.b", BooleanValue, NumberValue)))))
    }

    "Undeclared object field" in {
      val args = validArguments ++ Map(
        "myObject" -> ObjectValue(Map(
          "a" -> StringValue("STRING"),
          "b" -> NumberValue(2),
          "UNDECLARED" -> StringValue("UNDECLARED VALUE"))))
      assert(parameters.prepareOrderArguments(args) ==
        Left(Problem("Undeclared object fields in myObject: UNDECLARED")))
    }

    "Missing object fields" in {
      val args = validArguments + ("myObject" -> ObjectValue(Map.empty))
      assert(parameters.prepareOrderArguments(args) ==
        Left(Problem("Missing object fields in myObject: a, b")))
    }

    "Multiple errors" in {
      val args = NamedValues(
        "UNEXPECTED" -> NumberValue(1),
        "myString" -> BooleanValue(true))
      assert(parameters.prepareOrderArguments(args) == Left(Problem.Combined(Set(
        UndeclaredOrderArgumentProblem("UNEXPECTED"),
        MissingOrderArgumentProblem(myNumberParameter),
        MissingOrderArgumentProblem(myBooleanParameter),
        MissingOrderArgumentProblem(myNumberListParameter),
        MissingOrderArgumentProblem(myObjectParameter),
        WrongValueTypeProblem("myString", BooleanValue, StringValue)))))
    }

    "Valid arguments" in {
      assert(parameters.prepareOrderArguments(validArguments) == Right(validArguments))
    }

    "Workflow defined order variables" in {
      implicit val scope = Scope.empty
      pending // FIXME
    }
  }
}
