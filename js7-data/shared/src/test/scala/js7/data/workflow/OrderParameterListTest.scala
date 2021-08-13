package js7.data.workflow

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.problem.Problem
import js7.data.value.expression.Expression.{BooleanConstant, StringConstant}
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
          "type": "Boolean",
          "default": "false"
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

  "Valid arguments" in {
    val args = NamedValues(
      "myRequired" -> myRequired,
      "myObject" -> myObject,
      "myList" -> myList)
    assert(orderParameterList.prepareOrderArguments(args) == Right(args))
  }

  "Override optional" in {
    val args = NamedValues(
      "myOptional" -> BooleanValue(true),
      "myRequired" -> myRequired,
      "myObject" -> myObject,
      "myList" -> myList)
    assert(orderParameterList.prepareOrderArguments(args) == Right(args))
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
  val orderParameterList = OrderParameterList(
    Seq(
      OrderParameter.Optional("myOptional", BooleanValue, BooleanConstant(false)),
      myRequiredParameter,
      OrderParameter.Final("myFinal", StringConstant("FINAL-VALUE")),
      myObjectParameter,
      myListParameter))

  private val myRequired = NumberValue(1)

  private val myObject = ObjectValue(Map(
    "a" -> NumberValue(0),
    "b" -> ObjectValue(Map(
      "c" -> BooleanValue(true)))))

  private val myList = ListValue(Seq(myObject))
}
