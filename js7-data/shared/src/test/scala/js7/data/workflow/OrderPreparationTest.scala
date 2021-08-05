package js7.data.workflow

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.data.value.expression.Expression.{BooleanConstant, StringConstant}
import js7.data.value.expression.Scope
import js7.data.value.{BooleanValue, NamedValues, NumberValue, StringValue}
import js7.data.workflow.OrderParameters.{MissingOrderArgumentProblem, UndeclaredOrderArgumentProblem, WrongValueTypeProblem}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class OrderPreparationTest extends AnyFreeSpec
{
  private val stringParameter = OrderParameter.Required("string", StringValue)
  private val orderPreparation = OrderPreparation(OrderParameters.checked(Seq(
    stringParameter,
    OrderParameter("string-default", StringConstant("DEFAULT"))
  )).orThrow)

  "JSON" in {
    testJson(OrderPreparation(OrderParameters()),
      json"""{}""")

    testJson(OrderPreparation(OrderParameters(allowUndeclared = true)),
      json"""{
         "allowUndeclared": true
      }""")

    testJson(OrderPreparation(OrderParameters(
      Seq(
        OrderParameter.Required("myRequired", NumberValue),
        OrderParameter.Optional("myOptional", BooleanValue, BooleanConstant(true)),
        OrderParameter.Final("myFinal", StringConstant("FINAL"))),
      allowUndeclared = true)),
      json"""{
        "parameters": {
          "myOptional": {
            "type": "Boolean",
            "default": "true"
          },
          "myRequired": {
            "type": "Number"
          },
          "myFinal": {
            "final": "'FINAL'"
          }
        },
        "allowUndeclared": true
      }""")
  }

  "prepareOrderArguments" - {
    implicit val scope = Scope.empty

    "No arguments" in {
      assert(orderPreparation.parameters.prepareOrderArguments(NamedValues.empty) == Left(
        MissingOrderArgumentProblem(stringParameter).toSerialized))
    }

    "Undeclared argument" in {
      assert(orderPreparation.parameters.prepareOrderArguments(NamedValues("UNEXPECTED" -> BooleanValue.True)) ==
        Left(Problem.Combined(Set(
          UndeclaredOrderArgumentProblem("UNEXPECTED").toSerialized,
          MissingOrderArgumentProblem(stringParameter).toSerialized))))
    }

    "Wrong type" in {
      assert(orderPreparation.parameters.prepareOrderArguments(NamedValues("string" -> BooleanValue.True)) ==
        Left(WrongValueTypeProblem("string", BooleanValue, StringValue).toSerialized))
    }

    "Valid arguments" in {
      val args = NamedValues("string" -> StringValue("STRING"))
      assert(orderPreparation.parameters.prepareOrderArguments(args) == Right(args))
    }
  }

  "defaultArgument" in {
    assert(orderPreparation.parameters.defaultArgument("UNKNOWN").isEmpty)
    assert(orderPreparation.parameters.defaultArgument("string").isEmpty)
    assert(orderPreparation.parameters.defaultArgument("string-default") == Some(StringValue("DEFAULT")))
  }

  "defaultArguments" in {
    assert(orderPreparation.parameters.defaultArguments == Map(
      "string-default" -> StringValue("DEFAULT")))
  }
}
