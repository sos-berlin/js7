package js7.data.workflow

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.Scope
import js7.data.value.{BooleanValue, NamedValues, NumberValue, StringValue}
import js7.data.workflow.OrderParameters.{MissingOrderArgumentProblem, UndeclaredOrderArgumentProblem, WrongOrderArgumentTypeProblem}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class OrderRequirementsTest extends AnyFreeSpec
{
  private val stringParameter = OrderParameter.Required("string", StringValue)
  private val orderRequirements = OrderRequirements(OrderParameters.checked(Seq(
    stringParameter,
    OrderParameter("string-default", StringValue("DEFAULT"))
  )).orThrow)

  "JSON" in {
    testJson(OrderRequirements(OrderParameters()),
      json"""{}""")

    testJson(OrderRequirements(OrderParameters(allowUndeclared = true)),
      json"""{
         "allowUndeclared": true
      }""")

    testJson(OrderRequirements(OrderParameters(
      Seq(
        OrderParameter.Required("required", NumberValue),
        OrderParameter.Optional("optional", BooleanValue.True),
        OrderParameter.WorkflowDefined("variable", StringConstant("VARIABLE"))))),
      json"""{
        "parameters": {
          "optional": {
            "default": true
          },
          "required": {
            "type": "Number"
          },
          "variable": {
            "expression": "'VARIABLE'"
          }
        }
      }""")
  }

  "prepareOrderArguments" - {
    "No arguments" in {
      assert(orderRequirements.parameters.prepareOrderArguments(NamedValues.empty)(Scope.empty) == Left(
        MissingOrderArgumentProblem(stringParameter).toSerialized))
    }

    "Undeclared argument" in {
      assert(orderRequirements.parameters.prepareOrderArguments(NamedValues("UNEXPECTED" -> BooleanValue.True))(Scope.empty) ==
        Left(Problem.Combined(Set(
          UndeclaredOrderArgumentProblem("UNEXPECTED").toSerialized,
          MissingOrderArgumentProblem(stringParameter).toSerialized))))
    }

    "Wrong type" in {
      assert(orderRequirements.parameters.prepareOrderArguments(NamedValues("string" -> BooleanValue.True))(Scope.empty) == Left(
        WrongOrderArgumentTypeProblem(stringParameter, BooleanValue).toSerialized))
    }

    "Valid arguments" in {
      val args = NamedValues("string" -> StringValue("STRING"))
      assert(orderRequirements.parameters.prepareOrderArguments(args)(Scope.empty) == Right(args))
    }
  }

  "defaultArgument" in {
    assert(orderRequirements.parameters.defaultArgument("UNKNOWN").isEmpty)
    assert(orderRequirements.parameters.defaultArgument("string").isEmpty)
    assert(orderRequirements.parameters.defaultArgument("string-default") == Some(StringValue("DEFAULT")))
  }

  "defaultArguments" in {
    assert(orderRequirements.parameters.defaultArguments == Map(
      "string-default" -> StringValue("DEFAULT")))
  }
}
