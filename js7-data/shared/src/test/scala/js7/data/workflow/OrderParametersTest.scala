package js7.data.workflow

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.Scope
import js7.data.value.{BooleanValue, NamedValues, NumberValue, StringValue}
import js7.data.workflow.OrderParameters.{MissingOrderArgumentProblem, UndeclaredOrderArgumentProblem, WrongOrderArgumentTypeProblem}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class OrderParametersTest extends AnyFreeSpec
{
  private val stringParameter = OrderParameter.Required("string", StringValue)
  private val booleanParameter = OrderParameter.Required("boolean", BooleanValue)
  private val numberParameter = OrderParameter.Optional("number", NumberValue(7))
  private val workflowDefined = OrderParameter.WorkflowDefined("workflowDefined", StringConstant("EXPRESSION"))

  "JSON" in {
    testJson(
      OrderParameters(
        stringParameter,
        numberParameter,
        booleanParameter,
        workflowDefined),
      json"""{
        "boolean": {
          "type": "Boolean"
        },
        "number": {
          "default": 7
        },
        "string": {
          "type": "String"
        },
        "workflowDefined": {
          "expression": "'EXPRESSION'"
        }
      }""")
  }

  private val parameters = OrderParameters.checked(Seq(
    stringParameter,
    OrderParameter("number", NumberValue),
    booleanParameter,
    OrderParameter("string-default", StringValue("DEFAULT")),
    OrderParameter("boolean-default", BooleanValue(false)),
    OrderParameter("number-default", NumberValue(-1)),
    OrderParameter.WorkflowDefined("workflowDefined", StringConstant("EXPRESSION"))
  )).orThrow

  private val validArguments = NamedValues(
    "string" -> StringValue("STRING"),
    "number" -> NumberValue(1),
    "boolean" -> BooleanValue(true))

  "prepareOrderArguments" - {
    implicit val scope = Scope.empty
    val numberParameter = OrderParameter.Required("number", NumberValue)

    "Missing names" in {
      assert(parameters.prepareOrderArguments(NamedValues.empty) == Left(Problem.Combined(Set(
        MissingOrderArgumentProblem(stringParameter),
        MissingOrderArgumentProblem(booleanParameter),
        MissingOrderArgumentProblem(numberParameter)))))
    }

    "Undefined names" in {
      val args = validArguments ++ Seq("UNEXPECTED" -> NumberValue(3), "X" -> NumberValue(7))
      assert(parameters.prepareOrderArguments(args) == Left(Problem.Combined(Set(
        UndeclaredOrderArgumentProblem("UNEXPECTED"),
        UndeclaredOrderArgumentProblem("X")))))
    }

    "Wrong type" in {
      val args = NamedValues(
        "string" -> BooleanValue(true),
        "number" -> StringValue("1"),
        "boolean" -> NumberValue(1))
      assert(parameters.prepareOrderArguments(args) == Left(Problem.Combined(Set(
        WrongOrderArgumentTypeProblem(stringParameter, BooleanValue),
        WrongOrderArgumentTypeProblem(booleanParameter, NumberValue),
        WrongOrderArgumentTypeProblem(numberParameter, StringValue)))))
    }

    "Multiple errors" in {
      val args = NamedValues(
        "UNEXPECTED" -> NumberValue(1),
        "string" -> BooleanValue(true))
      assert(parameters.prepareOrderArguments(args) == Left(Problem.Combined(Set(
        UndeclaredOrderArgumentProblem("UNEXPECTED"),
        MissingOrderArgumentProblem(booleanParameter),
        MissingOrderArgumentProblem(numberParameter),
        WrongOrderArgumentTypeProblem(stringParameter, BooleanValue)))))
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
