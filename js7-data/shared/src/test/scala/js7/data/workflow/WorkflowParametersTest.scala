package js7.data.workflow

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.data.value.{BooleanValue, ListValue, NamedValues, NumberValue, ObjectValue, StringValue}
import js7.data.workflow.WorkflowParameters.{MissingOrderArgumentProblem, UnexpectedOrderArgumentProblem, WrongOrderArgumentTypeProblem}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class WorkflowParametersTest extends AnyFreeSpec
{
  private val stringParameter = WorkflowParameter("string", StringValue)
  private val booleanParameter = WorkflowParameter("boolean", BooleanValue)
  private val numberParameter = WorkflowParameter("number", NumberValue(7))

  "JSON" in {
    testJson(
      WorkflowParameters(Seq(
        stringParameter,
        numberParameter,
        booleanParameter
      )),
      json"""{
        "boolean": {
          "type": "Boolean"
        },
        "number": {
          "type": "Number",
          "default": 7
        },
        "string": {
          "type": "String"
        }
      }""")
  }

  private val parameters = WorkflowParameters.checked(Seq(
    stringParameter,
    WorkflowParameter("number", NumberValue),
    booleanParameter,
    WorkflowParameter("string-default", StringValue("DEFAULT")),
    WorkflowParameter("boolean-default", BooleanValue(false)),
    WorkflowParameter("number-default", NumberValue(-1))
  )).orThrow

  private val validArguments = NamedValues(
    "string" -> StringValue("STRING"),
    "number" -> NumberValue(1),
    "boolean" -> BooleanValue(true))

  "checked" - {
    "Unsupported types" in {
      val checked = WorkflowParameters.checked(Seq(
        WorkflowParameter("number", NumberValue),
        WorkflowParameter("list", ListValue),
        WorkflowParameter("object", ObjectValue)))
      assert(checked == Left(Problem(
        "Unsupported type of parameter 'list': List\n" +
        " & Unsupported type of parameter 'object': Object")))
    }
  }

  "checkNamedValues" - {
    "Missing names" in {
      assert(parameters.checkNamedValues(NamedValues.empty) == Left(Problem.Combined(Set(
        MissingOrderArgumentProblem(stringParameter),
        MissingOrderArgumentProblem(booleanParameter),
        MissingOrderArgumentProblem(numberParameter)))))
    }

    "Undefined names" in {
      val args = validArguments ++ Seq("UNEXPECTED" -> NumberValue(3), "X" -> NumberValue(7))
      assert(parameters.checkNamedValues(args) == Left(Problem.Combined(Set(
        UnexpectedOrderArgumentProblem("UNEXPECTED"),
        UnexpectedOrderArgumentProblem("X")))))
    }

    "Wrong type" in {
      val args = NamedValues(
        "string" -> BooleanValue(true),
        "number" -> StringValue("1"),
        "boolean" -> NumberValue(1))
      assert(parameters.checkNamedValues(args) == Left(Problem.Combined(Set(
        WrongOrderArgumentTypeProblem(stringParameter, BooleanValue),
        WrongOrderArgumentTypeProblem(booleanParameter, NumberValue),
        WrongOrderArgumentTypeProblem(numberParameter, StringValue)))))
    }

    "Multiple errors" in {
      val args = NamedValues(
        "UNEXPECTED" -> NumberValue(1),
        "string" -> BooleanValue(true))
      assert(parameters.checkNamedValues(args) == Left(Problem.Combined(Set(
        UnexpectedOrderArgumentProblem("UNEXPECTED"),
        MissingOrderArgumentProblem(booleanParameter),
        MissingOrderArgumentProblem(numberParameter),
        WrongOrderArgumentTypeProblem(stringParameter, BooleanValue)))))
    }

    "Valid arguments" in {
      assert(parameters.checkNamedValues(validArguments) == Right(()))
    }
  }
}
