package js7.data.job

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.Test
import js7.data.controller.ControllerState.signableSimpleItemJsonCodec
import js7.data.item.{ItemRevision, SignableSimpleItem}
import js7.data.value.expression.Expression.{Argument, Concat, FunctionCall, StringConstant}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class JobResourceTest extends Test
{
  "JSON" in {
    testJsonDecoder[SignableSimpleItem](
      JobResource(JobResourcePath("JOB-RESOURCE")),
      json"""{
        "TYPE": "JobResource",
        "path": "JOB-RESOURCE"
      }""")

    testJson[SignableSimpleItem](
      JobResource(
        JobResourcePath("JOB-RESOURCE"),
        variables = Map(
          "VARIABLE" -> StringConstant("DEFAULT")),
        env = Map(
          "NAME" -> StringConstant("VALUE"),
          "MYPATH" -> Concat(
            StringConstant("/bin:"),
            FunctionCall("env", Seq(Argument(StringConstant("PATH")))))),
        Some(ItemRevision(1))),
      json"""{
        "path": "JOB-RESOURCE",
        "TYPE": "JobResource",
        "variables": {
          "VARIABLE": "'DEFAULT'"
        },
        "env": {
          "NAME": "'VALUE'",
          "MYPATH": "'/bin:' ++ env('PATH')"
        },
        "itemRevision": 1
      }""")
  }

  "JobResourcePath.itemTypeName" in {
    assert(JobResourcePath.itemTypeName == JobResource.typeName)
  }
}
