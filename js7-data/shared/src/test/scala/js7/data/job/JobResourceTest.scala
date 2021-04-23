package js7.data.job

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.controller.ControllerState.signableSimpleItemJsonCodec
import js7.data.item.{ItemRevision, SignableSimpleItem}
import js7.data.value.expression.Expression.{Argument, Concat, FunctionCall, ObjectExpression, StringConstant}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}
import org.scalatest.freespec.AnyFreeSpec

final class JobResourceTest extends AnyFreeSpec
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
        ObjectExpression(Map(
          "NAME" -> StringConstant("VALUE"),
          "MYPATH" -> Concat(
            StringConstant("/bin:"),
            FunctionCall("env", Seq(Argument(StringConstant("PATH"))))))),
        //Seq(JobResourcePath("DEPENDS-ON")),
        Some(ItemRevision(1))),
      json"""{
        "path": "JOB-RESOURCE",
        "TYPE": "JobResource",
        "env": {
          "NAME": "'VALUE'",
          "MYPATH": "'/bin:' ++ env('PATH')"
        },
        "itemRevision": 1
      }""")
  }
}
