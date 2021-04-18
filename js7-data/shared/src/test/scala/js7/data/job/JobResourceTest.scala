package js7.data.job

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.controller.ControllerState.signableSimpleItemJsonCodec
import js7.data.item.{ItemRevision, SignableSimpleItem}
import js7.data.value.expression.Expression.ObjectExpression
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}
import org.scalatest.freespec.AnyFreeSpec

final class JobResourceTest extends AnyFreeSpec
{
  "JSON" in {
    testJsonDecoder[SignableSimpleItem](
      JobResource(JobResourceId("JOB-RESOURCE")),
      json"""{
        "TYPE": "JobResource",
        "id": "JOB-RESOURCE"
      }""")

    testJson[SignableSimpleItem](
      JobResource(
        JobResourceId("JOB-RESOURCE"),
        ObjectExpression.empty,
        //Seq(JobResourceId("DEPENDS-ON")),
        Some(ItemRevision(1))),
      json"""{
        "TYPE": "JobResource",
        "id": "JOB-RESOURCE",
        "env": {},
        "itemRevision": 1
      }""")
  }
}
