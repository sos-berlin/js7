package js7.data.order

import js7.base.circeutils.CirceUtils._
import js7.data.command.CancelMode
import js7.data.workflow.position.Position
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class OrderMarkTest extends AnyFreeSpec
{
  "Cancelling" in {
    testJson[OrderMark](OrderMark.Cancelling(CancelMode.FreshOnly), json"""
      {
        "TYPE": "Cancelling",
        "mode": {
          "TYPE": "FreshOnly"
        }
      }""")
  }

  "Suspending" in {
    testJson[OrderMark](OrderMark.Suspending, json"""
      {
        "TYPE": "Suspending"
      }""")
  }

  "Resuming" in {
    testJson[OrderMark](OrderMark.Resuming(Some(Position(1))), json"""
      {
        "TYPE": "Resuming",
        "position": [ 1 ]
      }""")
  }
}
