package js7.data.workflow

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.workflow.OrderParameterListTest.orderParameterList
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class OrderPreparationTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(OrderPreparation(OrderParameterList()),
      json"""{}""")

    testJson(OrderPreparation(OrderParameterList(allowUndeclared = true)),
      json"""{
         "allowUndeclared": true
      }""")

    testJson(OrderPreparation(orderParameterList.copy(allowUndeclared = true)),
      json"""{
        "allowUndeclared": true,
        "parameters": {
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
        }
      }""")
  }
}
