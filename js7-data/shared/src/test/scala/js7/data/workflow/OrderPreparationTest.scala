package js7.data.workflow

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.workflow.OrderParameterListTest.orderParameterList
import js7.tester.CirceJsonTester.testJson

final class OrderPreparationTest extends OurTestSuite:
  "JSON" in:
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
          "myRequired": {
            "type": "Number"
          },
          "myRequiredAny": {},
          "myOptional": {
            "type": "String",
            "default": "'DEFAULT VALUE'"
          },
          "myOptionalAny": {
            "default": "$$myOptional"
          },
          "myFinal": {
            "final": "'FINAL VALUE'"
          },
          "myFinal2": {
            "final": "$$myRequired"
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
          },
          "myControllerId": {
            "final": "controllerId"
          },
          "myScheduledFor": {
            "final": "scheduledOrEmpty('yyyy-MM-dd', 'Antarctica/Troll')"
          },
          "myResource": {
            "final": "jobResourceVariable('myJobResource', 'fromRequired')"
          }
        }
      }""")
