package com.sos.jobscheduler.tests

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.MediaTypes.{`application/json`, `text/plain`}
import akka.http.scaladsl.model.headers.{Accept, Location}
import com.google.inject.{AbstractModule, Provides}
import com.sos.jobscheduler.agent.data.views.AgentOverview
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.event.EventIdClock
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.http.AkkaHttpClient.HttpException
import com.sos.jobscheduler.common.http.AkkaHttpUtils.RichHttpResponse
import com.sos.jobscheduler.common.http.CirceToYaml.yamlToJson
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.order.OrderEvent.{OrderFinished, OrderProcessed}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.test.TestSetting.TestAgentPath
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import javax.inject.Singleton
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterAll, FreeSpec}

/**
  * @author Joacim Zschimmer
  */
final class MasterWebServiceTest extends FreeSpec with BeforeAndAfterAll with DirectoryProvider.ForScalaTest {

  private val testStartedAt = System.currentTimeMillis - 24*3600*1000
  protected val agentPaths = TestAgentPath :: AgentPath("/FOLDER/AGENT-A") :: Nil
  private lazy val uri = master.localUri
  private lazy val agent1Uri = directoryProvider.agents(0).localUri.toString
  private lazy val agent2Uri = directoryProvider.agents(1).localUri.toString
  private lazy val httpClient = new SimpleAkkaHttpClient(label = "MasterWebServiceTest")
  private lazy val eventCollector = new TestEventCollector

  override val masterModule = new AbstractModule {
    @Provides @Singleton def eventIdClock(): EventIdClock = new EventIdClock.Fixed(1000)
  }

  import httpClient.materializer

  override def beforeAll() = {
    directoryProvider.master.writeTxt(WorkflowPath("/WORKFLOW"), """job "/A" on "/AGENT";""")
    directoryProvider.master.writeTxt(WorkflowPath("/WORKFLOW-2"), """job "/A" on "/AGENT"; job "/MISSING" on "/AGENT";""")
    eventCollector.start(master.injector.instance[ActorRefFactory], master.injector.instance[StampedKeyedEventBus])
    directoryProvider.agents(0).file(JobPath("/A"), SourceType.Xml).xml =
      <job><script language="shell">{if (isWindows) "@exit" else "exit"}</script></job>
    super.beforeAll()
  }

  // --------------------------------------------------------
  // Use HTTP headers
  // - Content-Type: application/json
  // - Accept: application/json
  //
  // For GET additionally if a fresh response is required:
  // - Cache-Control: no-cache, no-store
  //
  // And over a not very fast network:
  // - Content-Encoding: gzip
  // - Accept-Encoding: gzip
  // --------------------------------------------------------

  "/master/api" in {
    val overview = httpClient.get[Json](s"$uri/master/api") await 99.s
    assert(overview.fieldOrThrow("version").stringOrThrow == BuildInfo.buildVersion)
    assert(overview.fieldOrThrow("startedAt").longOrThrow >= testStartedAt)
    assert(overview.fieldOrThrow("startedAt").longOrThrow < Timestamp.parse("2100-01-01T00:00:00Z").toEpochMilli)
  }

  "/master/api/workflow" - {
    testGet("master/api/workflow",
      json"""{
        "eventId": 1000005,
        "count": 2
      }""")

    testGet("master/api/workflow/",
      json"""{
        "eventId": 1000005,
        "array": [
          "/WORKFLOW",
          "/WORKFLOW-2"
        ]
      }""")

    testGet("master/api/workflow/WORKFLOW",
      json"""{
        "eventId": 1000005,
        "id": {
          "path": "/WORKFLOW",
          "versionId": "(initial)"
        },
        "instructions": [
          {
            "TYPE": "Job",
            "jobPath": "/A",
            "agentPath": "/AGENT"
          }
        ],
        "source": "job \"/A\" on \"/AGENT\";"
      }""")
  }

  "/master/api/agent" - {
    testGet("master/api/agent",
      json"""{
        "eventId": 1000005,
        "count": 2
      }""")

    testGet("master/api/agent/",
      json"""{
        "eventId": 1000005,
        "array": [
          "/AGENT",
          "/FOLDER/AGENT-A"
        ]
      }""")

    testGet("master/api/agent/?return=Agent",
      json"""{
        "eventId": 1000005,
        "array": [
          {
            "id": {
              "path": "/AGENT",
              "versionId": "(initial)"
            },
            "uri": "$agent1Uri"
          }, {
            "id": {
              "path": "/FOLDER/AGENT-A",
              "versionId": "(initial)"
            },
            "uri": "$agent2Uri"
          }
        ]
      }""")

    testGet("master/api/agent/FOLDER/AGENT-A?return=Agent",
      json"""{
        "eventId": 1000005,
        "id": {
          "path": "/FOLDER/AGENT-A",
          "versionId": "(initial)"
        },
        "uri": "$agent2Uri"
      }""")

    testGet("master/api/agent/FOLDER%2FAGENT-A?return=Agent",
      json"""{
        "eventId": 1000005,
        "id": {
          "path": "/FOLDER/AGENT-A",
          "versionId": "(initial)"
        },
        "uri": "$agent2Uri"
      }""")
  }

  "/master/api/agent-proxy" - {
    "/master/api/agent-proxy/FOLDER%2FAGENT-A" in {
      // Pass-through Agent. Slashes but the first in AgentPath must be coded as %2F.
      val overview = httpClient.get[AgentOverview](s"$uri/master/api/agent-proxy/FOLDER%2FAGENT-A") await 99.s
      assert(overview.version == BuildInfo.buildVersion)
    }

    "/master/api/agent-proxy/UNKNOWN returns 400" in {
      assert(
        intercept[HttpException] {
          httpClient.get[Json](s"$uri/master/api/agent-proxy/UNKNOWN") await 99.s
        }.status.intValue == 400/*BadRequest*/)
    }

    "/master/api/agent-proxy/FOLDER%2F/AGENT-A/NOT-FOUND returns 404" in {
      assert(
        intercept[HttpException] {
          httpClient.get[Json](s"$uri/master/api/agent-proxy/FOLDER%2FAGENT-A/task") await 99.s
        }.status.intValue == 404/*NotFound*/)
    }

    "/master/api/agent-proxy/FOLDER%2F/AGENT-A/timer" in {
      assert(
        intercept[HttpException] {
          httpClient.get[Json](s"$uri/master/api/agent-proxy/FOLDER%2FAGENT-A/timer") await 99.s
        }.status.intValue == 404/*NotFound*/)
    }
  }

  "/master/api/order" - {
    "POST order with missing workflow" in {
      val order = json"""{
        "id": "ORDER-ID",
        "workflowPath": "/MISSING"
      }"""
      val exception = intercept[HttpException] {
        httpClient.post(s"$uri/master/api/order", order) await 99.s
      }
      assert(exception.status.intValue == 400/*BadRequest*/)
      assert(exception.dataAsString contains "No such key 'Workflow:/MISSING'")  // Or similar
    }

    "POST" - {
      val order = json"""{
        "id": "ORDER-ID",
        "workflowPath": "/WORKFLOW"
      }"""

      "First" in {
        val response = httpClient.post_[Json](s"$uri/master/api/order", Accept(`application/json`) :: Nil, order) await 99.s
        assert(response.status.intValue == 201/*Created*/)
        assert(response.header[Location] == Some(Location(s"$uri/master/api/order/ORDER-ID")))
        response.entity.discardBytes()
      }

      "Duplicate" in {
        val response = httpClient.post_[Json](s"$uri/master/api/order", Accept(`application/json`) :: Nil, order) await 99.s
        assert(response.status.intValue == 409/*Conflict*/)
        assert(response.header[Location] == Some(Location(s"$uri/master/api/order/ORDER-ID")))
        response.entity.discardBytes()
      }
    }

    "GET" in {
      val order = httpClient.get[Json](s"$uri/master/api/order/ORDER-ID") await 99.s
      assert(order.fieldOrThrow("id") == Json.fromString("ORDER-ID"))  // May fail when OrderFinished
    }

    "(await OrderFinished)" in {
      eventCollector.await[OrderFinished]()  // Needed for test /master/api/events
    }
  }

  "/master/api/event (only JSON)" in {
    val events = httpClient.get[Json](s"$uri/master/api/event?after=0") await 99.s
    // Fields named "eventId" are renumbered for this test, "timestamp" are removed due to time-dependant values
    assert(manipulateEventsForTest(events) == json"""{
      "TYPE": "NonEmpty",
      "stampeds": [
        {
          "eventId": 1001,
          "TYPE": "VersionAdded",
          "versionId": "(initial)"
        }, {
          "eventId": 1002,
          "TYPE": "FileBasedAdded",
          "fileBased": {
            "TYPE": "Agent",
            "id": {
              "path": "/AGENT",
              "versionId": "(initial)"
            },
            "uri": "$agent1Uri"
          }
        }, {
          "eventId": 1003,
          "TYPE": "FileBasedAdded",
          "fileBased": {
            "TYPE": "Agent",
            "id": {
              "path": "/FOLDER/AGENT-A",
              "versionId": "(initial)"
            },
            "uri": "$agent2Uri"
          }
        }, {
          "eventId": 1004,
          "TYPE": "FileBasedAdded",
          "fileBased": {
            "TYPE": "Workflow",
            "id": {
              "path": "/WORKFLOW",
              "versionId": "(initial)"
            },
            "instructions": [
              {
                "TYPE": "Job",
                "jobPath": "/A",
                "agentPath": "/AGENT"
              }
            ],
            "source": "job \"/A\" on \"/AGENT\";"
          }
        }, {
          "eventId": 1005,
          "TYPE": "FileBasedAdded",
          "fileBased": {
            "TYPE": "Workflow",
            "id": {
              "path": "/WORKFLOW-2",
              "versionId": "(initial)"
            },
            "instructions": [
              {
                "TYPE": "Job",
                "jobPath": "/A",
                "agentPath": "/AGENT"
              }, {
                "TYPE": "Job",
                "jobPath": "/MISSING",
                "agentPath": "/AGENT"
              }
            ],
            "source": "job \"/A\" on \"/AGENT\"; job \"/MISSING\" on \"/AGENT\";"
          }
        }, {
          "eventId": 1006,
          "TYPE": "MasterReady"
        }, {
          "eventId": 1007,
          "TYPE": "OrderAdded",
          "key": "ORDER-ID",
          "workflowId": {
            "path": "/WORKFLOW",
            "versionId": "(initial)"
          }
        }, {
          "eventId": 1008,
          "TYPE": "OrderTransferredToAgent",
          "key": "ORDER-ID",
          "agentId": {
            "path": "/AGENT",
            "versionId": "(initial)"
          }
        }, {
          "eventId": 1009,
          "TYPE": "OrderProcessingStarted",
          "key": "ORDER-ID"
        }, {
          "eventId": 1010,
          "TYPE": "OrderProcessed",
          "key": "ORDER-ID",
          "variablesDiff": {
            "changed": {},
            "deleted": []
          },
          "outcome": {
            "TYPE": "Succeeded",
            "returnCode": 0
          }
        }, {
          "eventId": 1011,
          "TYPE": "OrderMoved",
          "key": "ORDER-ID",
          "to": [ 1 ]
        }, {
          "eventId": 1012,
          "TYPE": "OrderDetachable",
          "key": "ORDER-ID"
        }, {
          "eventId": 1013,
          "TYPE": "OrderTransferredToMaster",
          "key": "ORDER-ID"
        }, {
          "eventId": 1014,
          "TYPE": "OrderFinished",
          "key": "ORDER-ID"
        }
      ]
    }""")

    def manipulateEventsForTest(eventResponse: Json): Json = {
      val eventIds = Iterator.from(1001)
      def changeEvent(json: Json): Json = {
        val obj = json.asObject.get
        Json.fromJsonObject(JsonObject.fromMap(
          obj.toMap flatMap {
            case ("eventId", _) ⇒ ("eventId" → Json.fromInt(eventIds.next())) :: Nil
            case ("timestamp", _) ⇒ Nil
            case o ⇒ o :: Nil
          }))
      }
      eventResponse.hcursor
        .downField("stampeds").withFocus(_.mapArray(_ map changeEvent)).up
        .top.get
    }
  }

  "/master/api/graphql" - {
    "Syntax error" in {
      val query = "INVALID"
      val queryJson = json"""{ "query": "$query" }"""
      val response = httpClient.post_[Json](s"$uri/master/api/graphql", Accept(`application/json`) :: Nil, queryJson) await 99.s
      assert(response.status.intValue == 400/*BadRequest*/)
      assert(response.utf8StringFuture.await(99.s).parseJson ==
        json"""{
          "errors": [
            {
              "message": "Syntax error while parsing GraphQL query. Invalid input 'I', expected ExecutableDefinition or TypeSystemDefinition (line 1, column 1):\nINVALID\n^",
              "locations": [
                {
                  "line": 1,
                  "column": 1
                }
              ]
            }
          ]
        }""")
    }

    "Unknown field" in {
      val query = "{ UNKNOWN }"
      val queryJson = json"""{ "query": "$query" }"""
      val response = httpClient.post_[Json](s"$uri/master/api/graphql", Accept(`application/json`) :: Nil, queryJson) await 99.s
      assert(response.status.intValue == 400/*BadRequest*/)
      assert(response.utf8StringFuture.await(99.s).parseJson ==
        json"""{
          "errors": [
            {
              "message": "Query does not pass validation. Violations:\n\nCannot query field 'UNKNOWN' on type 'Query'. (line 1, column 3):\n{ UNKNOWN }\n  ^"
            }
          ]
        }""")
    }

    "Orders" - {
      val order2Id = OrderId("ORDER-MISSING-JOB")

      "(add order)" in {
        master.addOrderBlocking(FreshOrder(OrderId("ORDER-FRESH"), WorkflowPath("/WORKFLOW"), Some(Timestamp.parse("3000-01-01T12:00:00Z"))))
        master.addOrderBlocking(FreshOrder(order2Id, WorkflowPath("/WORKFLOW-2")))
        eventCollector.await[OrderProcessed](_.key == order2Id)
      }

      "Single order" in {
        val body = Json.obj(
          "query" → """
            query Q($orderId: OrderId!) {
              order(id: $orderId) {
                id
                workflowPosition {
                  workflowId { path, versionId }
                  position
                },
                attachedTo {
                  agentId { path, versionId }
                }
              }
            }""".asJson,
          "variables" → Json.obj(
            "orderId" → order2Id.asJson))
        assert(postGraphql(body) ==
          json"""{
            "data": {
              "order": {
                "id": "ORDER-MISSING-JOB",
                "workflowPosition": {
                  "workflowId": {
                    "path": "/WORKFLOW-2",
                    "versionId": "(initial)"
                  },
                  "position": [ 1 ]
                },
                "attachedTo": {
                  "agentId": {
                    "path": "/AGENT",
                    "versionId": "(initial)"
                  }
                }
              }
            }
          }""")
      }

      "All orders" in {
        val body = Json.obj(
          "query" → """{
            orders {
              id
              workflowPosition {
                workflowId { path }
                position
              }
            }
          }""".asJson)
        assert(postGraphql(body) ==
          json"""{
           "data": {
              "orders": [
                {
                  "id": "ORDER-FRESH",
                  "workflowPosition": {
                    "workflowId": { "path": "/WORKFLOW" },
                    "position": [ 0 ]
                  }
                }, {
                  "id": "ORDER-MISSING-JOB",
                  "workflowPosition": {
                    "workflowId": { "path": "/WORKFLOW-2" },
                    "position": [ 1 ]
                  }
                }
              ]
            }
          }""")
      }

      "Order in /WORKFLOW-2" in {
        val body = Json.obj(
          "query" → """
            query Q($workflowPath: WorkflowPath) {
              orders(workflowPath: $workflowPath) {
                id
                workflowPosition {
                  instruction {
                    ... on Job {
                      jobPath
                    }
                  }
                }
              }
            }""".asJson,
          "variables" → Json.obj(
            "workflowPath" → "/WORKFLOW-2".asJson))
        assert(postGraphql(body) ==
          json"""{
           "data": {
              "orders": [
                {
                  "id": "ORDER-MISSING-JOB",
                  "workflowPosition": {
                    "instruction": {
                      "jobPath": "/MISSING"
                    }
                  }
                }
              ]
            }
          }""")
      }
    }

    def postGraphql(graphql: Json): Json =
      httpClient.post(s"$uri/master/api/graphql", graphql).await(99.s)
  }

  "Commands" - {
    "Terminate" in {
      val cmd = json"""{ "TYPE": "Terminate" }"""
      testJson(
        httpClient.post(s"$uri/master/api", cmd) await 99.s,
        json"""{
          "TYPE": "Accepted"
        }""")
    }
  }

  private def testGet(suburi: String, expected: ⇒ Json, mainpulateResponse: Json ⇒ Json = identity): Unit =
    suburi - {
      "JSON" in {
        testJson(
          mainpulateResponse(httpClient.get[Json](s"$uri/$suburi") await 99.s),
          expected)
      }
      "YAML" in {
        val yamlString = httpClient.get_[String](s"$uri/$suburi", Accept(`text/plain`) :: Nil) await 99.s
        assert(yamlString.head.isLetter)  // A YAML object starts with the first field name
        testJson(mainpulateResponse(yamlToJson(yamlString)), expected)
      }
    }
}
