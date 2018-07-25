package com.sos.jobscheduler.tests

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import akka.http.scaladsl.model.MediaTypes.{`application/json`, `text/plain`}
import akka.http.scaladsl.model.StatusCodes.{Forbidden, NotFound, OK}
import akka.http.scaladsl.model.headers.{Accept, Location, RawHeader}
import akka.http.scaladsl.model.{HttpEntity, HttpHeader}
import com.google.inject.{AbstractModule, Provides}
import com.sos.jobscheduler.agent.data.views.AgentOverview
import com.sos.jobscheduler.agent.scheduler.job.{JobConfiguration, JobScript}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.event.EventIdClock
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.http.AkkaHttpClient.HttpException
import com.sos.jobscheduler.common.http.AkkaHttpUtils.RichHttpResponse
import com.sos.jobscheduler.common.http.CirceToYaml.yamlToJson
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits.RichPath
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.system.OperatingSystem.operatingSystem
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.order.OrderEvent.{OrderFinished, OrderProcessed}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.test.TestSetting.TestAgentPath
import com.sos.jobscheduler.master.data.events.MasterAgentEvent
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import com.sos.jobscheduler.tests.MasterWebServiceTest._
import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import java.time.ZoneId
import javax.inject.Singleton
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class MasterWebServiceTest extends FreeSpec with BeforeAndAfterAll with DirectoryProvider.ForScalaTest {

  private val testStartedAt = System.currentTimeMillis - 24*3600*1000

  private lazy val uri = master.localUri

  protected val agentPaths = TestAgentPath :: AgentPath("/FOLDER/AGENT-A") :: Nil
  private lazy val agent1Uri = directoryProvider.agents(0).localUri.toString
  private lazy val agent2Uri = directoryProvider.agents(1).localUri.toString
  private lazy val httpClient = new SimpleAkkaHttpClient(label = "MasterWebServiceTest", uri, "/master")

  private lazy val eventCollector = new TestEventCollector
  private var sessionToken: String = "INVALID"

  override val masterModule = new AbstractModule {
    @Provides @Singleton def eventIdClock(): EventIdClock = new EventIdClock.Fixed(1000)
  }

  import httpClient.materializer

  override def beforeAll() = {
    writeMasterConfiguration(directoryProvider.master)
    writeAgentConfiguration(directoryProvider.agents(0))
    eventCollector.start(master.injector.instance[ActorRefFactory], master.injector.instance[StampedKeyedEventBus])
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

  "Await AgentReady" in {
    // Proceed first after all AgentReady have been received, to get an event sequence as expected
    for (agentPath ← agentPaths) {
      master.eventWatch.await[MasterAgentEvent.AgentReady](predicate = _.key == agentPath)
    }
  }

  "/master/api" in {
    val overview = httpClient.get[Json](s"$uri/master/api") await 99.s
    assert(overview.fieldOrThrow("version").stringOrThrow == BuildInfo.buildVersion)
    assert(overview.fieldOrThrow("startedAt").longOrThrow >= testStartedAt)
    assert(overview.fieldOrThrow("startedAt").longOrThrow < Timestamp.parse("2100-01-01T00:00:00Z").toEpochMilli)
  }

  "Login" in {
    val cmd = json"""{
        "TYPE": "Login",
        "userAndPassword": {
          "userId": "TEST-USER",
          "password": "TEST-PASSWORD"
        }
      }"""
    val response = httpClient.post[Json, JsonObject](s"$uri/master/api/session", cmd) await 99.s

    /** Returns a structure like
      * {
      *   "TYPE": "LoggedIn",
      *   "sessionToken": "..."
      * }
      */
    assert(response("TYPE").get == "LoggedIn".asJson)
    sessionToken = response("sessionToken").get.asString.get
  }

  "/master/api/workflow" - {
    testGet("master/api/workflow",
      RawHeader("X-JobScheduler-Session", sessionToken) :: Nil,
      json"""{
        "eventId": 1000009,
        "count": 2
      }""")

    testGet("master/api/workflow/",
      RawHeader("X-JobScheduler-Session", sessionToken) :: Nil,
      json"""{
        "eventId": 1000009,
        "array": [
          "/FOLDER/WORKFLOW-2",
          "/WORKFLOW"
        ]
      }""")

    testGets("master/api/workflow/FOLDER/WORKFLOW-2" ::
             "master/api/workflow//FOLDER/WORKFLOW-2" ::
             "master/api/workflow/%2FFOLDER%2FWORKFLOW-2" :: Nil,
      RawHeader("X-JobScheduler-Session", sessionToken) :: Nil,
      json"""{
        "eventId": 1000009,
        "id": {
          "path": "/FOLDER/WORKFLOW-2",
          "versionId": "(initial)"
        },
        "instructions": [
          {
            "TYPE": "Job",
            "jobPath": "/B",
            "agentPath": "/AGENT"
          }, {
            "TYPE": "Job",
            "jobPath": "/MISSING",
            "agentPath": "/AGENT"
          }
        ],
        "source": "job \"/B\" on \"/AGENT\"; job \"/MISSING\" on \"/AGENT\";"
      }""")
  }

  "/master/api/agent" - {
    testGet("master/api/agent",
      RawHeader("X-JobScheduler-Session", sessionToken) :: Nil,
      json"""{
        "eventId": 1000009,
        "count": 2
      }""")

    testGet("master/api/agent/",
      RawHeader("X-JobScheduler-Session", sessionToken) :: Nil,
      json"""{
        "eventId": 1000009,
        "array": [
          "/AGENT",
          "/FOLDER/AGENT-A"
        ]
      }""")

    testGet("master/api/agent/?return=Agent",
      RawHeader("X-JobScheduler-Session", sessionToken) :: Nil,
      json"""{
        "eventId": 1000009,
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
      RawHeader("X-JobScheduler-Session", sessionToken) :: Nil,
      json"""{
        "eventId": 1000009,
        "id": {
          "path": "/FOLDER/AGENT-A",
          "versionId": "(initial)"
        },
        "uri": "$agent2Uri"
      }""")

    testGet("master/api/agent/FOLDER%2FAGENT-A?return=Agent",
      RawHeader("X-JobScheduler-Session", sessionToken) :: Nil,
      json"""{
        "eventId": 1000009,
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
      val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Nil
      val overview = httpClient.get[AgentOverview](s"$uri/master/api/agent-proxy/FOLDER%2FAGENT-A", Duration.Inf, headers) await 99.s
      assert(overview.version == BuildInfo.buildVersion)
    }

    "/master/api/agent-proxy/UNKNOWN returns 400" in {
      val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Nil
      assert(
        intercept[HttpException] {
          httpClient.get[Json](s"$uri/master/api/agent-proxy/UNKNOWN", headers) await 99.s
        }.status.intValue == 400/*BadRequest*/)
    }

    "/master/api/agent-proxy/FOLDER%2F/AGENT-A/NOT-FOUND returns 404" in {
      val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Nil
      assert(
        intercept[HttpException] {
          httpClient.get[Json](s"$uri/master/api/agent-proxy/FOLDER%2FAGENT-A/task", headers) await 99.s
        }.status.intValue == 404/*NotFound*/)
    }

    "/master/api/agent-proxy/FOLDER%2F/AGENT-A/timer" in {
      val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Nil
      assert(
        intercept[HttpException] {
          httpClient.get[Json](s"$uri/master/api/agent-proxy/FOLDER%2FAGENT-A/timer", headers) await 99.s
        }.status.intValue == 404/*NotFound*/)
    }
  }

  "/master/api/order" - {
    "POST" - {
      "Order with missing workflow is rejected" in {
        val order = json"""{
          "id": "ORDER-ID",
          "workflowPath": "/MISSING"
        }"""
        val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Nil
        val exception = intercept[HttpException] {
          httpClient.post[Json, Json](s"$uri/master/api/order", order, headers) await 99.s
        }
        assert(exception.status.intValue == 400/*BadRequest*/)
        assert(exception.dataAsString contains "No such key 'Workflow:/MISSING'")  // Or similar
      }

      val order = json"""{
        "id": "ORDER-ID",
        "workflowPath": "/WORKFLOW"
      }"""

      "First" in {
        val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Accept(`application/json`) :: Nil
        val response = httpClient.post_[Json](s"$uri/master/api/order", order, headers) await 99.s
        assert(response.status.intValue == 201/*Created*/)
        assert(response.header[Location] == Some(Location(s"$uri/master/api/order/ORDER-ID")))
        response.entity.discardBytes()
      }

      "Duplicate" in {
        val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Accept(`application/json`) :: Nil
        val response = httpClient.post_[Json](s"$uri/master/api/order", order, headers) await 99.s
        assert(response.status.intValue == 409/*Conflict*/)
        assert(response.header[Location] == Some(Location(s"$uri/master/api/order/ORDER-ID")))
        response.entity.discardBytes()
      }
    }

    testGet("master/api/order",
      RawHeader("X-JobScheduler-Session", sessionToken) :: Nil,
      json"""{
        "count": 1
      }""")

    testGet("master/api/order/",
      RawHeader("X-JobScheduler-Session", sessionToken) :: Nil,
      json"""{
        "array": [
          "ORDER-ID"
        ]
      }""",
      _.remove("eventId"))

    "master/api/order/?return=Order" in {
      val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Accept(`application/json`) :: Nil
      val response = httpClient.get[Json](s"$uri/master/api/order/?return=Order", headers) await 99.s
      val orders = response.fieldOrThrow("array").asArray.get
      assert(orders.length == 1)
      assert(orders(0).fieldOrThrow("id").stringOrThrow == "ORDER-ID")
    }

    "master/api/order/ORDER-ID" in {
      val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Nil
      val order = httpClient.get[Json](s"$uri/master/api/order/ORDER-ID", headers) await 99.s
      assert(order.fieldOrThrow("id") == Json.fromString("ORDER-ID"))  // May fail when OrderFinished
    }
  }

  "(await OrderFinished)" in {
    eventCollector.await[OrderFinished]()  // Needed for test /master/api/events
  }

  "/master/api/event (only JSON)" in {
    val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Nil
    val events = httpClient.get[Json](s"$uri/master/api/event?after=0", headers) await 99.s
    // Fields named "eventId" are renumbered for this test, "timestamp" are removed due to time-dependant values
    assert(manipulateEventsForTest(events) == json"""{
      "TYPE": "NonEmpty",
      "stamped": [
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
              "path": "/FOLDER/WORKFLOW-2",
              "versionId": "(initial)"
            },
            "instructions": [
              {
                "TYPE": "Job",
                "jobPath": "/B",
                "agentPath": "/AGENT"
              }, {
                "TYPE": "Job",
                "jobPath": "/MISSING",
                "agentPath": "/AGENT"
              }
            ],
            "source": "job \"/B\" on \"/AGENT\"; job \"/MISSING\" on \"/AGENT\";"
          }
        }, {
          "eventId": 1005,
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
          "eventId": 1006,
          "TYPE": "MasterReady",
          "masterId": "Master",
          "timezone": "${ZoneId.systemDefault.getId}"
        }, {
          "eventId": 1007,
          "TYPE": "AgentReady",
          "key": "/AGENT",
          "timezone": "${ZoneId.systemDefault.getId}"
        }, {
          "eventId": 1008,
          "TYPE": "OrderAdded",
          "key": "ORDER-ID",
          "workflowId": {
            "path": "/WORKFLOW",
            "versionId": "(initial)"
          }
        }, {
          "eventId": 1009,
          "TYPE": "OrderTransferredToAgent",
          "key": "ORDER-ID",
          "agentId": {
            "path": "/AGENT",
            "versionId": "(initial)"
          }
        }, {
          "eventId": 1010,
          "TYPE": "OrderProcessingStarted",
          "key": "ORDER-ID"
        }, {
          "eventId": 1011,
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
          "eventId": 1012,
          "TYPE": "OrderMoved",
          "key": "ORDER-ID",
          "to": [ 1 ]
        }, {
          "eventId": 1013,
          "TYPE": "OrderDetachable",
          "key": "ORDER-ID"
        }, {
          "eventId": 1014,
          "TYPE": "OrderTransferredToMaster",
          "key": "ORDER-ID"
        }, {
          "eventId": 1015,
          "TYPE": "OrderFinished",
          "key": "ORDER-ID"
        }
      ]
    }""")

    def manipulateEventsForTest(eventResponse: Json): Json = {
      def ignoreIt(json: Json): Boolean = {
        val obj = json.asObject.get.toMap
        obj("TYPE") == Json.fromString("AgentReady") && obj("key") != Json.fromString("/AGENT")  // Let through only AgentReady for one Agent, because ordering is undefined
      }
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
        .downField("stamped").withFocus(_.mapArray(_ filterNot ignoreIt map changeEvent)).up
        .top.get
    }
  }

  "/master/api/graphql" - {
    "Syntax error" in {
      val query = "INVALID"
      val queryJson = json"""{ "query": "$query" }"""
      val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Accept(`application/json`) :: Nil
      val response = httpClient.post_[Json](s"$uri/master/api/graphql", queryJson, headers) await 99.s
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
      val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Accept(`application/json`) :: Nil
      val response = httpClient.post_[Json](s"$uri/master/api/graphql", queryJson, headers) await 99.s
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
        master.addOrderBlocking(FreshOrder(order2Id, WorkflowPath("/FOLDER/WORKFLOW-2")))
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
                    "path": "/FOLDER/WORKFLOW-2",
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
                    "workflowId": { "path": "/FOLDER/WORKFLOW-2" },
                    "position": [ 1 ]
                  }
                }
              ]
            }
          }""")
      }

      "Order in /FOLDER/WORKFLOW-2" in {
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
            "workflowPath" → "/FOLDER/WORKFLOW-2".asJson))
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

    def postGraphql(graphql: Json): Json = {
      val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Nil
      httpClient.post[Json, Json](s"$uri/master/api/graphql", graphql, headers).await(99.s)
    }
  }

  "Unknown path returns 404 Not found" in {
    val response = httpClient.post_(s"$uri/master/UNKNOWN", Json.obj(), Nil) await 99.s
    assert(response.status == NotFound)
  }

  "CSRF with POST" - {
    lazy val testUri = s"$uri/master/TEST/post"

    "application/json is allowed" in {
      val response = httpClient.postRaw(testUri, Nil, HttpEntity(`application/json`, "{}")) await 99.s
      assert(response.status == OK)
    }

    "text/plain like HTML form POST is forbidden" in {
      val forbidden = httpClient.postRaw(testUri, Nil, HttpEntity(`text/plain(UTF-8)`, "STRING")) await 99.s
      assert(forbidden.status == Forbidden)
      assert(forbidden.utf8StringFuture.await(99.s) == Forbidden.defaultMessage)  //"HTML form POST is forbidden")
    }
  }

  "Commands" - {
    "Terminate" in {
      val cmd = json"""{ "TYPE": "Terminate" }"""
      val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Nil
      testJson(
        httpClient.post[Json, Json](s"$uri/master/api/command", cmd, headers) await 99.s,
        json"""{
          "TYPE": "Accepted"
        }""")
      master.terminated await 99.s
    }
  }

  private def testGets(suburis: Iterable[String], headers: ⇒ List[HttpHeader], expected: ⇒ Json, manipulateResponse: JsonObject ⇒ JsonObject = identity): Unit =
    for (suburi ← suburis) testGet(suburi, headers, expected, manipulateResponse)

  private def testGet(suburi: String, headers: ⇒ List[HttpHeader], expected: ⇒ Json, manipulateResponse: JsonObject ⇒ JsonObject = identity): Unit =
    suburi - {
      "JSON" in {
        testJson(
          manipulateResponse(httpClient.get[JsonObject](s"$uri/$suburi", Duration.Inf, headers) await 99.s),
          expected)
      }

      "YAML" in {
        val yamlString = httpClient.get_[String](s"$uri/$suburi", headers ::: Accept(`text/plain`) :: Nil) await 99.s
        assert(yamlString.head.isLetter)  // A YAML object starts with the first field name
        testJson(manipulateResponse(yamlToJson(yamlString).orThrow.asObject.get), expected)
      }
    }
}

object MasterWebServiceTest
{
  private def writeMasterConfiguration(master: DirectoryProvider.Tree): Unit = {
    (master.config / "master.conf").contentString =
      """jobscheduler.webserver.test = true
        |""".stripMargin
    (master.config / "private" / "private.conf").append(
      """jobscheduler.auth.users {
        |  TEST-USER: "plain:TEST-PASSWORD"
        |}
        |""".stripMargin)
    master.writeTxt(WorkflowPath("/WORKFLOW"), """job "/A" on "/AGENT";""")
    master.writeTxt(WorkflowPath("/FOLDER/WORKFLOW-2"), """job "/B" on "/AGENT"; job "/MISSING" on "/AGENT";""")
  }

  private def writeAgentConfiguration(agent: DirectoryProvider.AgentTree): Unit = {
    agent.writeJson(JobConfiguration(
      JobPath("/A") % VersionId.Anonymous,
      JobScript(operatingSystem.sleepingShellScript(1.second))))  // Allow some time to check web service before order finishes
    agent.writeJson(JobConfiguration(
      JobPath("/B") % VersionId.Anonymous,
      JobScript(operatingSystem.sleepingShellScript(0.seconds))))
  }
}
