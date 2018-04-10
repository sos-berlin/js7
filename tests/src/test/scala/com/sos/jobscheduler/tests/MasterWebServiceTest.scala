package com.sos.jobscheduler.tests

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes.{BadRequest, Conflict, Created, InternalServerError, NotFound}
import akka.http.scaladsl.model.headers.Location
import com.google.inject.{AbstractModule, Provides}
import com.sos.jobscheduler.agent.views.AgentOverview
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.event.EventIdClock
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.http.AkkaHttpUtils.RichHttpResponse
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.test.TestSetting.TestAgentPath
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.{Json, JsonObject}
import javax.inject.Singleton
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * @author Joacim Zschimmer
  */
final class MasterWebServiceTest extends FreeSpec with BeforeAndAfterAll with DirectoryProvider.ForScalaTest {

  private val testStartedAt = System.currentTimeMillis - 24*3600*1000
  protected val agentPaths = TestAgentPath :: AgentPath("/FOLDER/AGENT-A") :: Nil
  private lazy val agent1Uri = directoryProvider.agents(0).localUri.toString
  private lazy val agent2Uri = directoryProvider.agents(1).localUri.toString
  private lazy val httpClient = new SimpleAkkaHttpClient(label = "MasterWebServiceTest")
  private lazy val eventCollector = new TestEventCollector

  override val masterModule = new AbstractModule {
    @Provides @Singleton def eventIdClock(): EventIdClock = new EventIdClock.Fixed(1000)
  }

  override def beforeAll() = {
    directoryProvider.master.writeTxt(WorkflowPath("/WORKFLOW"), """job "/A" on "/AGENT";""")
    eventCollector.start(master.injector.instance[ActorRefFactory], master.injector.instance[StampedKeyedEventBus])
    directoryProvider.agents(0).file(JobPath("/A"), SourceType.Xml).xml = <job><script language="shell">exit</script></job>
    super.beforeAll()
  }

  // --------------------------------------------------------
  // Use headers
  // - Content-Type: application/json
  // - Accept: application/json
  // And over a not very fast network:
  // - Content-Encoding: gzip
  // - Accept-Encoding: gzip
  // --------------------------------------------------------

  "/master/api" in {
    val overview = httpClient.get[Json](master.localUri + "/master/api") await 99.s
    assert(overview.fieldOrThrow("version").stringOrThrow == BuildInfo.buildVersion)
    assert(overview.fieldOrThrow("startedAt").longOrThrow >= testStartedAt)
    assert(overview.fieldOrThrow("startedAt").longOrThrow < Timestamp.parse("2100-01-01T00:00:00Z").toEpochMilli)
  }

  "/master/api/workflow" in {
    testJson(
      httpClient.get[Json](master.localUri + "/master/api/workflow") await 99.s,
      json"""{
        "eventId": 1000005,
        "count": 1
      }""")
  }

  "/master/api/workflow/" in {
    testJson(
      httpClient.get[Json](master.localUri + "/master/api/workflow/") await 99.s,
      json"""{
        "eventId": 1000005,
        "value": [
          "/WORKFLOW"
        ]
      }""")
  }

  "/master/api/workflow/WORKFLOW" in {
    testJson(
      httpClient.get[Json](master.localUri + "/master/api/workflow/WORKFLOW") await 99.s,
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

  "/master/api/agent" in {
    testJson(
      httpClient.get[Json](master.localUri + "/master/api/agent") await 99.s,
      json"""{
        "eventId": 1000005,
        "count": 2
      }""")
  }

  "/master/api/agent/" in {
    testJson(
      httpClient.get[Json](master.localUri + "/master/api/agent/") await 99.s,
      json"""{
        "eventId": 1000005,
        "value": [
          "/AGENT",
          "/FOLDER/AGENT-A"
        ]
      }""")
  }

  "/master/api/agent/?return=Agent" in {
    testJson(
      httpClient.get[Json](master.localUri + "/master/api/agent/?return=Agent") await 99.s,
      json"""{
        "eventId": 1000005,
        "value": [
          {
            "id": {
              "path": "/AGENT",
              "versionId": "(initial)"
            },
            "uri": "$agent1Uri"
          },
          {
            "id": {
              "path": "/FOLDER/AGENT-A",
              "versionId": "(initial)"
            },
            "uri": "$agent2Uri"
          }
        ]
      }""")
  }

  "/master/api/agent/FOLDER/AGENT-A?return=Agent" in {
    testJson(
      httpClient.get[Json](master.localUri + "/master/api/agent/FOLDER/AGENT-A?return=Agent") await 99.s,
      json"""{
        "eventId": 1000005,
        "id": {
          "path": "/FOLDER/AGENT-A",
          "versionId": "(initial)"
        },
        "uri": "$agent2Uri"
      }""")
  }

  "/master/api/agent/FOLDER%2FAGENT-A?return=Agent" in {
    testJson(
      httpClient.get[Json](master.localUri + "/master/api/agent/FOLDER%2FAGENT-A?return=Agent") await 99.s,
      json"""{
        "eventId": 1000005,
        "id": {
          "path": "/FOLDER/AGENT-A",
          "versionId": "(initial)"
        },
        "uri": "$agent2Uri"
      }""")
  }

  "/master/api/agent-proxy/FOLDER%2FAGENT-A" in {
    // Pass-through Agent. Slashes but the first in AgentPath must be coded as %2F.
    val overview = httpClient.get[AgentOverview](master.localUri + "/master/api/agent-proxy/FOLDER%2FAGENT-A") await 99.s
    assert(overview.version == BuildInfo.buildVersion)
  }

  "/master/api/agent-proxy/UNKNOWN returns 400" in {
    assert(
      intercept[AkkaHttpClient.HttpException] {
        httpClient.get[Json](master.localUri + "/master/api/agent-proxy/UNKNOWN") await 99.s
      }.status == BadRequest)
  }

  "/master/api/agent-proxy/FOLDER%2F/AGENT-A/NOT-FOUND returns 404" in {
    assert(
      intercept[AkkaHttpClient.HttpException] {
        httpClient.get[Json](master.localUri + "/master/api/agent-proxy/FOLDER%2FAGENT-A/task") await 99.s
      }.status == NotFound)
  }

  "/master/api/agent-proxy/FOLDER%2F/AGENT-A/timer" in {
    assert(
      intercept[AkkaHttpClient.HttpException] {
        httpClient.get[Json](master.localUri + "/master/api/agent-proxy/FOLDER%2FAGENT-A/timer") await 99.s
      }.status == NotFound)
  }

  "/master/api/order" - {
    "POST order with missing workflow" in {
      val order = json"""{
        "id": "ORDER-ID",
        "workflowPath": "/MISSING"
      }"""
      val httpResponse = httpClient.post_(master.localUri + "/master/api/order", order) await 99.s
      assert(httpResponse.status == InternalServerError/*500*/)  // Or 403 Bad Request
      import httpClient.materializer
      assert(httpResponse.utf8StringFuture.await(99.s) contains "No such key 'Workflow:/MISSING'")  // Or similar
    }

    "POST" - {
      val order = json"""{
        "id": "ORDER-ID",
        "workflowPath": "/WORKFLOW"
      }"""

      "First" in {
        val response = httpClient.post_[Json](master.localUri + "/master/api/order", order) await 99.s
        assert(response.status == Created/*201*/)
        assert(response.header[Location] == Some(Location(master.localUri + "/master/api/order/ORDER-ID")))
      }

      "Duplicate" in {
        val response = httpClient.post_[Json](master.localUri + "/master/api/order", order) await 99.s
        assert(response.status == Conflict/*409*/)
        assert(response.header[Location] == Some(Location(master.localUri + "/master/api/order/ORDER-ID")))
      }
    }

    "GET" in {
      val order = httpClient.get[Json](master.localUri + "/master/api/order/ORDER-ID") await 99.s
      assert(order.fieldOrThrow("id") == Json.fromString("ORDER-ID"))
    }
  }

  "/master/api/event" in {
    eventCollector.await[OrderFinished]()
    val events = httpClient.get[Json](master.localUri + "/master/api/event?after=1000000") await 99.s
    // Fields named "eventId" are renumbered for this test, "timestamp" are removed due to time-dependant values
    assert(manipulateForTest(events) == json"""{
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
          "TYPE": "MasterReady"
        }, {
          "eventId": 1006,
          "TYPE": "OrderAdded",
          "key": "ORDER-ID",
          "workflowId": {
            "path": "/WORKFLOW",
            "versionId": "(initial)"
          }
        }, {
          "eventId": 1007,
          "TYPE": "OrderTransferredToAgent",
          "key": "ORDER-ID",
          "agentId": {
            "path": "/AGENT",
            "versionId": "(initial)"
          }
        }, {
          "eventId": 1008,
          "TYPE": "OrderProcessingStarted",
          "key": "ORDER-ID"
        }, {
          "eventId": 1009,
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
          "eventId": 1010,
          "TYPE": "OrderMoved",
          "key": "ORDER-ID",
          "to": [ 1 ]
        }, {
          "eventId": 1011,
          "TYPE": "OrderDetachable",
          "key": "ORDER-ID"
        }, {
          "eventId": 1012,
          "TYPE": "OrderTransferredToMaster",
          "key": "ORDER-ID"
        }, {
          "eventId": 1013,
          "TYPE": "OrderFinished",
          "key": "ORDER-ID"
        }
      ]
    }""")
  }

  private def manipulateForTest(eventResponse: Json): Json = {
    val eventIds = Iterator.from(1001)
    def changeEvent(json: Json): Json = {
      val obj = json.asObject.get
      //(obj("TYPE") != Some(Json.fromString("AgentEventIdEvent"))) ?
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
