package com.sos.jobscheduler.tests

import akka.http.scaladsl.model.StatusCodes.{BadRequest, InternalServerError, NotFound}
import akka.http.scaladsl.model.{HttpResponse, Uri}
import com.google.inject.{AbstractModule, Provides}
import com.sos.jobscheduler.agent.views.AgentOverview
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.event.EventIdClock
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.test.TestSetting.SimpleTestWorkflow
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.{Encoder, Json}
import javax.inject.Singleton
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
final class MasterWebServiceTest extends FreeSpec with BeforeAndAfterAll with DirectoryProvider.ForScalaTest {

  private val testStartedAt = System.currentTimeMillis - 24*3600*1000
  protected val agentPaths = AgentPath("/FOLDER/AGENT-A") :: Nil
  private lazy val agentUri = directoryProvider.agents(0).localUri.toString
  private lazy val httpClient = new SimpleAkkaHttpClient(label = "MasterWebServiceTest")

  override val masterModule = new AbstractModule {
    @Provides @Singleton def eventIdClock(): EventIdClock = new EventIdClock.Fixed(1000)
  }

  override def beforeAll(): Unit = {
    directoryProvider.master.writeJson(SimpleTestWorkflow.withoutVersion)
    super.beforeAll()
  }

  "/master/api" in {
    val overview = httpClient.get[Json](master.localUri + "/master/api") await 99.s
    assert(overview.fieldOrThrow("version").stringOrThrow == BuildInfo.buildVersion)
    assert(overview.fieldOrThrow("startedAt").longOrThrow >= testStartedAt)
    assert(overview.fieldOrThrow("startedAt").longOrThrow < Timestamp.parse("2100-01-01T00:00:00Z").toEpochMilli)
  }

  "/master/api/workflow" - {
    // TODO
  }

  "/master/api/agent" in {
    testJson(
      httpClient.get[Json](master.localUri + "/master/api/agent") await 99.s,
      json"""{
        "eventId": 1000004,
        "count": 1
      }""")
  }

  "/master/api/agent/" in {
    testJson(
      httpClient.get[Json](master.localUri + "/master/api/agent/") await 99.s,
      json"""{
        "eventId": 1000004,
        "value": [ "/FOLDER/AGENT-A" ]
      }""")
  }

  "/master/api/agent/?return=Agent" in {
    testJson(
      httpClient.get[Json](master.localUri + "/master/api/agent/?return=Agent") await 99.s,
      json"""{
        "eventId": 1000004,
        "value": [
          {
            "id": {
              "path": "/FOLDER/AGENT-A",
              "versionId": "(initial)"
            },
            "uri": "$agentUri"
          }
        ]
      }""")
  }

  "/master/api/agent/FOLDER/AGENT-A?return=Agent" in {
    testJson(
      httpClient.get[Json](master.localUri + "/master/api/agent/FOLDER/AGENT-A?return=Agent") await 99.s,
      json"""{
        "eventId": 1000004,
        "id": {
          "path": "/FOLDER/AGENT-A",
          "versionId": "(initial)"
        },
        "uri": "$agentUri"
      }""")
  }

  "/master/api/agent/FOLDER%2FAGENT-A?return=Agent" in {
    testJson(
      httpClient.get[Json](master.localUri + "/master/api/agent/FOLDER%2FAGENT-A?return=Agent") await 99.s,
      json"""{
        "eventId": 1000004,
        "id": {
          "path": "/FOLDER/AGENT-A",
          "versionId": "(initial)"
        },
        "uri": "$agentUri"
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
      val exception = postFailing(master.localUri + "/master/api/order", order)
      assert(exception.status == InternalServerError/*500*/)  // Or 403 Bad Request
      assert(exception.message contains "No such key 'Workflow:/MISSING'")  // Or similar
    }

    "POST" in {
      val order = json"""{
        "id": "ORDER-ID",
        "workflowPath": "/WORKFLOW"
      }"""
      httpClient.postIgnoreResponse(master.localUri + "/master/api/order", order) await 99.s
    }

    "GET" in {
      val order = httpClient.get[Json](master.localUri + "/master/api/order/ORDER-ID") await 99.s
      assert(order == json"""{
          "id": "ORDER-ID",
          "workflowPosition": {
            "workflowId": {
              "path": "/WORKFLOW",
              "versionId": "(initial)"
            },
            "position": [ 0 ]
          },
          "state": {
            "TYPE": "Fresh"
          },
          "payload": {
            "variables": {}
          }
        }""")
    }
  }

  "/master/api/event" in {
    val x = httpClient.get[Json](master.localUri + s"/master/api/event?after=1000000") await 99.s
    assert(x == json"""{
      "eventId": 1000005,
      "TYPE": "NonEmpty",
      "stampeds": [
        {
          "eventId": 1000001,
          "TYPE": "VersionAdded",
          "versionId": "(initial)"
        }, {
          "eventId": 1000002,
          "TYPE": "FileBasedAdded",
          "fileBased": {
            "TYPE": "Agent",
            "id": {
              "path": "/FOLDER/AGENT-A",
              "versionId": "(initial)"
            },
            "uri": "$agentUri"
          }
        },{
          "eventId": 1000003,
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
              }, {
                "TYPE": "Job",
                "jobPath": "/B",
                "agentPath": "/AGENT"
              }
            ]
          }
        }, {
          "eventId": 1000004,
          "TYPE": "MasterReady"
        }, {
          "eventId": 1000005,
          "key": "ORDER-ID",
          "TYPE": "OrderAdded",
          "workflowId": {
            "path": "/WORKFLOW",
            "versionId": "(initial)"
          }
        }
      ]
    }""")
  }

  private def postFailing[A: Encoder](uri: Uri, data: A): AkkaHttpClient.HttpException =
    Try { httpClient.post_[A, HttpResponse](uri, data) await 99.s }
      .failed.get.asInstanceOf[AkkaHttpClient.HttpException]
}
