package com.sos.jobscheduler.tests

import akka.http.scaladsl.model.StatusCodes.InternalServerError
import akka.http.scaladsl.model.{HttpResponse, Uri}
import com.google.inject.{AbstractModule, Provides}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.event.EventIdClock
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.workflow.test.TestSetting.SimpleTestWorkflow
import io.circe.{Encoder, Json}
import javax.inject.Singleton
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
final class MasterWebServiceTest extends FreeSpec with BeforeAndAfterAll with DirectoryProvider.ForScalaTest {

  private val testStartedAt = System.currentTimeMillis - 24*3600*1000
  protected val agentPaths = Nil
  private lazy val httpClient = new SimpleAkkaHttpClient(label = "MasterWebServiceTest")

  override val masterModule = new AbstractModule {
    def configure() = ()

    @Provides @Singleton def eventIdClock(): EventIdClock = new EventIdClock.Fixed(1000)
  }

  override def beforeAll(): Unit = {
    directoryProvider.master.writeJson(SimpleTestWorkflow.withoutVersion)
    super.beforeAll()
  }

  "/master/api" in {
    val overview = httpClient.get[Json](master.localUri + "/master/api") await 99.s
    assert(overview.forceField("version").forceString == BuildInfo.buildVersion)
    assert(overview.forceField("startedAt").forceLong >= testStartedAt)
    assert(overview.forceField("startedAt").forceLong < Timestamp.parse("2100-01-01T00:00:00Z").toEpochMilli)
  }

  "/master/api/order" - {
    "POST order with missing workflow" in {
      val order =json"""{
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
            "TYPE": "StartNow"
          },
          "payload": {
            "variables": {}
          }
        }""")
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
                }, {
                  "TYPE": "ImplicitEnd"
                }
              ]
            }
          }, {
            "eventId": 1000003,
            "TYPE": "MasterReady"
          }, {
            "eventId": 1000004,
            "key": "ORDER-ID",
            "TYPE": "OrderAdded",
            "workflowId": {
              "path": "/WORKFLOW",
              "versionId": "(initial)"
            },
            "state": {
              "TYPE": "StartNow"
            },
            "payload": {
              "variables": {}
            }
          }
        ]
      }""")
    }
  }

  private def postFailing[A: Encoder](uri: Uri, data: A): AkkaHttpClient.HttpException =
    Try { httpClient.post_[A, HttpResponse](uri, data) await 99.s }
      .failed.get.asInstanceOf[AkkaHttpClient.HttpException]
}
