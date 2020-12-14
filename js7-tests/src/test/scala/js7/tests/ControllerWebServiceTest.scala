package js7.tests

import akka.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import akka.http.scaladsl.model.MediaTypes.{`application/json`, `text/plain`}
import akka.http.scaladsl.model.StatusCodes.{Forbidden, NotFound, OK}
import akka.http.scaladsl.model.headers.{Accept, Location, RawHeader}
import akka.http.scaladsl.model.{HttpEntity, HttpHeader, Uri => AkkaUri}
import com.google.inject.{AbstractModule, Provides}
import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import java.time.ZoneId
import javax.inject.Singleton
import js7.base.BuildInfo
import js7.base.auth.SessionToken
import js7.base.circeutils.CirceUtils._
import js7.base.crypt.silly.{SillySignature, SillySigner}
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.Uri
import js7.common.event.EventIdClock
import js7.common.http.AkkaHttpClient.HttpException
import js7.common.http.AkkaHttpUtils.RichHttpResponse
import js7.common.http.CirceToYaml.yamlToJson
import js7.common.process.Processes.{ShellFileExtension => sh}
import js7.common.scalautil.FileUtils.syntax.RichPath
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.system.ServerOperatingSystem.operatingSystem
import js7.common.time.WaitForCondition
import js7.controller.data.events.AgentRefStateEvent
import js7.controller.data.events.AgentRefStateEvent.AgentRegisteredController
import js7.controller.data.events.ControllerEvent.ControllerReady
import js7.controller.data.{ControllerMetaState, ControllerState}
import js7.data.agent.AgentId
import js7.data.event.{<-:, Event, KeyedEvent}
import js7.data.job.RelativeExecutablePath
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.test.TestSetting.TestAgentId
import js7.tester.CirceJsonTester.testJson
import js7.tests.ControllerWebServiceTest._
import js7.tests.testenv.{ControllerAgentForScalaTest, DirectoryProvider}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
final class ControllerWebServiceTest extends AnyFreeSpec with BeforeAndAfterAll with ControllerAgentForScalaTest
{
  override lazy val signer = new SillySigner(SillySignature("MY-SILLY-SIGNATURE"))
  override lazy val verifier = signer.toVerifier

  private val testStartedAt = Timestamp.now - 24.h

  private lazy val uri = controller.localUri

  protected val agentIds = TestAgentId :: AgentId("AGENT-A") :: Nil
  protected val versionedItems = Nil
  private lazy val agent1Uri = directoryProvider.agents(0).localUri
  private lazy val agent2Uri = directoryProvider.agents(1).localUri
  private lazy val httpClient = new SimpleAkkaHttpClient(label = "ControllerWebServiceTest", uri, "/controller")
    .closeWithCloser

  private var sessionToken: String = "INVALID"

  private implicit def implicitSessionToken = Task(Some(SessionToken(SecretString(sessionToken))))

  override val controllerModule = new AbstractModule {
    @Provides @Singleton def eventIdClock(): EventIdClock = new EventIdClock.Fixed(1000)
  }

  private implicit def materializer = httpClient.materializer

  override def beforeAll() = {
    directoryProvider.controller.configDir / "controller.conf" ++=
      """js7.journal.sync = off
         js7.journal.delay = 0s
        |""".stripMargin
    writeControllerConfiguration(directoryProvider.controller)
    writeAgentConfiguration(directoryProvider.agents(0))
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

  "Await ControllerReady" in {
    controller.waitUntilReady()
  }

  "Await AgentReady" in {
    // Proceed first after all AgentReady have been received, to get an event sequence as expected
    for (agentId <- agentIds) {
      controller.eventWatch.await[AgentRefStateEvent.AgentReady](predicate = _.key == agentId)
    }
  }

  "/controller/api" in {
    val overview = httpClient.get[Json](Uri(s"$uri/controller/api")) await 99.s
    assert(overview.fieldOrThrow("version").stringOrThrow == BuildInfo.prettyVersion)
    WaitForCondition.waitForCondition(9.s, 10.ms) { Try(overview.fieldOrThrow("startedAt")).isSuccess }
    assert(overview.fieldOrThrow("startedAt").longOrThrow >= testStartedAt.toEpochMilli)
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
    val response = httpClient.post[Json, JsonObject](Uri(s"$uri/controller/api/session"), cmd) await 99.s

    /** Returns a structure like
      * {
      *   "TYPE": "LoggedIn",
      *   "sessionToken": "..."
      * }
      */
    assert(response("TYPE").get == "LoggedIn".asJson)
    sessionToken = response("sessionToken").get.asString.get
  }

  "Add workflows" in {
    val workflowJson = json"""
      {
        "TYPE": "Workflow",
        "path": "/WORKFLOW",
        "versionId": "VERSION-1",
        "instructions": [
          {
            "TYPE": "Execute.Anonymous",
            "job": {
              "agentId": "AGENT",
              "executable": {
                "TYPE": "ExecutablePath",
                "path": "A$sh"
              },
              "taskLimit": 1
            }
          }
        ]
      }""".compactPrint

    val workflow2Json = json"""
      {
        "TYPE": "Workflow",
        "path": "/FOLDER/WORKFLOW-2",
        "versionId": "VERSION-1",
        "instructions": [
          {
            "TYPE": "Execute.Anonymous",
            "job": {
              "agentId": "AGENT",
              "executable": {
                "TYPE": "ExecutablePath",
                "path": "B$sh"
              },
              "taskLimit": 1
            }
          }, {
            "TYPE": "Execute.Anonymous",
            "job": {
              "agentId": "AGENT",
              "executable": {
                "TYPE": "ExecutablePath",
                "path": "MISSING$sh"
              },
              "taskLimit": 1
            }
          }
        ]
      }""".compactPrint

    val cmd = json"""
      {
        "TYPE": "UpdateRepo",
        "versionId": "VERSION-1",
        "change": [
          {
            "string": "$workflowJson",
            "signature": {
              "TYPE": "Silly",
              "signatureString": "MY-SILLY-SIGNATURE"
            }
          }, {
            "string": "$workflow2Json",
            "signature": {
              "TYPE": "Silly",
              "signatureString": "MY-SILLY-SIGNATURE"
            }
          }
        ],
        "delete": []
      }"""

    val headers = RawHeader("X-JS7-Session", sessionToken) :: Nil
    val response = httpClient.postWithHeaders[Json, Json](Uri(s"$uri/controller/api/command"), cmd, headers) await 99.s
    assert(response == json"""
      {
        "TYPE": "Accepted"
      }""")
  }

  "/controller/api/workflow" - {
    testGet("controller/api/workflow",
      RawHeader("X-JS7-Session", sessionToken) :: Nil,
      json"""{
        "count": 2
      }""")

    testGet("controller/api/workflow/",
      RawHeader("X-JS7-Session", sessionToken) :: Nil,
      json"""[
        "/FOLDER/WORKFLOW-2",
        "/WORKFLOW"
      ]""")

    testGets("controller/api/workflow/FOLDER/WORKFLOW-2"::
             "controller/api/workflow//FOLDER/WORKFLOW-2"::
             "controller/api/workflow/%2FFOLDER%2FWORKFLOW-2":: Nil,
      RawHeader("X-JS7-Session", sessionToken) :: Nil,
      json"""{
        "path": "/FOLDER/WORKFLOW-2",
        "versionId": "VERSION-1",
        "instructions": [
          {
            "TYPE": "Execute.Anonymous",
            "job": {
              "agentId": "AGENT",
              "executable": {
               "TYPE": "ExecutablePath",
               "path": "B$sh"
             },
              "taskLimit": 1
            }
          }, {
            "TYPE": "Execute.Anonymous",
            "job": {
              "agentId": "AGENT",
              "executable": {
               "TYPE": "ExecutablePath",
               "path": "MISSING$sh"
             },
              "taskLimit": 1
            }
          }
        ]
      }""")
  }

  "/controller/api/agent" - {
    //testGet("controller/api/agent",
    //  RawHeader("X-JS7-Session", sessionToken) :: Nil,
    //  json"""{
    //    "count": 2
    //  }""")

    testGet("controller/api/agent/",
      RawHeader("X-JS7-Session", sessionToken) :: Nil,
      json"""[
        "AGENT",
        "AGENT-A"
      ]""")

    testGet("controller/api/agent/?return=AgentRef",
      RawHeader("X-JS7-Session", sessionToken) :: Nil,
      json"""[
          {
            "id": "AGENT",
            "uri": "$agent1Uri"
          }, {
            "id": "AGENT-A",
            "uri": "$agent2Uri"
          }
        ]""")

    testGet("controller/api/agent/AGENT-A?return=AgentRef",
      RawHeader("X-JS7-Session", sessionToken) :: Nil,
      json"""{
        "id": "AGENT-A",
        "uri": "$agent2Uri"
      }""")
  }

  "/controller/api/agent-proxy" - {
    //"/controller/api/agent-proxy/%2FFOLDER%2FAGENT-A" in {
    //  // Pass-through AgentRef. Slashes but the first in AgentId must be coded as %2F.
    //  val headers = RawHeader("X-JS7-Session", sessionToken) :: Nil
    //  val overview = httpClient.get[AgentOverview](Uri(s"$uri/controller/api/agent-proxy/FOLDER%2FAGENT-A"), Duration.Inf, headers) await 99.s
    //  assert(overview.version == BuildInfo.prettyVersion)
    //}

    "/controller/api/agent-proxy/UNKNOWN returns 400" in {
      val headers = RawHeader("X-JS7-Session", sessionToken) :: Nil
      val e = intercept[HttpException] {
        httpClient.get[Json](Uri(s"$uri/controller/api/agent-proxy/UNKNOWN"), headers) await 99.s
      }
      assert(e.status.intValue == 400/*BadRequest*/)
      assert(e.problem == Some(UnknownKeyProblem("AgentId", AgentId("UNKNOWN"))))
    }

    //"/controller/api/agent-proxy/FOLDER%2FAGENT-A/NOT-FOUND returns 404" in {
    //  val headers = RawHeader("X-JS7-Session", sessionToken) :: Nil
    //  assert(
    //    intercept[HttpException] {
    //      httpClient.get[Json](Uri(s"$uri/controller/api/agent-proxy/FOLDER%2FAGENT-A/task"), headers) await 99.s
    //    }.status.intValue == 404/*NotFound*/)
    //}
    //
    //"/controller/api/agent-proxy/FOLDER%2FAGENT-A/timer" in {
    //  val headers = RawHeader("X-JS7-Session", sessionToken) :: Nil
    //  assert(
    //    intercept[HttpException] {
    //      httpClient.get[Json](Uri(s"$uri/controller/api/agent-proxy/FOLDER%2FAGENT-A/timer"), headers) await 99.s
    //    }.status.intValue == 404/*NotFound*/)
    //}
  }

  "/controller/api/order" - {
    "POST" - {
      val orderWithMissingWorkflow = json"""{
        "id": "ORDER-ID",
        "workflowPath": "/MISSING"
      }"""

      "Order with missing workflow is rejected (single order)" in {
        val headers = RawHeader("X-JS7-Session", sessionToken) :: Nil
        val exception = intercept[HttpException] {
          httpClient.postWithHeaders[Json, Json](Uri(s"$uri/controller/api/order"), orderWithMissingWorkflow, headers) await 99.s
        }
        assert(exception.status.intValue == 400/*BadRequest*/)
        assert(exception.dataAsString contains "No such ItemPath: Workflow:/MISSING")  // Or similar
        assert(exception.problem == Some(UnknownKeyProblem("ItemPath", WorkflowPath("/MISSING"))))
      }

      "Order with missing workflow is rejected (order array)" in {
        val headers = RawHeader("X-JS7-Session", sessionToken) :: Nil
        val orders = Json.fromValues(orderWithMissingWorkflow :: Nil)
        val exception = intercept[HttpException] {
          httpClient.postWithHeaders[Json, Json](Uri(s"$uri/controller/api/order"), orders, headers) await 99.s
        }
        assert(exception.status.intValue == 400/*BadRequest*/)
        assert(exception.dataAsString contains "No such ItemPath: Workflow:/MISSING")  // Or similar
        assert(exception.problem == Some(UnknownKeyProblem("ItemPath", WorkflowPath("/MISSING"))))
      }

      "Invalid OrderId is rejected (single order)" in {
        val headers = RawHeader("X-JS7-Session", sessionToken) :: Nil
        val order = json"""{ "id": "ORDER|ID", "workflowPath": "/MISSING" }"""
        val exception = intercept[HttpException] {
          httpClient.postWithHeaders[Json, Json](Uri(s"$uri/controller/api/order"), order, headers) await 99.s
        }
        assert(exception.status.intValue == 400/*BadRequest*/)
        assert(exception.dataAsString contains "OrderId must not contain reserved characters |")
        assert(exception.problem == Some(Problem("JSON DecodingFailure at : OrderId must not contain reserved characters |")))
      }

      "Invalid OrderId is rejected (order array)" in {
        val headers = RawHeader("X-JS7-Session", sessionToken) :: Nil
        val orders = Json.fromValues(json"""{ "id": "ORDER|ID", "workflowPath": "/MISSING" }""":: Nil)
        val exception = intercept[HttpException] {
          httpClient.postWithHeaders[Json, Json](Uri(s"$uri/controller/api/order"), orders, headers) await 99.s
        }
        assert(exception.status.intValue == 400/*BadRequest*/)
        assert(exception.dataAsString contains "OrderId must not contain reserved characters |")
        assert(exception.problem == Some(Problem("JSON DecodingFailure at [0]: OrderId must not contain reserved characters |")))
      }

      val order = json"""{
        "id": "ORDER-ID",
        "workflowPath": "/WORKFLOW"
      }"""

      "First" in {
        val headers = RawHeader("X-JS7-Session", sessionToken) :: Accept(`application/json`) :: Nil
        val response = httpClient.post_[Json](Uri(s"$uri/controller/api/order"), order, headers) await 99.s
        assert(response.status.intValue == 201/*Created*/)
        assert(response.header[Location] == Some(Location(AkkaUri(s"$uri/controller/api/order/ORDER-ID"))))
        response.entity.discardBytes()
      }

      "Duplicate" in {
        val headers = RawHeader("X-JS7-Session", sessionToken) :: Accept(`application/json`) :: Nil
        val response = httpClient.post_[Json](Uri(s"$uri/controller/api/order"), order, headers) await 99.s
        assert(response.status.intValue == 409/*Conflict*/)
        assert(response.header[Location] == Some(Location(AkkaUri(s"$uri/controller/api/order/ORDER-ID"))))
        response.entity.discardBytes()
      }

      "Bad OrderId" in {
        val order = json"""{
          "id": "A|B",
          "workflowPath": "/WORKFLOW"
        }"""

        val headers = RawHeader("X-JS7-Session", sessionToken) :: Accept(`application/json`) :: Nil
        val response = httpClient.post_[Json](Uri(s"$uri/controller/api/order"), order, headers) await 99.s
        assert(response.status.intValue == 400/*BadRequest*/)
        assert(response.utf8StringFuture.await(99.seconds).parseJsonCheckedAs[Problem]
          == Right(Problem("JSON DecodingFailure at : OrderId must not contain reserved characters |")))
        assert(response.header[Location].isEmpty)
      }

      "Multiple" in {
        val orders = json"""
          [
            {
              "id": "ORDER-ID",
              "workflowPath": "/WORKFLOW"
            }
          ]"""
        val headers = RawHeader("X-JS7-Session", sessionToken) :: Accept(`application/json`) :: Nil
        val response = httpClient.post_[Json](Uri(s"$uri/controller/api/order"), orders, headers) await 99.s
        assert(response.status == OK)  // Duplicates are silently ignored
        assert(response.header[Location].isEmpty)
        response.entity.discardBytes()
      }

      "Multiple with bad OrderId" in {
        val orders = json"""
          [
            {
              "id": "A|B",
              "workflowPath": "/WORKFLOW"
            }
          ]"""
        val headers = RawHeader("X-JS7-Session", sessionToken) :: Accept(`application/json`) :: Nil
        val response = httpClient.post_[Json](Uri(s"$uri/controller/api/order"), orders, headers) await 99.s
        assert(response.status.intValue == 400/*BadRequest*/)
        assert(response.header[Location].isEmpty)
        assert(response.utf8StringFuture.await(99.seconds).parseJsonCheckedAs[Problem]
          == Right(Problem("JSON DecodingFailure at [0]: OrderId must not contain reserved characters |")))
      }
    }

    testGet("controller/api/order",
      RawHeader("X-JS7-Session", sessionToken) :: Nil,
      json"""{
        "count": 1
      }""")

    testGet("controller/api/order/",
      RawHeader("X-JS7-Session", sessionToken) :: Nil,
      json"""[ "ORDER-ID" ]""")

    "controller/api/order/?return=Order" in {
      val headers = RawHeader("X-JS7-Session", sessionToken) :: Accept(`application/json`) :: Nil
      val response = httpClient.get[Json](Uri(s"$uri/controller/api/order/?return=Order"), headers) await 99.s
      val orders = response.asArray.get
      assert(orders.length == 1)
      assert(orders(0).fieldOrThrow("id").stringOrThrow == "ORDER-ID")
    }

    "controller/api/order/ORDER-ID" in {
      val headers = RawHeader("X-JS7-Session", sessionToken) :: Nil
      val order = httpClient.get[Json](Uri(s"$uri/controller/api/order/ORDER-ID"), headers) await 99.s
      assert(order.fieldOrThrow("id") == Json.fromString("ORDER-ID"))  // May fail when OrderFinished
    }
  }

  "(await OrderFinished)" in {
    controller.eventWatch.await[OrderFinished]()  // Needed for test /controller/api/events
  }

  "/controller/api/snapshot/ (only JSON)" in {
    val observable = httpClient.getRawLinesObservable(Uri(s"$uri/controller/api/snapshot/")) await 99.s
    val shortenedArray =
      observable.map(_.parseJson.orThrow)
        // Delete AgentRefState in `array` (for easy comparison)
        .filterNot(_.asObject.get("TYPE").contains("AgentRefState".asJson))
        .toListL await 99.s
    val controllerMetaState = shortenedArray.iterator.map(_.as(ControllerState.snapshotObjectJsonCodec).orThrow)
      .collectFirst { case o: ControllerMetaState => o }.get
    assert(shortenedArray.toSet/*ignore ordering*/ == json"""[
      {
        "TYPE": "SnapshotEventId",
        "eventId": ${controller.eventWatch.lastAddedEventId}
      }, {
        "TYPE": "ControllerMetaState",
        "controllerId": "Controller",
        "startedAt": ${controllerMetaState.startedAt.toEpochMilli},
        "timezone": "${controllerMetaState.timezone}"
      }, {
        "TYPE": "VersionAdded",
        "versionId": "VERSION-1"
      }, {
        "TYPE": "ItemAdded",
        "path": "Workflow:/FOLDER/WORKFLOW-2",
        "signed": {
          "string": "{\"TYPE\":\"Workflow\",\"path\":\"/FOLDER/WORKFLOW-2\",\"versionId\":\"VERSION-1\",\"instructions\":[{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentId\":\"AGENT\",\"executable\":{\"TYPE\":\"ExecutablePath\",\"path\":\"B$sh\"},\"taskLimit\":1}},{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentId\":\"AGENT\",\"executable\":{\"TYPE\":\"ExecutablePath\",\"path\":\"MISSING$sh\"},\"taskLimit\":1}}]}",
          "signature": {
            "TYPE": "Silly",
            "signatureString": "MY-SILLY-SIGNATURE"
          }
        }
      }, {
        "TYPE": "ItemAdded",
        "path": "Workflow:/WORKFLOW",
        "signed": {
          "string": "{\"TYPE\":\"Workflow\",\"path\":\"/WORKFLOW\",\"versionId\":\"VERSION-1\",\"instructions\":[{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentId\":\"AGENT\",\"executable\":{\"TYPE\":\"ExecutablePath\",\"path\":\"A$sh\"},\"taskLimit\":1}}]}",
          "signature": {
            "TYPE": "Silly",
            "signatureString": "MY-SILLY-SIGNATURE"
          }
        }
      }, {
        "TYPE": "Order",
        "id": "ORDER-ID",
        "workflowPosition": {
          "workflowId": {
            "path": "/WORKFLOW",
            "versionId": "VERSION-1"
          },
          "position": [ 1 ]
        },
        "state": {
          "TYPE": "Finished"
        },
        "historicOutcomes": [
          {
            "position": [ 0 ],
            "outcome": {
              "TYPE": "Succeeded",
              "namedValues": {
                "returnCode": 0
              }
            }
          }
        ]
      }
    ]""".asArray.get.toSet)   // Any orders would be added to `array`.
  }

  "/controller/api/event (only JSON)" in {
    val headers = RawHeader("X-JS7-Session", sessionToken) :: Nil
    val eventsJson = httpClient.get[Json](Uri(s"$uri/controller/api/event?after=0"), headers) await 99.s
    val keyedEvents: Seq[KeyedEvent[Event]] =
      eventsJson.asObject.get("stamped").get.asArray.get.map(_.as(ControllerState.keyedEventJsonCodec).orThrow)
    val agentRunId = keyedEvents.collectFirst { case AgentId("AGENT") <-: (e: AgentRegisteredController) => e.agentRunId }.get
    val totalRunningTime = keyedEvents.collectFirst { case _ <-: (e: ControllerReady) => e.totalRunningTime }.get
    // Fields named "eventId" are renumbered for this test, "timestamp" are removed due to time-dependant values
    assert(manipulateEventsForTest(eventsJson) == json"""{
      "TYPE": "NonEmpty",
      "stamped": [
        {
          "eventId": 1001,
          "TYPE": "SnapshotTaken"
        }, {
          "eventId": 1002,
          "TYPE": "ControllerInitialized",
          "controllerId": "Controller",
          "startedAt": 111222333
        }, {
          "eventId": 1003,
          "TYPE": "ControllerReady",
          "timezone": "${ZoneId.systemDefault.getId}",
          "totalRunningTime": ${totalRunningTime.toBigDecimalSeconds}
        }, {
          "eventId": 1004,
          "TYPE": "SimpleItemAdded",
          "item": {
            "TYPE": "AgentRef",
            "id": "AGENT",
            "uri": "$agent1Uri"
          }
        }, {
          "eventId": 1005,
          "TYPE": "SimpleItemAdded",
          "item": {
            "TYPE": "AgentRef",
            "id": "AGENT-A",
            "uri": "$agent2Uri"
          }
        }, {
          "eventId": 1006,
          "key": "AGENT",
          "TYPE": "AgentRegisteredController",
          "agentRunId": "${agentRunId.string}"
        }, {
          "eventId": 1007,
          "TYPE": "AgentReady",
          "key": "AGENT",
          "timezone": "${ZoneId.systemDefault.getId}"
        }, {
          "eventId": 1008,
          "TYPE": "VersionAdded",
          "versionId": "VERSION-1"
        }, {
          "eventId": 1009,
          "TYPE": "ItemAdded",
          "path": "Workflow:/WORKFLOW",
          "signed": {
            "string": "{\"TYPE\":\"Workflow\",\"path\":\"/WORKFLOW\",\"versionId\":\"VERSION-1\",\"instructions\":[{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentId\":\"AGENT\",\"executable\":{\"TYPE\":\"ExecutablePath\",\"path\":\"A$sh\"},\"taskLimit\":1}}]}",
            "signature": {
              "TYPE": "Silly",
              "signatureString": "MY-SILLY-SIGNATURE"
            }
          }
        }, {
          "eventId": 1010,
          "TYPE": "ItemAdded",
          "path": "Workflow:/FOLDER/WORKFLOW-2",
          "signed": {
            "string": "{\"TYPE\":\"Workflow\",\"path\":\"/FOLDER/WORKFLOW-2\",\"versionId\":\"VERSION-1\",\"instructions\":[{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentId\":\"AGENT\",\"executable\":{\"TYPE\":\"ExecutablePath\",\"path\":\"B$sh\"},\"taskLimit\":1}},{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentId\":\"AGENT\",\"executable\":{\"TYPE\":\"ExecutablePath\",\"path\":\"MISSING$sh\"},\"taskLimit\":1}}]}",
            "signature": {
              "TYPE": "Silly",
              "signatureString": "MY-SILLY-SIGNATURE"
            }
          }
        }, {
          "eventId": 1011,
          "TYPE": "OrderAdded",
          "key": "ORDER-ID",
          "workflowId": {
            "path": "/WORKFLOW",
            "versionId": "VERSION-1"
          }
        }, {
          "eventId": 1012,
          "TYPE": "OrderAttachable",
          "key": "ORDER-ID",
          "agentId":"AGENT"
        }, {
          "eventId": 1013,
          "TYPE": "OrderAttached",
          "key": "ORDER-ID",
          "agentId": "AGENT"
        }, {
          "eventId": 1014,
          "TYPE": "OrderStarted",
          "key": "ORDER-ID"
        }, {
          "eventId": 1015,
          "TYPE": "OrderProcessingStarted",
          "key": "ORDER-ID"
        }, {
          "eventId": 1016,
          "TYPE": "OrderProcessed",
          "key": "ORDER-ID",
          "outcome": {
            "TYPE": "Succeeded",
            "namedValues": {
              "returnCode": 0
            }
          }
        }, {
          "eventId": 1017,
          "TYPE": "OrderMoved",
          "key": "ORDER-ID",
          "to": [ 1 ]
        }, {
          "eventId": 1018,
          "TYPE": "OrderDetachable",
          "key": "ORDER-ID"
        }, {
          "eventId": 1019,
          "TYPE": "OrderDetached",
          "key": "ORDER-ID"
        }, {
          "eventId": 1020,
          "TYPE": "OrderFinished",
          "key": "ORDER-ID"
        }
      ]
    }""")

    def manipulateEventsForTest(eventResponse: Json): Json = {
      def ignoreIt(json: Json): Boolean = {
        val obj = json.asObject.get.toMap
        (obj("TYPE") == Json.fromString("AgentReady") || obj("TYPE") == Json.fromString("AgentRegisteredController")) &&
            json.as[KeyedEvent[AgentRefStateEvent]].orThrow.key != TestAgentId || // Let through only Events for one AgentRef, because ordering is undefined
          obj("TYPE") == Json.fromString("AgentCouplingFailed") ||
          obj("TYPE") == Json.fromString("AgentEventsObserved")
      }
      val eventIds = Iterator.from(1001)
      def changeEvent(json: Json): Json = {
        val obj = json.asObject.get
        Json.fromJsonObject(JsonObject.fromMap(
          obj.toMap flatMap {
            case ("eventId", _) => ("eventId" -> Json.fromInt(eventIds.next())) :: Nil
            case ("startedAt", _) => ("startedAt" -> Json.fromLong(111222333)) :: Nil
            case ("timestamp", _) => Nil
            case o => o :: Nil
          }))
      }
      eventResponse.hcursor
        .downField("stamped").withFocus(_.mapArray(_ filterNot ignoreIt map changeEvent)).up
        .top.get
    }
  }

  "Unknown path returns 404 Not found" in {
    val response = httpClient.post_(Uri(s"$uri/controller/UNKNOWN"), Json.obj(), Nil) await 99.s
    assert(response.status == NotFound)
  }

  "CSRF with POST" - {
    lazy val testUri = Uri(s"$uri/controller/TEST/post")

    "application/json is allowed" in {
      val response = httpClient.postRaw(testUri, Nil, HttpEntity(`application/json`, "{}")) await 99.s
      assert(response.status == OK)
    }

    "text/plain like HTML form POST is forbidden" in {
      val forbidden = httpClient.postRaw(testUri, Nil, HttpEntity(`text/plain(UTF-8)`, "STRING")) await 99.s
      assert(forbidden.status == Forbidden)
      assert(forbidden.utf8StringFuture.await(99.s) == Forbidden.defaultMessage)
    }
  }

  "Commands" - {
    "(add order)" in {
      controller.addOrderBlocking(FreshOrder(OrderId("ORDER-FRESH"), WorkflowPath("/WORKFLOW"), Some(Timestamp.parse("3000-01-01T12:00:00Z"))))
    }

    "CancelOrders" in {
      val cmd = json"""
        {
          "TYPE": "Batch",
          "commands": [
            {
              "TYPE": "CancelOrders",
              "orderIds": [ "UNKNOWN" ]
            }, {
              "TYPE": "CancelOrders",
              "orderIds": [ "ORDER-FRESH" ]
            }
          ]
        }"""
      val headers = RawHeader("X-JS7-Session", sessionToken) :: Nil
      testJson(
        httpClient.postWithHeaders[Json, Json](Uri(s"$uri/controller/api/command"), cmd, headers) await 99.s,
        json"""{
          "TYPE": "BatchResponse",
          "responses": [
            {
              "TYPE": "Problem",
              "code": "UnknownOrder",
              "arguments": {
                "orderId": "UNKNOWN"
              },
              "message": "Unknown OrderId 'UNKNOWN'"
            }, {
              "TYPE": "Accepted"
            }
          ]
        }""")
    }

    "ShutDown" in {
      val cmd = json"""{ "TYPE": "ShutDown" }"""
      val headers = RawHeader("X-JS7-Session", sessionToken) :: Nil
      testJson(
        httpClient.postWithHeaders[Json, Json](Uri(s"$uri/controller/api/command"), cmd, headers) await 99.s,
        json"""{
          "TYPE": "Accepted"
        }""")
      controller.terminated await 99.s
    }
  }

  private def testGets(suburis: Iterable[String], headers: => List[HttpHeader], expected: => Json, manipulateResponse: Json => Json = identity): Unit =
    for (suburi <- suburis) testGet(suburi, headers, expected, manipulateResponse)

  private def testGet(suburi: String, headers: => List[HttpHeader], expected: => Json, manipulateResponse: Json => Json = identity): Unit =
    suburi - {
      "JSON" in {
        testJson(
          manipulateResponse(httpClient.get[Json](Uri(s"$uri/$suburi"), Duration.Inf, headers) await 99.s),
          expected)
      }

      "YAML" in {
        val yamlString = httpClient.get_[String](Uri(s"$uri/$suburi"), headers ::: Accept(`text/plain`) :: Nil) await 99.s
        assert(yamlString.head.isLetter || yamlString.head == '-')  // A YAML object starts with the first field id or an array entry
        testJson(manipulateResponse(yamlToJson(yamlString).orThrow), expected)
      }
    }
}

object ControllerWebServiceTest
{
  private def writeControllerConfiguration(controller: DirectoryProvider.ControllerTree): Unit = {
    controller.configDir / "controller.conf" ++= """
      |js7.web.server.test = true
      |""".stripMargin
    (controller.configDir / "private" / "private.conf").append("""
      |js7.auth.users {
      |  TEST-USER {
      |    password = "plain:TEST-PASSWORD",
      |    permissions = [ UpdateRepo ]
      |  }
      |}
      |""".stripMargin)
  }

  private def writeAgentConfiguration(agent: DirectoryProvider.AgentTree): Unit = {
    agent.writeExecutable(RelativeExecutablePath(s"A$sh"), operatingSystem.sleepingShellScript(1.second))  // Allow some time to check web service before order finishes
    agent.writeExecutable(RelativeExecutablePath(s"B$sh"), operatingSystem.sleepingShellScript(0.seconds))
  }
}
