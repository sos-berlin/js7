package js7.tests

import akka.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import akka.http.scaladsl.model.MediaTypes.`application/json`
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
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.process.Processes.{ShellFileExtension => sh}
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.thread.Futures.implicits._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.{Timestamp, WaitForCondition}
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.Uri
import js7.common.http.AkkaHttpClient.HttpException
import js7.common.http.AkkaHttpUtils.RichHttpResponse
import js7.common.system.ServerOperatingSystem.operatingSystem
import js7.data.Problems.UnknownItemPathProblem
import js7.data.agent.AgentRefStateEvent.AgentDedicated
import js7.data.agent.{AgentPath, AgentRefStateEvent}
import js7.data.controller.ControllerEvent.ControllerReady
import js7.data.controller.{ControllerMetaState, ControllerState}
import js7.data.event.{<-:, Event, KeyedEvent}
import js7.data.item.{ItemOperation, VersionId}
import js7.data.job.{PathExecutable, RelativePathExecutable}
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.test.TestSetting.TestAgentPath
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.journal.EventIdClock
import js7.tester.CirceJsonTester.testJson
import js7.tests.ControllerWebServiceTest._
import js7.tests.testenv.{ControllerAgentForScalaTest, DirectoryProvider}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
final class ControllerWebServiceTest
extends AnyFreeSpec with BeforeAndAfterAll with ControllerAgentForScalaTest
{
  override lazy val signer = new SillySigner(SillySignature("MY-SILLY-SIGNATURE"))
  override lazy val verifier = signer.toVerifier

  private val testStartedAt = Timestamp.now - 24.h

  private lazy val uri = controller.localUri

  protected val agentPaths = TestAgentPath :: AgentPath("AGENT-A") :: Nil
  protected val items = Nil
  private lazy val agent1Uri = directoryProvider.agents(0).localUri
  private lazy val agent2Uri = directoryProvider.agents(1).localUri
  private lazy val httpClient = new SimpleAkkaHttpClient(label = "ControllerWebServiceTest", uri, "/controller")
    .closeWithCloser

  private var sessionToken: String = "INVALID"

  private implicit def implicitSessionToken = Task(Some(SessionToken(SecretString(sessionToken))))

  override val agentModule = new AbstractModule {
    @Provides @Singleton def eventIdClock(): EventIdClock = EventIdClock.fixed(2000)
  }

  override val controllerModule = new AbstractModule {
    @Provides @Singleton def eventIdClock(): EventIdClock = EventIdClock.fixed(1000)
  }

  private implicit def materializer = httpClient.materializer

  override def beforeAll() = {
    directoryProvider.controller.configDir / "controller.conf" ++=
      """js7.journal.sync = off
         js7.journal.delay = 0s"""
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
    for (agentPath <- agentPaths) {
      controller.eventWatch.await[AgentRefStateEvent.AgentReady](predicate = _.key == agentPath)
    }
  }

  "/controller/api" in {
    val overview = httpClient.get[Json](Uri(s"$uri/controller/api")) await 99.s
    assert(overview.fieldOrThrow("version").stringOrThrow == BuildInfo.prettyVersion)
    WaitForCondition.waitForCondition(9.s, 10.ms) { Try(overview.fieldOrThrow("initiallyStartedAt")).isSuccess }
    assert(overview.fieldOrThrow("initiallyStartedAt").longOrThrow >= testStartedAt.toEpochMilli)
    assert(overview.fieldOrThrow("initiallyStartedAt").longOrThrow < Timestamp.parse("2100-01-01T00:00:00Z").toEpochMilli)
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
    controller.updateItemsAsSystemUser(
      ItemOperation.AddVersion(VersionId("VERSION-1")) +:
        Observable(
          Workflow.of(WorkflowPath("WORKFLOW") ~ "VERSION-1",
            Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable(s"A$sh")))),
          Workflow.of(WorkflowPath("FOLDER/WORKFLOW-2") ~ "VERSION-1",
            Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable(s"B$sh"))),
            Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable(s"MISSING$sh"))))
        ).map(directoryProvider.itemSigner.toSignedString)
          .map(ItemOperation.AddOrChangeSigned.apply)
    ).await(99.s).orThrow
  }

  "/controller/api/workflow" - {
    testGet("controller/api/workflow",
      RawHeader("x-js7-session", sessionToken) :: Nil,
      json"""{
        "count": 2
      }""")

    testGet("controller/api/workflow/",
      RawHeader("x-js7-session", sessionToken) :: Nil,
      json"""[
        "FOLDER/WORKFLOW-2",
        "WORKFLOW"
      ]""")

    testGets("controller/api/workflow/FOLDER/WORKFLOW-2"::
             "controller/api/workflow/FOLDER%2FWORKFLOW-2":: Nil,
      RawHeader("x-js7-session", sessionToken) :: Nil,
      json"""{
        "path": "FOLDER/WORKFLOW-2",
        "versionId": "VERSION-1",
        "instructions": [
          {
            "TYPE": "Execute.Anonymous",
            "job": {
              "agentPath": "AGENT",
              "executable": {
               "TYPE": "PathExecutable",
               "path": "B$sh"
             },
              "parallelism": 1
            }
          }, {
            "TYPE": "Execute.Anonymous",
            "job": {
              "agentPath": "AGENT",
              "executable": {
               "TYPE": "PathExecutable",
               "path": "MISSING$sh"
             },
              "parallelism": 1
            }
          }
        ]
      }""")
  }

  "/controller/api/agent" - {
    //testGet("controller/api/agent",
    //  RawHeader("x-js7-session", sessionToken) :: Nil,
    //  json"""{
    //    "count": 2
    //  }""")

    testGet("controller/api/agent/",
      RawHeader("x-js7-session", sessionToken) :: Nil,
      json"""[
        "AGENT",
        "AGENT-A"
      ]""")

    testGet("controller/api/agent/?return=AgentRef",
      RawHeader("x-js7-session", sessionToken) :: Nil,
      json"""[
          {
            "path": "AGENT",
            "uri": "$agent1Uri",
            "itemRevision": 0
          }, {
            "path": "AGENT-A",
            "uri": "$agent2Uri",
            "itemRevision": 0
          }
        ]""")

    testGet("controller/api/agent/AGENT-A?return=AgentRef",
      RawHeader("x-js7-session", sessionToken) :: Nil,
      json"""{
        "path": "AGENT-A",
        "uri": "$agent2Uri",
        "itemRevision": 0
      }""")
  }

  "/controller/api/agent-proxy" - {
    //"/controller/api/agent-proxy/%2FFOLDER%2FAGENT-A" in {
    //  // Pass-through AgentRef. Slashes but the first in AgentPath must be coded as %2F.
    //  val headers = RawHeader("x-js7-session", sessionToken) :: Nil
    //  val overview = httpClient.get[AgentOverview](Uri(s"$uri/controller/api/agent-proxy/FOLDER%2FAGENT-A"), Duration.Inf, headers) await 99.s
    //  assert(overview.version == BuildInfo.prettyVersion)
    //}

    "/controller/api/agent-proxy/UNKNOWN returns 400" in {
      val headers = RawHeader("x-js7-session", sessionToken) :: Nil
      val e = intercept[HttpException] {
        httpClient.get[Json](Uri(s"$uri/controller/api/agent-proxy/UNKNOWN"), headers) await 99.s
      }
      assert(e.status.intValue == 400/*BadRequest*/)
      assert(e.problem == Some(UnknownKeyProblem("AgentPath", AgentPath("UNKNOWN"))))
    }

    //"/controller/api/agent-proxy/FOLDER%2FAGENT-A/NOT-FOUND returns 404" in {
    //  val headers = RawHeader("x-js7-session", sessionToken) :: Nil
    //  assert(
    //    intercept[HttpException] {
    //      httpClient.get[Json](Uri(s"$uri/controller/api/agent-proxy/FOLDER%2FAGENT-A/task"), headers) await 99.s
    //    }.status.intValue == 404/*NotFound*/)
    //}
    //
    //"/controller/api/agent-proxy/FOLDER%2FAGENT-A/timer" in {
    //  val headers = RawHeader("x-js7-session", sessionToken) :: Nil
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
        "workflowPath": "MISSING"
      }"""

      "Order with missing workflow is rejected (single order)" in {
        val headers = RawHeader("x-js7-session", sessionToken) :: Nil
        val exception = intercept[HttpException] {
          httpClient.postWithHeaders[Json, Json](Uri(s"$uri/controller/api/order"), orderWithMissingWorkflow, headers) await 99.s
        }
        assert(exception.status.intValue == 400/*BadRequest*/)
        assert(exception.dataAsString contains "Unknown item: Workflow:MISSING")  // Or similar
        assert(exception.problem == Some(UnknownItemPathProblem(WorkflowPath("MISSING"))))
      }

      "Order with missing workflow is rejected (order array)" in {
        val headers = RawHeader("x-js7-session", sessionToken) :: Nil
        val orders = Json.fromValues(orderWithMissingWorkflow :: Nil)
        val exception = intercept[HttpException] {
          httpClient.postWithHeaders[Json, Json](Uri(s"$uri/controller/api/order"), orders, headers) await 99.s
        }
        assert(exception.status.intValue == 400/*BadRequest*/)
        assert(exception.dataAsString contains "Unknown item: Workflow:MISSING")  // Or similar
        assert(exception.problem == Some(UnknownItemPathProblem(WorkflowPath("MISSING"))))
      }

      "Invalid OrderId is rejected (single order)" in {
        val headers = RawHeader("x-js7-session", sessionToken) :: Nil
        val order = json"""{ "id": "ORDER|ID", "workflowPath": "MISSING" }"""
        val exception = intercept[HttpException] {
          httpClient.postWithHeaders[Json, Json](Uri(s"$uri/controller/api/order"), order, headers) await 99.s
        }
        assert(exception.status.intValue == 400/*BadRequest*/)
        assert(exception.dataAsString contains "OrderId must not contain reserved characters: |")
        assert(exception.problem == Some(Problem("JSON DecodingFailure at : OrderId must not contain reserved characters: |")))
      }

      "Invalid OrderId is rejected (order array)" in {
        val headers = RawHeader("x-js7-session", sessionToken) :: Nil
        val orders = Json.fromValues(json"""{ "id": "ORDER|ID", "workflowPath": "MISSING" }""":: Nil)
        val exception = intercept[HttpException] {
          httpClient.postWithHeaders[Json, Json](Uri(s"$uri/controller/api/order"), orders, headers) await 99.s
        }
        assert(exception.status.intValue == 400/*BadRequest*/)
        assert(exception.dataAsString contains "OrderId must not contain reserved characters: |")
        assert(exception.problem == Some(Problem("JSON DecodingFailure at [0]: OrderId must not contain reserved characters: |")))
      }

      val order = json"""{
        "id": "ORDER-ID",
        "workflowPath": "WORKFLOW"
      }"""

      "First" in {
        val headers = RawHeader("x-js7-session", sessionToken) :: Accept(`application/json`) :: Nil
        val response = httpClient.post_[Json](Uri(s"$uri/controller/api/order"), order, headers) await 99.s
        assert(response.status.intValue == 201/*Created*/)
        assert(response.header[Location] == Some(Location(AkkaUri(s"$uri/controller/api/order/ORDER-ID"))))
        response.entity.discardBytes()
      }

      "Duplicate" in {
        val headers = RawHeader("x-js7-session", sessionToken) :: Accept(`application/json`) :: Nil
        val response = httpClient.post_[Json](Uri(s"$uri/controller/api/order"), order, headers) await 99.s
        assert(response.status.intValue == 409/*Conflict*/)
        assert(response.header[Location] == Some(Location(AkkaUri(s"$uri/controller/api/order/ORDER-ID"))))
        response.entity.discardBytes()
      }

      "Bad OrderId" in {
        val order = json"""{
          "id": "A|B",
          "workflowPath": "WORKFLOW"
        }"""

        val headers = RawHeader("x-js7-session", sessionToken) :: Accept(`application/json`) :: Nil
        val response = httpClient.post_[Json](Uri(s"$uri/controller/api/order"), order, headers) await 99.s
        assert(response.status.intValue == 400/*BadRequest*/)
        assert(response.utf8String.await(99.s).parseJsonAs[Problem]
          == Right(Problem("JSON DecodingFailure at : OrderId must not contain reserved characters: |")))
        assert(response.header[Location].isEmpty)
      }

      "Multiple" in {
        val orders = json"""
          [
            {
              "id": "ORDER-ID",
              "workflowPath": "WORKFLOW"
            }
          ]"""
        val headers = RawHeader("x-js7-session", sessionToken) :: Accept(`application/json`) :: Nil
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
              "workflowPath": "WORKFLOW"
            }
          ]"""
        val headers = RawHeader("x-js7-session", sessionToken) :: Accept(`application/json`) :: Nil
        val response = httpClient.post_[Json](Uri(s"$uri/controller/api/order"), orders, headers) await 99.s
        assert(response.status.intValue == 400/*BadRequest*/)
        assert(response.header[Location].isEmpty)
        assert(response.utf8String.await(99.s).parseJsonAs[Problem]
          == Right(Problem("JSON DecodingFailure at [0]: OrderId must not contain reserved characters: |")))
      }
    }

    testGet("controller/api/order",
      RawHeader("x-js7-session", sessionToken) :: Nil,
      json"""{
        "count": 1
      }""")

    testGet("controller/api/order/",
      RawHeader("x-js7-session", sessionToken) :: Nil,
      json"""[ "ORDER-ID" ]""")

    "controller/api/order/?return=Order" in {
      val headers = RawHeader("x-js7-session", sessionToken) :: Accept(`application/json`) :: Nil
      val response = httpClient.get[Json](Uri(s"$uri/controller/api/order/?return=Order"), headers) await 99.s
      val orders = response.asArray.get
      assert(orders.length == 1)
      assert(orders(0).fieldOrThrow("id").stringOrThrow == "ORDER-ID")
    }

    "controller/api/order/ORDER-ID" in {
      val headers = RawHeader("x-js7-session", sessionToken) :: Nil
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
        "initiallyStartedAt": ${controllerMetaState.initiallyStartedAt.toEpochMilli},
        "timezone": "${controllerMetaState.timezone}"
      }, {
        "TYPE": "VersionAdded",
        "versionId": "VERSION-1"
      }, {
        "TYPE": "VersionedItemAdded",
        "signed": {
          "string": "{\"TYPE\":\"Workflow\",\"path\":\"FOLDER/WORKFLOW-2\",\"versionId\":\"VERSION-1\",\"instructions\":[{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentPath\":\"AGENT\",\"executable\":{\"TYPE\":\"PathExecutable\",\"path\":\"B$sh\"},\"parallelism\":1}},{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentPath\":\"AGENT\",\"executable\":{\"TYPE\":\"PathExecutable\",\"path\":\"MISSING$sh\"},\"parallelism\":1}}]}",
          "signature": {
            "TYPE": "Silly",
            "signatureString": "MY-SILLY-SIGNATURE"
          }
        }
      }, {
        "TYPE": "VersionedItemAdded",
        "signed": {
          "string": "{\"TYPE\":\"Workflow\",\"path\":\"WORKFLOW\",\"versionId\":\"VERSION-1\",\"instructions\":[{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentPath\":\"AGENT\",\"executable\":{\"TYPE\":\"PathExecutable\",\"path\":\"A$sh\"},\"parallelism\":1}}]}",
          "signature": {
            "TYPE": "Silly",
            "signatureString": "MY-SILLY-SIGNATURE"
          }
        }
      }, {
        "TYPE" : "ItemAttached",
        "key": "Workflow:WORKFLOW~VERSION-1",
        "delegateId": "Agent:AGENT"
      },
      { "TYPE": "Order",
        "id": "ORDER-ID",
        "workflowPosition": {
          "workflowId": {
            "path": "WORKFLOW",
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
    val headers = RawHeader("x-js7-session", sessionToken) :: Nil
    val eventsJson = httpClient.get[Json](Uri(s"$uri/controller/api/event?after=0"), headers) await 99.s
    val keyedEvents: Seq[KeyedEvent[Event]] =
      eventsJson.asObject.get("stamped").get.asArray.get.map(_.as(ControllerState.keyedEventJsonCodec).orThrow)
    val agentRunId = keyedEvents.collectFirst { case AgentPath("AGENT") <-: (e: AgentDedicated) => e.agentRunId }.get
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
          "initiallyStartedAt": 111222333
        }, {
          "eventId": 1003,
          "TYPE": "ControllerReady",
          "timezone": "${ZoneId.systemDefault.getId}",
          "totalRunningTime": ${totalRunningTime.toBigDecimalSeconds}
        }, {
          "eventId": 1004,
          "TYPE": "UnsignedSimpleItemAdded",
          "item": {
            "TYPE": "AgentRef",
            "path": "AGENT",
            "uri": "$agent1Uri",
            "itemRevision": 0
          }
        }, {
          "eventId": 1005,
          "TYPE": "UnsignedSimpleItemAdded",
          "item": {
            "TYPE": "AgentRef",
            "path": "AGENT-A",
            "uri": "$agent2Uri",
            "itemRevision": 0
          }
        }, {
          "eventId": 1006,
          "TYPE": "AgentDedicated",
          "Key": "AGENT",
          "agentRunId": "${agentRunId.string}",
          "agentEventId": 2000001
        }, {
          "eventId": 1007,
          "TYPE": "AgentReady",
          "Key": "AGENT",
          "timezone": "${ZoneId.systemDefault.getId}"
        }, {
          "eventId": 1008,
          "TYPE": "VersionAdded",
          "versionId": "VERSION-1"
        }, {
          "eventId": 1009,
          "TYPE": "VersionedItemAdded",
          "signed": {
            "string": "{\"TYPE\":\"Workflow\",\"path\":\"WORKFLOW\",\"versionId\":\"VERSION-1\",\"instructions\":[{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentPath\":\"AGENT\",\"executable\":{\"TYPE\":\"PathExecutable\",\"path\":\"A$sh\"},\"parallelism\":1}}]}",
            "signature": {
              "TYPE": "Silly",
              "signatureString": "MY-SILLY-SIGNATURE"
            }
          }
        }, {
          "eventId": 1010,
          "TYPE": "VersionedItemAdded",
          "signed": {
            "string": "{\"TYPE\":\"Workflow\",\"path\":\"FOLDER/WORKFLOW-2\",\"versionId\":\"VERSION-1\",\"instructions\":[{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentPath\":\"AGENT\",\"executable\":{\"TYPE\":\"PathExecutable\",\"path\":\"B$sh\"},\"parallelism\":1}},{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentPath\":\"AGENT\",\"executable\":{\"TYPE\":\"PathExecutable\",\"path\":\"MISSING$sh\"},\"parallelism\":1}}]}",
            "signature": {
              "TYPE": "Silly",
              "signatureString": "MY-SILLY-SIGNATURE"
            }
          }
        }, {
          "eventId": 1011,
          "TYPE": "OrderAdded",
          "Key": "ORDER-ID",
          "workflowId": {
            "path": "WORKFLOW",
            "versionId": "VERSION-1"
          }
        }, {
          "eventId": 1012,
          "TYPE": "OrderAttachable",
          "Key": "ORDER-ID",
          "agentPath":"AGENT"
        }, {
          "eventId": 1013,
          "TYPE": "ItemAttached",
          "key": "Workflow:WORKFLOW~VERSION-1",
          "delegateId": "Agent:AGENT"
        }, {
          "eventId": 1014,
          "TYPE": "OrderAttached",
          "Key": "ORDER-ID",
          "agentPath": "AGENT"
        }, {
          "eventId": 1015,
          "TYPE": "OrderStarted",
          "Key": "ORDER-ID"
        }, {
          "eventId": 1016,
          "TYPE": "OrderProcessingStarted",
          "Key": "ORDER-ID"
        }, {
          "eventId": 1017,
          "TYPE": "OrderProcessed",
          "Key": "ORDER-ID",
          "outcome": {
            "TYPE": "Succeeded",
            "namedValues": {
              "returnCode": 0
            }
          }
        }, {
          "eventId": 1018,
          "TYPE": "OrderMoved",
          "Key": "ORDER-ID",
          "to": [ 1 ]
        }, {
          "eventId": 1019,
          "TYPE": "OrderDetachable",
          "Key": "ORDER-ID"
        }, {
          "eventId": 1020,
          "TYPE": "OrderDetached",
          "Key": "ORDER-ID"
        }, {
          "eventId": 1021,
          "TYPE": "OrderFinished",
          "Key": "ORDER-ID"
        }
      ]
    }""")

    def manipulateEventsForTest(eventResponse: Json): Json = {
      def ignoreIt(json: Json): Boolean = {
        val obj = json.asObject.get.toMap
        (obj("TYPE") == Json.fromString("AgentReady") || obj("TYPE") == Json.fromString("AgentDedicated")) &&
            json.as[KeyedEvent[AgentRefStateEvent]].orThrow.key != TestAgentPath || // Let through only Events for one AgentRef, because ordering is undefined
          obj("TYPE") == Json.fromString("AgentCoupled") ||
          obj("TYPE") == Json.fromString("AgentCouplingFailed") ||
          obj("TYPE") == Json.fromString("AgentEventsObserved")
      }
      val eventIds = Iterator.from(1001)
      def changeEvent(json: Json): Json = {
        val obj = json.asObject.get
        Json.fromJsonObject(JsonObject.fromMap(
          obj.toMap flatMap {
            case ("eventId", _) => ("eventId" -> Json.fromInt(eventIds.next())) :: Nil
            case ("initiallyStartedAt", _) => ("initiallyStartedAt" -> Json.fromLong(111222333)) :: Nil
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
      assert(forbidden.utf8String.await(99.s) == Forbidden.defaultMessage)
    }
  }

  "Commands" - {
    "(add order)" in {
      controller.addOrderBlocking(FreshOrder(OrderId("ORDER-FRESH"), WorkflowPath("WORKFLOW"),
        scheduledFor = Some(Timestamp.parse("3000-01-01T12:00:00Z"))))
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
      val headers = RawHeader("x-js7-session", sessionToken) :: Nil
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
      val headers = RawHeader("x-js7-session", sessionToken) :: Nil
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
          manipulateResponse(httpClient.get[Json](Uri(s"$uri/$suburi"), headers) await 99.s),
          expected)
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
      |    permissions = [ UpdateItem ]
      |  }
      |}
      |""".stripMargin)
  }

  private def writeAgentConfiguration(agent: DirectoryProvider.AgentTree): Unit = {
    agent.writeExecutable(RelativePathExecutable(s"A$sh"), operatingSystem.sleepingShellScript(1.s))  // Allow some time to check web service before order finishes
    agent.writeExecutable(RelativePathExecutable(s"B$sh"), operatingSystem.sleepingShellScript(0.s))
  }
}
