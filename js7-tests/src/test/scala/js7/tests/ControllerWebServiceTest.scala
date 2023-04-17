package js7.tests

import akka.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.{Forbidden, NotFound, OK}
import akka.http.scaladsl.model.headers.{Accept, Location, RawHeader}
import akka.http.scaladsl.model.{HttpEntity, HttpHeader, Uri as AkkaUri}
import akka.stream.Materializer
import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import js7.agent.RunningAgent
import js7.base.auth.SessionToken
import js7.base.circeutils.CirceUtils.*
import js7.base.crypt.silly.{SillySignature, SillySigner}
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.process.Processes.ShellFileExtension as sh
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.{Timestamp, WaitForCondition}
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.base.web.Uri
import js7.base.{BuildInfo, Js7Version}
import js7.common.http.AkkaHttpClient.HttpException
import js7.common.http.AkkaHttpUtils.RichHttpResponse
import js7.common.system.ServerOperatingSystem.operatingSystem
import js7.controller.RunningController
import js7.data.Problems.UnknownItemPathProblem
import js7.data.agent.{AgentPath, AgentRefStateEvent}
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
import js7.tests.ControllerWebServiceTest.*
import js7.tests.testenv.{ControllerAgentForScalaTest, ControllerEnv, SubagentEnv}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable
import org.scalactic.source
import org.scalatest.BeforeAndAfterAll
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
final class ControllerWebServiceTest
extends OurTestSuite with BeforeAndAfterAll with ControllerAgentForScalaTest
{
  override lazy val signer: SillySigner = new SillySigner(SillySignature("MY-SILLY-SIGNATURE"))
  override lazy val verifier = signer.toVerifier

  private val testStartedAt = Timestamp.now - 24.h

  private lazy val uri = controller.localUri

  protected val agentPaths = TestAgentPath :: AgentPath("AGENT-A") :: Nil
  protected val items = Nil
  private lazy val agent1Uri = directoryProvider.agentEnvs(0).localUri
  private lazy val agent2Uri = directoryProvider.agentEnvs(1).localUri
  private lazy val httpClient = new SimpleAkkaHttpClient(label = "ControllerWebServiceTest", uri, "/controller")
    .closeWithCloser

  private var sessionToken: String = "INVALID"

  private implicit def implicitSessionToken: Task[Some[SessionToken]] =
    Task(Some(SessionToken(SecretString(sessionToken))))

  override protected def agentTestWiring = RunningAgent.TestWiring(
    eventIdClock = Some(EventIdClock.fixed(2000)))

  override protected def controllerTestWiring = RunningController.TestWiring(
    eventIdClock = Some(EventIdClock.fixed(1000)))

  private implicit def materializer: Materializer = httpClient.materializer

  override def beforeAll() = {
    directoryProvider.controllerEnv.configDir / "controller.conf" ++=
      """js7.journal.sync = off
         js7.journal.delay = 0s"""
    writeControllerConfiguration(directoryProvider.controllerEnv)
    writeAgentConfiguration(directoryProvider.agentEnvs(0))
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
        },
        "js7Version": "${Js7Version.string}"
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

  "/controller/api/agent-forward" - {
    //"/controller/api/agent-forward/%2FFOLDER%2FAGENT-A" in {
    //  // Pass-through AgentRef. Slashes but the first in AgentPath must be coded as %2F.
    //  val headers = RawHeader("x-js7-session", sessionToken) :: Nil
    //  val overview = httpClient.get[AgentOverview](Uri(s"$uri/controller/api/agent-forward/FOLDER%2FAGENT-A"), Duration.Inf, headers) await 99.s
    //  assert(overview.version == BuildInfo.prettyVersion)
    //}

    "/controller/api/agent-forward/UNKNOWN returns 400" in {
      val headers = RawHeader("x-js7-session", sessionToken) :: Nil
      val e = intercept[HttpException] {
        httpClient.get[Json](Uri(s"$uri/controller/api/agent-forward/UNKNOWN"), headers) await 99.s
      }
      assert(e.status.intValue == 400/*BadRequest*/)
      assert(e.problem == Some(UnknownKeyProblem("AgentPath", AgentPath("UNKNOWN"))))
    }

    //"/controller/api/agent-forward/FOLDER%2FAGENT-A/NOT-FOUND returns 404" in {
    //  val headers = RawHeader("x-js7-session", sessionToken) :: Nil
    //  assert(
    //    intercept[HttpException] {
    //      httpClient.get[Json](Uri(s"$uri/controller/api/agent-forward/FOLDER%2FAGENT-A/task"), headers) await 99.s
    //    }.status.intValue == 404/*NotFound*/)
    //}
    //
    //"/controller/api/agent-forward/FOLDER%2FAGENT-A/timer" in {
    //  val headers = RawHeader("x-js7-session", sessionToken) :: Nil
    //  assert(
    //    intercept[HttpException] {
    //      httpClient.get[Json](Uri(s"$uri/controller/api/agent-forward/FOLDER%2FAGENT-A/timer"), headers) await 99.s
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

    "controller/api/order/ORDER-ID" in {
      val headers = RawHeader("x-js7-session", sessionToken) :: Nil
      val order = httpClient.get[Json](Uri(s"$uri/controller/api/order/ORDER-ID"), headers) await 99.s
      assert(order.fieldOrThrow("id") == Json.fromString("ORDER-ID"))  // May fail when OrderFinished
    }
  }

  "(await OrderFinished)" in {
    controller.eventWatch.await[OrderFinished]()  // Needed for test /controller/api/events
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
              "message": "Unknown Order:UNKNOWN"
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

  private def testGet(
    suburi: String,
    headers: => List[HttpHeader],
    expected: => Json,
    manipulateResponse: Json => Json = identity)
    (implicit pos: source.Position)
  : Unit =
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
  private def writeControllerConfiguration(controller: ControllerEnv): Unit = {
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

  private def writeAgentConfiguration(agent: SubagentEnv): Unit = {
    agent.writeExecutable(RelativePathExecutable(s"A$sh"), operatingSystem.sleepingShellScript(1.s))  // Allow some time to check web service before order finishes
    agent.writeExecutable(RelativePathExecutable(s"B$sh"), operatingSystem.sleepingShellScript(0.s))
  }
}
