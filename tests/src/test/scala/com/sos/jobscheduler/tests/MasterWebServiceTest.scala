package com.sos.jobscheduler.tests

import akka.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import akka.http.scaladsl.model.MediaTypes.{`application/json`, `text/plain`}
import akka.http.scaladsl.model.StatusCodes.{Forbidden, NotFound, OK}
import akka.http.scaladsl.model.headers.{Accept, Location, RawHeader}
import akka.http.scaladsl.model.{HttpEntity, HttpHeader}
import com.google.inject.{AbstractModule, Provides}
import com.sos.jobscheduler.agent.data.views.AgentOverview
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowableEither
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.event.EventIdClock
import com.sos.jobscheduler.common.http.AkkaHttpClient.HttpException
import com.sos.jobscheduler.common.http.AkkaHttpUtils.RichHttpResponse
import com.sos.jobscheduler.common.http.CirceToYaml.yamlToJson
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension => sh}
import com.sos.jobscheduler.common.scalautil.Closer.ops.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits.RichPath
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.system.OperatingSystem.operatingSystem
import com.sos.jobscheduler.common.time.WaitForCondition
import com.sos.jobscheduler.core.crypt.silly.{SillySignature, SillySigner}
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{<-:, Event, EventId, KeyedEvent}
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.{OrderFinished, OrderMoved, OrderProcessed}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.test.TestSetting.TestAgentRefPath
import com.sos.jobscheduler.master.data.MasterSnapshots
import com.sos.jobscheduler.master.data.MasterSnapshots.MasterMetaState
import com.sos.jobscheduler.master.data.events.MasterAgentEvent
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.AgentRegisteredMaster
import com.sos.jobscheduler.master.data.events.MasterEvent.MasterReady
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import com.sos.jobscheduler.tests.MasterWebServiceTest._
import com.sos.jobscheduler.tests.testenv.{DirectoryProvider, MasterAgentForScalaTest}
import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import java.time.ZoneId
import javax.inject.Singleton
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.immutable
import scala.concurrent.duration._
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
final class MasterWebServiceTest extends FreeSpec with BeforeAndAfterAll with MasterAgentForScalaTest
{
  override lazy val signer = new SillySigner(SillySignature("MY-SILLY-SIGNATURE"))

  private val testStartedAt = Timestamp.now - 24.h

  private lazy val uri = master.localUri

  protected val agentRefPaths = TestAgentRefPath :: AgentRefPath("/FOLDER/AGENT-A") :: Nil
  protected val fileBased = Nil
  private lazy val agent1Uri = directoryProvider.agents(0).localUri.toString
  private lazy val agent2Uri = directoryProvider.agents(1).localUri.toString
  private lazy val httpClient = new SimpleAkkaHttpClient(label = "MasterWebServiceTest", uri, "/master")
    .closeWithCloser

  private var sessionToken: String = "INVALID"

  override val masterModule = new AbstractModule {
    @Provides @Singleton def eventIdClock(): EventIdClock = new EventIdClock.Fixed(1000)
  }

  import httpClient.materializer

  override def beforeAll() = {
    directoryProvider.master.configDir / "master.conf" ++=
      """jobscheduler.journal.sync = off
         jobscheduler.journal.delay = 0s
        |""".stripMargin
    writeMasterConfiguration(directoryProvider.master)
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

  "Await MasterReady" in {
    master.waitUntilReady()
  }

  "Await AgentReady" in {
    // Proceed first after all AgentReady have been received, to get an event sequence as expected
    for (agentRefPath <- agentRefPaths) {
      master.eventWatch.await[MasterAgentEvent.AgentReady](predicate = _.key == agentRefPath)
    }
  }

  "/master/api" in {
    val overview = httpClient.get[Json](s"$uri/master/api") await 99.s
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
              "agentRefPath": "/AGENT",
              "executable": {
                "TYPE": "ExecutablePath",
                "path": "/A$sh"
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
              "agentRefPath": "/AGENT",
              "executable": {
                "TYPE": "ExecutablePath",
                "path": "/B$sh"
              },
              "taskLimit": 1
            }
          }, {
            "TYPE": "Execute.Anonymous",
            "job": {
              "agentRefPath": "/AGENT",
              "executable": {
                "TYPE": "ExecutablePath",
                "path": "/MISSING$sh"
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

    val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Nil
    val response = httpClient.postWithHeaders[Json, Json](s"$uri/master/api/command", cmd, headers) await 99.s
    assert(response == json"""
      {
        "TYPE": "Accepted"
      }""")
  }

  "/master/api/workflow" - {
    testGet("master/api/workflow",
      RawHeader("X-JobScheduler-Session", sessionToken) :: Nil,
      json"""{
        "eventId": ${master.eventWatch.lastAddedEventId},
        "count": 2
      }""")

    testGet("master/api/workflow/",
      RawHeader("X-JobScheduler-Session", sessionToken) :: Nil,
      json"""{
        "eventId": ${master.eventWatch.lastAddedEventId},
        "array": [
          "/FOLDER/WORKFLOW-2",
          "/WORKFLOW"
        ]
      }""")

    testGets("master/api/workflow/FOLDER/WORKFLOW-2"::
             "master/api/workflow//FOLDER/WORKFLOW-2"::
             "master/api/workflow/%2FFOLDER%2FWORKFLOW-2":: Nil,
      RawHeader("X-JobScheduler-Session", sessionToken) :: Nil,
      json"""{
        "eventId": ${master.eventWatch.lastAddedEventId},
        "path": "/FOLDER/WORKFLOW-2",
        "versionId": "VERSION-1",
        "instructions": [
          {
            "TYPE": "Execute.Anonymous",
            "job": {
              "agentRefPath": "/AGENT",
              "executable": {
               "TYPE": "ExecutablePath",
               "path": "/B$sh"
             },
              "taskLimit": 1
            }
          }, {
            "TYPE": "Execute.Anonymous",
            "job": {
              "agentRefPath": "/AGENT",
              "executable": {
               "TYPE": "ExecutablePath",
               "path": "/MISSING$sh"
             },
              "taskLimit": 1
            }
          }
        ]
      }""")
  }

  "/master/api/agent" - {
    testGet("master/api/agent",
      RawHeader("X-JobScheduler-Session", sessionToken) :: Nil,
      json"""{
        "eventId": ${master.eventWatch.lastAddedEventId},
        "count": 2
      }""")

    testGet("master/api/agent/",
      RawHeader("X-JobScheduler-Session", sessionToken) :: Nil,
      json"""{
        "eventId": ${master.eventWatch.lastAddedEventId},
        "array": [
          "/AGENT",
          "/FOLDER/AGENT-A"
        ]
      }""")

    testGet("master/api/agent/?return=AgentRef",
      RawHeader("X-JobScheduler-Session", sessionToken) :: Nil,
      json"""{
        "eventId": ${master.eventWatch.lastAddedEventId},
        "array": [
          {
            "path": "/AGENT",
            "versionId": "INITIAL",
            "uri": "$agent1Uri"
          }, {
            "path": "/FOLDER/AGENT-A",
            "versionId": "INITIAL",
            "uri": "$agent2Uri"
          }
        ]
      }""")

    testGet("master/api/agent/FOLDER/AGENT-A?return=AgentRef",
      RawHeader("X-JobScheduler-Session", sessionToken) :: Nil,
      json"""{
        "eventId": ${master.eventWatch.lastAddedEventId},
        "path": "/FOLDER/AGENT-A",
        "versionId": "INITIAL",
        "uri": "$agent2Uri"
      }""")

    testGet("master/api/agent/%2FFOLDER%2FAGENT-A?return=AgentRef",
      RawHeader("X-JobScheduler-Session", sessionToken) :: Nil,
      json"""{
        "eventId": ${master.eventWatch.lastAddedEventId},
        "path": "/FOLDER/AGENT-A",
        "versionId": "INITIAL",
        "uri": "$agent2Uri"
      }""")
  }

  "/master/api/agent-proxy" - {
    "/master/api/agent-proxy/%2FFOLDER%2FAGENT-A" in {
      // Pass-through AgentRef. Slashes but the first in AgentRefPath must be coded as %2F.
      val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Nil
      val overview = httpClient.get[AgentOverview](s"$uri/master/api/agent-proxy/FOLDER%2FAGENT-A", Duration.Inf, headers) await 99.s
      assert(overview.version == BuildInfo.prettyVersion)
    }

    "/master/api/agent-proxy/UNKNOWN returns 400" in {
      val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Nil
      val e = intercept[HttpException] {
        httpClient.get[Json](s"$uri/master/api/agent-proxy/UNKNOWN", headers) await 99.s
      }
      assert(e.status.intValue == 400/*BadRequest*/)
      assert(e.problem == Some(Problem("No such key 'AgentRef:/UNKNOWN'")))
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
          httpClient.postWithHeaders[Json, Json](s"$uri/master/api/order", order, headers) await 99.s
        }
        assert(exception.status.intValue == 400/*BadRequest*/)
        assert(exception.dataAsString contains "No such key 'Workflow:/MISSING'")  // Or similar
        assert(exception.problem == Some(Problem("No such key 'Workflow:/MISSING'")))
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

      "Bad OrderId" in {
        val order = json"""{
          "id": "A/B",
          "workflowPath": "/WORKFLOW"
        }"""

        val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Accept(`application/json`) :: Nil
        val response = httpClient.post_[Json](s"$uri/master/api/order", order, headers) await 99.s
        assert(response.status.intValue == 400/*BadRequest*/)
        assert(response.utf8StringFuture.await(99.seconds).parseJsonCheckedAs[Problem]
          == Right(Problem("OrderId must not contain reserved characters /")))
        assert(response.header[Location].isEmpty)
      }

      "Multiple" in {
        val orders = json"""
          [
            {
              "id": "ORDER-ID",
              "workflowPath": "/WORKFLOW"
            }, {
              "id": "ORDER-ID",
              "workflowPath": "/WORKFLOW"
            }
          ]"""
        val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Accept(`application/json`) :: Nil
        val response = httpClient.post_[Json](s"$uri/master/api/order", orders, headers) await 99.s
        assert(response.status == OK)  // Duplicates are silently ignored
        assert(response.header[Location].isEmpty)
        response.entity.discardBytes()
      }

      "Multiple with bad OrderId" in {
        val orders = json"""
          [
            {
              "id": "A/B",
              "workflowPath": "/WORKFLOW"
            }
          ]"""
        val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Accept(`application/json`) :: Nil
        val response = httpClient.post_[Json](s"$uri/master/api/order", orders, headers) await 99.s
        assert(response.status.intValue == 400/*BadRequest*/)
        assert(response.header[Location].isEmpty)
        assert(response.utf8StringFuture.await(99.seconds).parseJsonCheckedAs[Problem]
          == Right(Problem("OrderId must not contain reserved characters /")))
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

  var orderFinishedEventId: EventId = -1

  "(await OrderFinished)" in {
    val stampedEvents = master.eventWatch.await[OrderFinished]()  // Needed for test /master/api/events
    orderFinishedEventId = stampedEvents.head.eventId
  }

  "/master/api/snapshot/ (only JSON)" in {
    val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Nil
    val snapshots = httpClient.get[Json](s"$uri/master/api/snapshot/", headers) await 99.s
    assert(snapshots.asObject.get("eventId") == Some(orderFinishedEventId.asJson))
    val array = snapshots.asObject.get("array").get.asArray.get
    val shortenedArray = Json.fromValues(array.filterNot(o => o.asObject.get("TYPE").contains("AgentSnapshot".asJson)))  // Delete AgentSnapshot in `array` (for easy comparison)
    val masterMetaState = array.iterator.map(_.as(MasterSnapshots.SnapshotJsonCodec).orThrow)
      .collectFirst { case o: MasterMetaState => o }.get
    assert(shortenedArray == json"""[
      {
        "TYPE": "MasterMetaState",
        "masterId": "Master",
        "startedAt": ${masterMetaState.startedAt.toEpochMilli}
      }, {
        "TYPE": "VersionAdded",
        "versionId": "INITIAL"
      }, {
        "TYPE": "FileBasedAdded",
        "path": "AgentRef:/AGENT",
        "signed": {
          "string": "{\"TYPE\":\"AgentRef\",\"path\":\"/AGENT\",\"versionId\":\"INITIAL\",\"uri\":\"$agent1Uri\"}",
          "signature": {
            "TYPE": "Silly",
            "signatureString": "MY-SILLY-SIGNATURE"
          }
        }
      }, {
        "TYPE": "FileBasedAdded",
        "path": "AgentRef:/FOLDER/AGENT-A",
        "signed": {
          "string": "{\"TYPE\":\"AgentRef\",\"path\":\"/FOLDER/AGENT-A\",\"versionId\":\"INITIAL\",\"uri\":\"$agent2Uri\"}",
          "signature": {
            "TYPE": "Silly",
            "signatureString": "MY-SILLY-SIGNATURE"
          }
        }
      }, {
        "TYPE": "VersionAdded",
        "versionId": "VERSION-1"
      }, {
        "TYPE": "FileBasedAdded",
        "path": "Workflow:/FOLDER/WORKFLOW-2",
        "signed": {
          "string": "{\"TYPE\":\"Workflow\",\"path\":\"/FOLDER/WORKFLOW-2\",\"versionId\":\"VERSION-1\",\"instructions\":[{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentRefPath\":\"/AGENT\",\"executable\":{\"TYPE\":\"ExecutablePath\",\"path\":\"/B$sh\"},\"taskLimit\":1}},{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentRefPath\":\"/AGENT\",\"executable\":{\"TYPE\":\"ExecutablePath\",\"path\":\"/MISSING$sh\"},\"taskLimit\":1}}]}",
          "signature": {
            "TYPE": "Silly",
            "signatureString": "MY-SILLY-SIGNATURE"
          }
        }
      }, {
        "TYPE": "FileBasedAdded",
        "path": "Workflow:/WORKFLOW",
        "signed": {
          "string": "{\"TYPE\":\"Workflow\",\"path\":\"/WORKFLOW\",\"versionId\":\"VERSION-1\",\"instructions\":[{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentRefPath\":\"/AGENT\",\"executable\":{\"TYPE\":\"ExecutablePath\",\"path\":\"/A$sh\"},\"taskLimit\":1}}]}",
          "signature": {
            "TYPE": "Silly",
            "signatureString": "MY-SILLY-SIGNATURE"
          }
        }
      }
    ]""")   // Any orders would be added to `array`.
  }

  "/master/api/event (only JSON)" in {
    val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Nil
    val eventsJson = httpClient.get[Json](s"$uri/master/api/event?after=0", headers) await 99.s
    val keyedEvents: immutable.Seq[KeyedEvent[Event]] = {
      import com.sos.jobscheduler.master.data.events.MasterKeyedEventJsonCodec
      eventsJson.asObject.get("stamped").get.asArray.get.map(_.as[KeyedEvent[Event]].orThrow)
    }
    val agentRunId = keyedEvents.collectFirst { case AgentRefPath("/AGENT") <-: (e: AgentRegisteredMaster) => e.agentRunId }.get
    val totalRunningTime = keyedEvents.collectFirst { case _ <-: (e: MasterReady) => e.totalRunningTime }.get
    // Fields named "eventId" are renumbered for this test, "timestamp" are removed due to time-dependant values
    assert(manipulateEventsForTest(eventsJson) == json"""{
      "TYPE": "NonEmpty",
      "stamped": [
        {
          "eventId" : 1001,
          "TYPE" : "MasterInitialized",
          "masterId" : "Master",
          "startedAt" : 111222333
        },        {
          "eventId": 1002,
          "TYPE": "MasterReady",
          "timezone": "${ZoneId.systemDefault.getId}",
          "totalRunningTime": ${totalRunningTime.toBigDecimal}
        }, {
          "eventId": 1003,
          "TYPE": "VersionAdded",
          "versionId": "INITIAL"
        }, {
          "eventId": 1004,
          "TYPE": "FileBasedAdded",
          "path": "AgentRef:/AGENT",
          "signed": {
            "string": "{\"TYPE\":\"AgentRef\",\"path\":\"/AGENT\",\"versionId\":\"INITIAL\",\"uri\":\"$agent1Uri\"}",
            "signature": {
              "TYPE": "Silly",
              "signatureString": "MY-SILLY-SIGNATURE"
            }
          }
        }, {
          "eventId": 1005,
          "TYPE": "FileBasedAdded",
          "path": "AgentRef:/FOLDER/AGENT-A",
          "signed": {
            "string": "{\"TYPE\":\"AgentRef\",\"path\":\"/FOLDER/AGENT-A\",\"versionId\":\"INITIAL\",\"uri\":\"$agent2Uri\"}",
            "signature": {
              "TYPE": "Silly",
              "signatureString": "MY-SILLY-SIGNATURE"
            }
          }
        }, {
          "eventId": 1006,
          "key": "/AGENT",
          "TYPE": "AgentRegisteredMaster",
          "agentRunId": "${agentRunId.string}"
        }, {
          "eventId": 1007,
          "TYPE": "AgentReady",
          "key": "/AGENT",
          "timezone": "${ZoneId.systemDefault.getId}"
        }, {
          "eventId": 1008,
          "TYPE": "VersionAdded",
          "versionId": "VERSION-1"
        }, {
          "eventId": 1009,
          "TYPE": "FileBasedAdded",
          "path": "Workflow:/WORKFLOW",
          "signed": {
            "string": "{\"TYPE\":\"Workflow\",\"path\":\"/WORKFLOW\",\"versionId\":\"VERSION-1\",\"instructions\":[{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentRefPath\":\"/AGENT\",\"executable\":{\"TYPE\":\"ExecutablePath\",\"path\":\"/A$sh\"},\"taskLimit\":1}}]}",
            "signature": {
              "TYPE": "Silly",
              "signatureString": "MY-SILLY-SIGNATURE"
            }
          }
        }, {
          "eventId": 1010,
          "TYPE": "FileBasedAdded",
          "path": "Workflow:/FOLDER/WORKFLOW-2",
          "signed": {
            "string": "{\"TYPE\":\"Workflow\",\"path\":\"/FOLDER/WORKFLOW-2\",\"versionId\":\"VERSION-1\",\"instructions\":[{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentRefPath\":\"/AGENT\",\"executable\":{\"TYPE\":\"ExecutablePath\",\"path\":\"/B$sh\"},\"taskLimit\":1}},{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentRefPath\":\"/AGENT\",\"executable\":{\"TYPE\":\"ExecutablePath\",\"path\":\"/MISSING$sh\"},\"taskLimit\":1}}]}",
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
          "agentRefPath":"/AGENT"
        }, {
          "eventId": 1013,
          "TYPE": "OrderTransferredToAgent",
          "key": "ORDER-ID",
          "agentRefPath": "/AGENT"
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
            "returnCode": 0
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
          "TYPE": "OrderTransferredToMaster",
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
        (obj("TYPE") == Json.fromString("AgentReady") || obj("TYPE") == Json.fromString("AgentRegisteredMaster")) &&
            json.as[KeyedEvent[MasterAgentEvent]].orThrow.key != TestAgentRefPath || // Let through only Events for one AgentRef, because ordering is undefined
          obj("TYPE") == Json.fromString("AgentCouplingFailed")
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

  "/master/api/graphql" - {
    "Syntax error" in {
      val query = "INVALID"
      val queryJson = json"""{ "query": "$query" }"""
      val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Accept(`application/json`) :: Nil
      val response = httpClient.post_[Json](s"$uri/master/api/graphql", queryJson, headers) await 99.s
      assert(response.status.intValue == 400/*BadRequest*/)
      assert(response.utf8StringFuture.await(99.s).parseJsonOrThrow ==
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
      assert(response.utf8StringFuture.await(99.s).parseJsonOrThrow ==
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
        master.eventWatch.await[OrderProcessed](_.key == order2Id)
        master.eventWatch.await[OrderMoved](_.key == order2Id)
      }

      "Single order" in {
        val body = Json.obj(
          "query" -> """
            query Q($orderId: OrderId!) {
              order(id: $orderId) {
                id
                workflowPosition {
                  workflowId { path, versionId }
                  position
                },
                attachedState {
                  agentRefPath
                }
              }
            }""".asJson,
          "variables" -> Json.obj(
            "orderId" -> order2Id.asJson))
        assert(postGraphql(body) ==
          json"""{
            "data": {
              "order": {
                "id": "ORDER-MISSING-JOB",
                "workflowPosition": {
                  "workflowId": {
                    "path": "/FOLDER/WORKFLOW-2",
                    "versionId": "VERSION-1"
                  },
                  "position": [ 1 ]
                },
                "attachedState": {
                  "agentRefPath": "/AGENT"
                }
              }
            }
          }""")
      }

      "All orders" in {
        val body = Json.obj(
          "query" -> """{
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
          "query" -> """
            query Q($workflowPath: WorkflowPath) {
              orders(workflowPath: $workflowPath) {
                id
                workflowPosition {
                  instruction {
                    ... on Execute_Anonymous {
                      job {
                        executablePath
                      }
                    }
                  }
                }
              }
            }""".asJson,
          "variables" -> Json.obj(
            "workflowPath" -> "/FOLDER/WORKFLOW-2".asJson))
        assert(postGraphql(body) ==
          json"""{
           "data": {
              "orders": [
                {
                  "id": "ORDER-MISSING-JOB",
                  "workflowPosition": {
                    "instruction": {
                      "job": {
                        "executablePath": "/MISSING$sh"
                      }
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
      httpClient.postWithHeaders[Json, Json](s"$uri/master/api/graphql", graphql, headers).await(99.s)
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
      assert(forbidden.utf8StringFuture.await(99.s) == Forbidden.defaultMessage)
    }
  }

  "Commands" - {
    "CancelOrder" in {
      val cmd = json"""
        {
          "TYPE": "Batch",
          "commands": [
            {
              "TYPE": "CancelOrder",
              "orderId": "UNKNOWN"
            }, {
              "TYPE": "CancelOrder",
              "orderId": "ORDER-FRESH"
            }
          ]
        }"""
      val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Nil
      testJson(
        httpClient.postWithHeaders[Json, Json](s"$uri/master/api/command", cmd, headers) await 99.s,
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
      val headers = RawHeader("X-JobScheduler-Session", sessionToken) :: Nil
      testJson(
        httpClient.postWithHeaders[Json, Json](s"$uri/master/api/command", cmd, headers) await 99.s,
        json"""{
          "TYPE": "Accepted"
        }""")
      master.terminated await 99.s
    }
  }

  private def testGets(suburis: Iterable[String], headers: => List[HttpHeader], expected: => Json, manipulateResponse: JsonObject => JsonObject = identity): Unit =
    for (suburi <- suburis) testGet(suburi, headers, expected, manipulateResponse)

  private def testGet(suburi: String, headers: => List[HttpHeader], expected: => Json, manipulateResponse: JsonObject => JsonObject = identity): Unit =
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
  private def writeMasterConfiguration(master: DirectoryProvider.MasterTree): Unit = {
    master.configDir / "master.conf" ++= """
      |jobscheduler.webserver.test = true
      |""".stripMargin
    (master.configDir / "private" / "private.conf").append("""
      |jobscheduler.auth.users {
      |  TEST-USER {
      |    password = "plain:TEST-PASSWORD",
      |    permissions = [ UpdateRepo ]
      |  }
      |}
      |""".stripMargin)
  }

  private def writeAgentConfiguration(agent: DirectoryProvider.AgentTree): Unit = {
    agent.writeExecutable(ExecutablePath(s"/A$sh"), operatingSystem.sleepingShellScript(1.second))  // Allow some time to check web service before order finishes
    agent.writeExecutable(ExecutablePath(s"/B$sh"), operatingSystem.sleepingShellScript(0.seconds))
  }
}
