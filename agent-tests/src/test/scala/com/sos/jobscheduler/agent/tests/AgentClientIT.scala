package com.sos.jobscheduler.agent.tests

import akka.actor.ActorRefFactory
import akka.util.Timeout
import com.google.common.io.Closer
import com.sos.jobscheduler.agent.Agent
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.client.AgentClient.{RequestTimeout, commandDurationToRequestTimeout}
import com.sos.jobscheduler.agent.configuration.{AgentConfiguration, Akkas}
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.soslicense.LicenseKeyString
import com.sos.jobscheduler.common.time.ScalaTime._
import java.time.Duration
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.duration._

/**
 * @author Joacim Zschimmer
 */
final class AgentClientIT extends FreeSpec with ScalaFutures with BeforeAndAfterAll {

  override implicit val patienceConfig = PatienceConfig(timeout = 10.s.toConcurrent)
  private implicit val closer = Closer.create()

  private lazy val agent = {
    val conf = AgentConfiguration.forTest().copy(uriPathPrefix = "test")
    new Agent(conf).closeWithCloser
  }
  private implicit lazy val actorRefFactory: ActorRefFactory = Akkas.newActorSystem("AgentClientIT")(closer)
  private lazy val client = AgentClient(
    agentUri = agent.localUri.string,
    licenseKeys = List(LicenseKeyString("SOS-DEMO-1-D3Q-1AWS-ZZ-ITOT9Q6")))

  override def beforeAll() = agent.start() await 10.s

  override def afterAll() = {
    closer.close()
    super.afterAll()
  }

  "commandMillisToRequestTimeout" in {
    val upperBound = 30 * 24.h  // The upper bound depends on Akka tick length (Int.MaxValue ticks, a tick can be as short as 1ms)
    for (duration ← List[Duration](0.s, 1.s, upperBound)) {
      assert(commandDurationToRequestTimeout(duration) == Timeout((RequestTimeout + duration).toMillis, MILLISECONDS))
    }
  }

  //"get /task" in {
  //  val view = awaitResult(client.task.overview, 2.s)
  //  assert(view == TaskRegisterOverview(
  //    currentTaskCount = 0,
  //    totalTaskCount = 0))
  //}
  //
  //"get /task/ (incomplete)" in {
  //  val tasks = awaitResult(client.task.tasks, 2.s)
  //  assert(tasks == Nil)
  //  pending
  //}
  //
  //"get /task/1-123 (incomplete)" in {
  //  val e = intercept[UnsuccessfulResponseException] {
  //    awaitResult(client.task(TestAgentTaskId), 2.s): TaskOverview
  //  }
  //  assert(e.response.status == InternalServerError)
  //  assert(e.response.entity.asString contains "UnknownTaskException")
  //  pending
  //}
}

object AgentClientIT {
  private val TestAgentTaskId = AgentTaskId("1-123")
}
