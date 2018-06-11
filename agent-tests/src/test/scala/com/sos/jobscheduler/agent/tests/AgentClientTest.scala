package com.sos.jobscheduler.agent.tests

import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.configuration.{AgentStartInformation, Akkas}
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.test.AgentTest
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import org.scalatest.concurrent.ScalaFutures

/**
 * @author Joacim Zschimmer
 */
final class AgentClientTest extends FreeSpec with ScalaFutures with AgentTest {

  override implicit val patienceConfig = PatienceConfig(timeout = 10.s.toConcurrent)

  override lazy val agentConfiguration = newAgentConfiguration()
  private implicit lazy val actorSystem = Akkas.newActorSystem("AgentClientTest")(closer)
  private lazy val client = AgentClient(agentUri = agent.localUri.toString)
    //licenseKeys = List(LicenseKeyString("SOS-DEMO-1-D3Q-1AWS-ZZ-ITOT9Q6")))

  "get /" in {
    val overview = client.overview await 99.s
    assert(!overview.isTerminating)
    assert(overview.version == AgentStartInformation.VersionString)
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

object AgentClientTest {
  private val TestAgentTaskId = AgentTaskId("1-123")
}
