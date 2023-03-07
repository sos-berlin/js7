package js7.agent.tests

import akka.actor.ActorSystem
import js7.agent.client.AgentClient
import js7.base.BuildInfo
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.common.akkautils.Akkas.newActorSystem
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.concurrent.ScalaFutures

/**
 * @author Joacim Zschimmer
 */
final class AgentClientTest extends OurTestSuite with ScalaFutures with AgentTester {

  override implicit val patienceConfig = PatienceConfig(timeout = 10.s)

  override lazy val agentConfiguration = newAgentConfiguration()
  private implicit lazy val actorSystem: ActorSystem = newActorSystem("AgentClientTest")
  private lazy val client = AgentClient(agentUri = agent.localUri, userAndPassword = None)

  override def afterAll(): Unit = {
    actorSystem.terminate().await(99.s)
    super.afterAll()
  }

  "get /" in {
    val overview = client.overview await 99.s
    assert(!overview.isTerminating)
    assert(overview.version == BuildInfo.prettyVersion)
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
  //    awaitResult(client.task(TestTaskId), 2.s): TaskOverview
  //  }
  //  assert(e.response.status == InternalServerError)
  //  assert(e.response.entity.asString contains "UnknownTaskException")
  //  pending
  //}
}

//object AgentClientTest {
//  private val TestTaskId = TaskId("1-123")
//}
