package js7.agent.tests

import cats.effect.unsafe.IORuntime
import js7.agent.client.AgentClient
import js7.base.BuildInfo
import js7.base.auth.Admission
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.time.ScalaTime.*
import js7.common.pekkoutils.Pekkos.newActorSystem
import org.apache.pekko.actor.ActorSystem
import org.scalatest.concurrent.ScalaFutures

/**
 * @author Joacim Zschimmer
 */
final class AgentClientTest extends OurTestSuite, ScalaFutures, AgentTester:

  private given IORuntime = ioRuntime

  override implicit val patienceConfig = PatienceConfig(timeout = 10.s)

  override lazy val agentConfiguration = newAgentConfiguration()
  private implicit lazy val actorSystem: ActorSystem =
    newActorSystem("AgentClientTest", executionContext = ioRuntime.compute)
  private lazy val client = AgentClient(Admission(agent.localUri))

  override def afterAll(): Unit =
    try 
      actorSystem.terminate().await(99.s)
    finally
      super.afterAll()

  "get /" in:
    val overview = client.overview await 99.s
    assert(overview.version == BuildInfo.prettyVersion)

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

//object AgentClientTest {
//  private val TestTaskId = TaskId("1-123")
//}
