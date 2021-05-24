package js7.tests

import js7.base.BuildInfo
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.controller.client.AkkaHttpControllerApi
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.AddOrder
import js7.data.job.PathExecutable
import js7.data.order.{FreshOrder, Order, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.AkkaHttpControllerApiTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AkkaHttpControllerApiTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentPaths = Nil
  protected val items = Seq(TestWorkflow)

  private lazy val api = new AkkaHttpControllerApi(controller.localUri, Some(userAndPassword), actorSystem = controller.actorSystem)
    .closeWithCloser

  override def beforeAll() = {
    directoryProvider.controller.configDir / "private" / "private.conf" ++= """
        |js7.auth.users.TEST-USER = "plain:TEST-PASSWORD"
        |""".stripMargin
    super.beforeAll()
  }

  "login" in {
    assert(controller.sessionRegister.count.await(99.s) == 1)
    api.login() await 99.s
    assert(controller.sessionRegister.count.await(99.s) == 2)
    api.login() await 99.s
    assert(controller.sessionRegister.count.await(99.s) == 2)
  }

  "POST order" in {
    assert(api.addOrder(FreshOrder(TestOrder.id, TestWorkflow.path)).await(99.s) == true)
    assert(api.addOrder(FreshOrder(TestOrder.id, TestWorkflow.path)).await(99.s) == false)  // Duplicate
  }

  "ControllerCommand.AddOrder" in {
    assert(api.executeCommand(AddOrder(FreshOrder(TestOrder.id, TestWorkflow.path))).await(99.s) ==
      AddOrder.Response(ignoredBecauseDuplicate = true))
    assert(api.executeCommand(AddOrder(FreshOrder(SecondOrder.id, TestWorkflow.path))).await(99.s) ==
      AddOrder.Response(ignoredBecauseDuplicate = false))
  }

  "overview" in {
    assert(api.overview.await(99.s).version == BuildInfo.prettyVersion)
  }

  "ordersOverview" in {
    assert(api.ordersOverview.await(99.s).count == 2)
  }

  "orders" in {
    assert(api.orders.await(99.s).map(_.toSet) == Right(attachedOrders))
  }

  "workflow" in {
    assert(api.workflows.await(99.s) == Right(List(TestWorkflow)))
  }

  "logout" in {
    assert(controller.sessionRegister.count.await(99.s) == 2)
    api.logout() await 99.s
    assert(controller.sessionRegister.count.await(99.s) == 1)
  }

  "resource" in {
    AkkaHttpControllerApi.separateAkkaResource(controller.localUri, userAndPassword = Some(userAndPassword))
      .use(api => Task {
        api.login() await 99.s
        assert(controller.sessionRegister.count.await(99.s) == 2)
        assert(api.orders.await(99.s).map(_.toSet) == Right(attachedOrders))
      })
      .map(_ => controller.sessionRegister.count.await(99.s) == 1)
      .await(99.s)
  }
}

private object AkkaHttpControllerApiTest
{
  private val userAndPassword = UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD"))
  private val TestWorkflow = Workflow.of(WorkflowPath("WORKFLOW") ~ "INITIAL",
    Execute(WorkflowJob(AgentPath("MISSING"), PathExecutable("MISSING"))))
  private val TestOrder = Order(OrderId("ORDER-ID"), TestWorkflow.id, Order.Fresh)
  private val SecondOrder = Order(OrderId("SECOND-ORDER"), TestWorkflow.id, Order.Fresh)

  private val attachedOrders = Set(TestOrder, SecondOrder)
    .map(_.copy(attachedState = Some(Order.Attaching(AgentPath("MISSING")))))
}
