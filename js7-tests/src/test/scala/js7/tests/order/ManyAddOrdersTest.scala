package js7.tests.order

import cats.instances.vector._
import cats.syntax.traverse._
import js7.base.circeutils.CirceUtils._
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.RichThrowableEither
import js7.data.agent.AgentPath
import js7.data.agent.AgentRefStateEvent.AgentReady
import js7.data.event.EventRequest
import js7.data.item.VersionId
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.OrderRemoved
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.order.ManyAddOrdersTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.script
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random

// Try to resemble a failed manual test
final class ManyAddOrdersTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentPaths = Seq(agentPath1, agentPath2)
  protected val items = Seq(workflow, workflow2)
  override def controllerConfig = config"""
    js7.auth.users.TEST-USER {
      password = "plain:TEST-PASSWORD"
    }
    """ withFallback super.controllerConfig
  override def agentConfig = config"""js7.job.execution.signed-script-injection-allowed = yes"""

  override def beforeAll() = {
    for (a <- directoryProvider.agents) {
      a.writeExecutable(pathExecutable, script(0.s))
    }
    super.beforeAll()
    controller.eventWatch.await[AgentReady]()
  }

  "Multiple AddOrders" in {
    run(workflow.path, n = 100)
    //run(workflow2.path, n = 100)
  }

  private def run(workflowPath: WorkflowPath, n: Int): Unit = {
    val orderIds = (1 to n).view.map(i => OrderId(s"ORDER-$i")).toVector
    val addOrders = Observable.fromIterable(orderIds)
      .bufferTumbling(2)
      .flatMap(orderIds =>
        Observable.fromTask(
          orderIds.toVector.traverse(orderId =>
            controllerApi.addOrders(Observable.pure(FreshOrder(orderId, workflowPath)))
          ).delayExecution(Random.nextInt(2).ms) >>
            orderIds.toVector.traverse(orderId =>
              controllerApi.removeOrdersWhenTerminated(Observable(orderId)))
        ))
      .completedL
    val awaitRemoved = controller.eventWatch
      .observe(EventRequest.singleClass[OrderRemoved](timeout = Some(99.s)))
      .scan(orderIds.toSet)((orderIds, stamped) => orderIds - stamped.value.key)
      .dropWhile(_.nonEmpty)
      .headL
    Task.sequence(Seq(addOrders, awaitRemoved)) await 99.s
  }
}

object ManyAddOrdersTest
{
  private val pathExecutable = RelativePathExecutable("executable.cmd")
  private val agentPath1 = AgentPath("AGENT-1")
  private val agentPath2 = AgentPath("AGENT-2")
  private val versionId = VersionId("INITIAL")

  private val workflow = Workflow.of(
    WorkflowPath("SINGLE") ~ versionId,
    Execute(WorkflowJob(agentPath1, pathExecutable, taskLimit = 3)),
    Execute(WorkflowJob(agentPath2, pathExecutable, taskLimit = 3)))

  private val workflow2 =json"""
    {
      "TYPE": "Workflow",
      "path": "TestRuns/Test0000000019/tryCatch_001",
      "instructions": [ {
        "TYPE": "Execute.Named",
        "jobName": "job1",
        "label": "job1"
      }, {
        "TYPE": "Try",
        "try": {
          "instructions": [ {
            "TYPE": "Execute.Named",
            "jobName": "job1_a",
            "label": "job1_a"
          }, {
            "TYPE": "Execute.Named",
            "jobName": "job1_b",
            "label": "job1_b"
          } ]
        },
        "catch": {
          "instructions": [ {
            "TYPE": "Execute.Named",
            "jobName": "job1_d",
            "label": "job1_d"
          }, {
            "TYPE": "Execute.Named",
            "jobName": "job1_e",
            "label": "job1_e"
          } ]
        }
      }, {
        "TYPE": "Execute.Named",
        "jobName": "job2",
        "label": "job2"
      }, {
        "TYPE": "Try",
        "try": {
          "instructions": [ {
            "TYPE": "Execute.Named",
            "jobName": "job2_a",
            "label": "job2_a"
          }, {
            "TYPE": "Execute.Named",
            "jobName": "job2_b",
            "label": "job2_b"
          }, {
            "TYPE": "Execute.Named",
            "jobName": "job2_c",
            "label": "job2_c"
          } ]
        },
        "catch": {
          "instructions": [ {
            "TYPE": "Retry"
          } ]
        },
        "maxTries": 10,
        "retryDelays": [ 1 ]
      }, {
        "TYPE": "Execute.Named",
        "jobName": "job3",
        "label": "job3"
      } ],
      "jobs": {
        "job1_e": {
          "agentPath": "AGENT-1",
          "executable": {
            "TYPE": "ScriptExecutable",
            "script": "echo \"hello world1\"\n"
          },
          "returnCodeMeaning": {
            "success": [ 0 ]
          },
          "taskLimit": 500
        },
        "job1_d": {
          "agentPath": "AGENT-1",
          "executable": {
            "TYPE": "ScriptExecutable",
            "script": "echo \"hello world1\"\n"
          },
          "returnCodeMeaning": {
            "success": [ 0 ]
          },
          "taskLimit": 500
        },
        "job2_c": {
          "agentPath": "AGENT-1",
          "executable": {
            "TYPE": "ScriptExecutable",
            "script": "echo \"hello world\"\n"
          },
          "returnCodeMeaning": {
            "success": [ 0 ]
          },
          "taskLimit": 500
        },
        "job2_b": {
          "agentPath": "AGENT-2",
          "executable": {
            "TYPE": "ScriptExecutable",
            "script": "echo \"hello world\"\n"
          },
          "returnCodeMeaning": {
            "success": [ 0 ]
          },
          "taskLimit": 500
        },
        "job2": {
          "agentPath": "AGENT-2",
          "executable": {
            "TYPE": "ScriptExecutable",
            "script": "echo \"hello world1\"\n"
          },
          "returnCodeMeaning": {
            "success": [ 0 ]
          },
          "taskLimit": 500
        },
        "job1": {
          "agentPath": "AGENT-1",
          "executable": {
            "TYPE": "ScriptExecutable",
            "script": "echo \"hello world1\"\n"
          },
          "returnCodeMeaning": {
            "success": [ 0 ]
          },
          "taskLimit": 500
        },
        "job3": {
          "agentPath": "AGENT-2",
          "executable": {
            "TYPE": "ScriptExecutable",
            "script": "echo \"hello world\"\nsleep 1"
          },
          "returnCodeMeaning": {
            "success": [ 0 ]
          },
          "taskLimit": 500
        },
        "job1_b": {
          "agentPath": "AGENT-1",
          "executable": {
            "TYPE": "ScriptExecutable",
            "script": "echo \"hello world1\"\n"
          },
          "returnCodeMeaning": {
            "success": [ 0 ]
          },
          "taskLimit": 500
        },
        "job2_a": {
          "agentPath": "AGENT-1",
          "executable": {
            "TYPE": "ScriptExecutable",
            "script": "echo \"hello world\"\n"
          },
          "returnCodeMeaning": {
            "success": [ 0 ]
          },
          "taskLimit": 500
        },
        "job1_a": {
          "agentPath": "AGENT-2",
          "executable": {
            "TYPE": "ScriptExecutable",
            "script": "echo \"hello world1\"\n"
          },
          "returnCodeMeaning": {
            "success": [ 0 ]
          },
          "taskLimit": 500
        }
      }
    }""".as[Workflow].orThrow
}
