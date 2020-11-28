package js7.tests

import java.util.regex.Pattern
import js7.agent.data.Problems.SignedInjectionNotAllowed
import js7.base.problem.Checked.Ops
import js7.base.system.OperatingSystem.isWindows
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax._
import js7.common.configutils.Configs.HoconStringInterpolator
import js7.common.scalautil.FileUtils.syntax.RichPath
import js7.common.scalautil.FileUtils.withTemporaryFile
import js7.data.agent.AgentName
import js7.data.event.{EventSeq, KeyedEvent, TearableEventSeq}
import js7.data.job.RelativeExecutablePath
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.NamedValues
import js7.data.workflow.position.BranchId.Then
import js7.data.workflow.position.Position
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.tests.ExecuteTest._
import js7.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class ExecuteTest extends AnyFreeSpec
{
  "signed-script-injection-allowed = off (default)" - {
    "Executing an inline script is not allowed" in {
      testInjectionNotAllowed("""
        define workflow {
          execute agent="AGENT", script=":";
        }"""
      )
    }

    "Executing an absolute path executable is not allowed" in {
      testInjectionNotAllowed("""
        define workflow {
          execute agent="AGENT", executable="/ABSOLUTE";
        }""")
    }

    def testInjectionNotAllowed(workflowNotation: String): Unit = {
      val workflow = WorkflowParser.parse(WorkflowPath("/WORKFLOW"), workflowNotation).orThrow

      autoClosing(new DirectoryProvider(TestAgentName :: Nil, inventoryItems = workflow :: Nil, testName = Some("ExecuteTest"))) { directoryProvider =>
        directoryProvider.run { (controller, _) =>
          val orderId = OrderId("❌")
          controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
          val orderProcessed = controller.eventWatch.await[OrderProcessed](_.key == orderId).head
          assert(orderProcessed.value.event.outcome.asInstanceOf[Outcome.Disrupted].reason.problem == SignedInjectionNotAllowed)
        }
      }
    }
  }

  "signed-script-injection-allowed = on" - {
    "Execute" in {
      val directoryProvider = new DirectoryProvider(TestAgentName :: Nil, inventoryItems = TestWorkflow :: Nil,
        testName = Some("ExecuteTest"),
        agentConfig = config"""js7.job.execution.signed-script-injection-allowed = on""")
      autoClosing(directoryProvider) { _ =>
        for (a <- directoryProvider.agents) {
          for (o <- Seq("SCRIPT-0a.cmd", "SCRIPT-0b.cmd")) a.writeExecutable(RelativeExecutablePath(o), ":")
          for (o <- Seq("SCRIPT-1.cmd", "SCRIPT-2.cmd", "SCRIPT-3.cmd"))
            a.writeExecutable(RelativeExecutablePath(o),
              if (isWindows) "@exit %SCHEDULER_PARAM_RETURN_CODE%" else "exit $SCHEDULER_PARAM_RETURN_CODE")
        }
        directoryProvider.run { (controller, _) =>
          val orderId = OrderId("🔺")
          controller.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
          controller.eventWatch.await[OrderFinished](_.key == orderId)
          checkEventSeq(orderId, controller.eventWatch.all[OrderEvent])
        }
      }
    }

    "Execute with command line" in {
      withTemporaryFile("ExecuteTest-", ".cmd") { executable =>
        executable.writeExecutable(
          if (isWindows) "@echo ==>%*<=="
          else """echo "1->$1 2->$2 3->$3"""")
        val workflowPath = WorkflowPath("/WORKFLOW")
        val workflow = WorkflowParser.parse(workflowPath,
          s"""define workflow {
            |  execute agent="AGENT", script='echo RESULT=TEST-VALUE >>${if (isWindows) "%SCHEDULER_RETURN_VALUES%" else "$SCHEDULER_RETURN_VALUES"}';
            |  execute agent="AGENT", executable="$executable";
            |  execute agent="AGENT", command="$executable \\$$RESULT 'B b' C";
            |}
            |""".stripMargin).orThrow
        val directoryProvider = new DirectoryProvider(Seq(TestAgentName), inventoryItems = Seq(workflow),
          testName = Some("ExecuteTest"),
          agentConfig = config"""js7.job.execution.signed-script-injection-allowed = on""")
        autoClosing(directoryProvider) { _ =>
          directoryProvider.run { (controller, _) =>
            val orderId = OrderId("🔴")
            controller.addOrderBlocking(FreshOrder(orderId, WorkflowPath("/WORKFLOW")))
            controller.eventWatch.await[OrderStdoutWritten](o => o.key == orderId &&
              removeAgentTaskId(o.event.chunk).contains(if (isWindows) "==><==" else "1-> 2-> 3->"))
            controller.eventWatch.await[OrderStdoutWritten](o => o.key == orderId &&
              o.event.chunk.contains(if (isWindows) "==><==" else "1->TEST-VALUE 2->B b 3->C"))
            controller.eventWatch.await[OrderFinished](_.key == orderId)
          }
        }
      }
    }
  }

  // TODO Replace --agent-task-id= by something different (for example, PID returned by Java 9)
  private def removeAgentTaskId(string: String): String =
    Pattern.compile("""--agent-task-id=[0-9]+-[0-9]+""").matcher(string).replaceAll("")

  private def checkEventSeq(orderId: OrderId, eventSeq: TearableEventSeq[IterableOnce, KeyedEvent[OrderEvent]]): Unit = {
    eventSeq match {
      case EventSeq.NonEmpty(stampeds) =>
        val events = stampeds.iterator.filter(_.value.key == orderId).map(_.value.event).to(Vector)
        assert(events == ExpectedEvents)
      case o =>
        fail(s"Unexpected EventSeq received: $o")
    }
  }
}

object ExecuteTest
{
  private val TestAgentName = AgentName("AGENT")
  private val ScriptProlog = isWindows ?? "@echo off\n"
  private val workflowNotation = s"""
    define workflow {
      execute executable="SCRIPT-0a.cmd", agent="AGENT";
      execute executable="SCRIPT-1.cmd", agent="AGENT", arguments={"return_code": "1"}, successReturnCodes=[1];
      job aJob;
      job bJob;  // returnCode=2
      if (true) {
        job aJob;
        job bJob;  // returnCode=3
        job cJob;  // returnCode=4
        define job bJob {
          execute agent="AGENT", executable="SCRIPT-3.cmd", arguments={"return_code": "3"}, successReturnCodes=[3];
        }
        define job cJob {
          execute agent="AGENT", script="${ScriptProlog}exit 4", arguments={"return_code": "4"}, successReturnCodes=[4];
        }
      };
      job dJob, arguments={"return_code": "5"};

      define job aJob {
        execute agent="AGENT", executable="SCRIPT-0b.cmd";
      }
      define job bJob {
        execute agent="AGENT", executable="SCRIPT-2.cmd", arguments={"return_code": "2"}, successReturnCodes=[2];
      }
      define job dJob {
        execute agent="AGENT", script="${ScriptProlog}exit 5", successReturnCodes=[5];
      }
    }"""
  private val TestWorkflow = WorkflowParser.parse(WorkflowPath("/WORKFLOW") ~ "INITIAL",  workflowNotation).orThrow

  private val ExpectedEvents = Vector(
    OrderAdded(TestWorkflow.id, None),
    OrderAttachable(TestAgentName),
    OrderAttached(TestAgentName),
    OrderStarted,
    OrderProcessingStarted,
    OrderProcessed(Outcome.Succeeded(NamedValues.rc(0))),
    OrderMoved(Position(1)),
    OrderProcessingStarted,
    OrderProcessed(Outcome.Succeeded(NamedValues.rc(1))),
    OrderMoved(Position(2)),
    OrderProcessingStarted,
    OrderProcessed(Outcome.Succeeded(NamedValues.rc(0))),
    OrderMoved(Position(3)),
    OrderProcessingStarted,
    OrderProcessed(Outcome.Succeeded(NamedValues.rc(2))),
    OrderMoved(Position(4) / Then % 0),
    OrderProcessingStarted,
    OrderProcessed(Outcome.Succeeded(NamedValues.rc(0))),
    OrderMoved(Position(4) / Then % 1),
    OrderProcessingStarted,
    OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
    OrderMoved(Position(4) / Then % 2),
    OrderProcessingStarted,
    OrderProcessed(Outcome.Succeeded(NamedValues.rc(4))),
    OrderMoved(Position(5)),
    OrderProcessingStarted,
    OrderProcessed(Outcome.Succeeded(NamedValues.rc(5))),
    OrderMoved(Position(6)),
    OrderDetachable,
    OrderDetached,
    OrderFinished)
}
