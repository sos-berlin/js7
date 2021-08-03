package js7.agent.scheduler.order

import akka.actor.{Actor, ActorRef, PoisonPill, Props, Terminated}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import com.softwaremill.diffx.generic.auto._
import com.typesafe.config.{Config, ConfigValueFactory}
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.{Files, Path}
import js7.agent.configuration.AgentConfiguration
import js7.agent.configuration.Akkas.newAgentActorSystem
import js7.agent.data.AgentState
import js7.agent.scheduler.job.JobDriver
import js7.agent.scheduler.order.OrderActorTest._
import js7.agent.tests.TestAgentDirectoryProvider
import js7.base.generic.Completed
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.process.Processes.{ShellFileExtension => sh}
import js7.base.io.process.{Stderr, Stdout, StdoutOrStderr}
import js7.base.problem.Problem
import js7.base.system.OperatingSystem.isWindows
import js7.base.thread.Futures.implicits._
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.time.ScalaTime._
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.HasCloser
import js7.base.utils.ScalaUtils.syntax._
import js7.common.akkautils.{CatchingActor, SupervisorStrategies}
import js7.common.utils.Exceptions.repeatUntilNoException
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.event.{EventRequest, JournalId, KeyedEvent, Stamped}
import js7.data.item.VersionId
import js7.data.job.{JobConf, JobKey, RelativePathExecutable}
import js7.data.order.OrderEvent.{OrderAttachedToAgent, OrderDetachable, OrderDetached, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId, Outcome}
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.{NumberValue, StringValue}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.executor.configuration.JobExecutorConf
import js7.journal.configuration.JournalConf
import js7.journal.data.JournalMeta
import js7.journal.watch.JournalEventWatch
import js7.journal.{EventIdGenerator, JournalActor, StampedKeyedEventBus}
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.scalatest.Assertions._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable
import scala.concurrent.Promise
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import shapeless.tag

/**
  * @author Joacim Zschimmer
  */
final class OrderActorTest extends AnyFreeSpec with HasCloser with BeforeAndAfterAll
{
  private lazy val directoryProvider = TestAgentDirectoryProvider()
  private lazy val config = AgentConfiguration.forTest(
    directoryProvider.agentDirectory,
    name = "OrderActorTest"
  ).finishAndProvideFiles.config
    .withValue("js7.journal.simulate-sync", ConfigValueFactory.fromAnyRef("20ms"))
  private lazy val actorSystem = newAgentActorSystem("OrderActorTest")

  override def afterAll() = {
    close()
    directoryProvider.close()
    super.afterAll()
  }

  "Shell script" in {
    val pathExecutable = RelativePathExecutable(s"TEST-1$sh", v1Compatible = true)
    pathExecutable.toFile(directoryProvider.agentDirectory / "config" / "executables").writeExecutable(TestScript)
    val (testActor, result) = runTestActor(DummyJobKey, WorkflowJob(TestAgentPath, pathExecutable,
      Map("VAR1" -> StringConstant("FROM-JOB"))))
    assert(result.events == ExpectedOrderEvents)
    assert(result.stdoutStderr(Stdout) == s"Hej!${Nl}var1=FROM-JOB$Nl")
    assert(result.stdoutStderr(Stderr) == s"THIS IS STDERR$Nl")
    actorSystem.stop(testActor)
    repeatUntilNoException(9.s, 10.ms) {  // Windows
      Files.delete(directoryProvider.dataDirectory / "state/agent--0.journal")
    }
  }

  "Shell script with big stdout and stderr" in {
    val n = 1000
    def line(x: String, i: Int) = (s" $x$i" * ((i+n/100-1)/(n/100))).trim.ensuring(_.length < 8000)  // Windows: Maximum command line length is 8191 characters
    val expectedStderr = (for (i <- 1 to n) yield line("e", i) + Nl).mkString
    val expectedStdout = (for (i <- 1 to n) yield line("o", i) + Nl).mkString
    val pathExecutable = RelativePathExecutable(s"TEST-2$sh")
    pathExecutable.toFile(directoryProvider.agentDirectory / "config" / "executables").writeExecutable(
      (isWindows ?? "@echo off\n") +
        (for (i <- 1 to n) yield
          s"""echo ${line("o", i)}
             |echo ${line("e", i)}>&2
             |""".stripMargin).mkString)
    val (testActor, result) = runTestActor(DummyJobKey, WorkflowJob(TestAgentPath, pathExecutable))
    info(s"2×($n unbuffered lines, ${toKBGB(expectedStdout.length)}) took ${result.duration.pretty}")
    assert(result.stdoutStderr(Stderr) == expectedStderr)
    assert(result.stdoutStderr(Stdout) == expectedStdout)
    testActor ! PoisonPill
  }

  private var actorCounter = 0

  private def runTestActor(jobKey: JobKey, workflowJob: WorkflowJob): (ActorRef, Result) = {
    actorCounter += 1
    def props(promise: Promise[Result]) = Props { new TestActor(directoryProvider.agentDirectory, jobKey, workflowJob, promise, config) }
    val (testActor, terminated) = CatchingActor.actorOf(props, s"TestActor-$actorCounter")(actorSystem)
    val result: Result = terminated await 99.s  // Continues when the nested actor has terminated. CatchingActor may still run for some microseconds.
    (testActor, result)
  }

}

private object OrderActorTest {
  private val TestVersion = VersionId("VERSION")
  private val TestOrder = Order(OrderId("TEST-ORDER"), WorkflowPath("WORKFLOW") ~ TestVersion, Order.Ready)
  private val DummyJobKey = JobKey.Named(WorkflowPath.NoId, WorkflowJob.Name("test"))
  private val TestAgentPath = AgentPath("TEST-AGENT")
  private val TestPosition = Position(777)
  private val ExpectedOrderEvents = List(
    OrderAttachedToAgent(TestOrder.workflowPosition, Order.Ready, TestOrder.arguments, None, None,
      TestOrder.historicOutcomes, AgentPath("TEST-AGENT"), None, None, false, false),
    OrderProcessingStarted,
    OrderProcessed(Outcome.Succeeded(Map("returnCode" -> NumberValue(0), "result" -> StringValue("TEST-RESULT-FROM-JOB")))),
    OrderMoved(TestPosition),
    OrderDetachable,
    OrderDetached)
  private val Nl = System.lineSeparator

  private val TestScript =
    if (isWindows) """
      |@echo off
      |echo Hej!
      |echo THIS IS STDERR>&2
      |echo var1=%SCHEDULER_PARAM_VAR1%
      |echo result=TEST-RESULT-%SCHEDULER_PARAM_VAR1% >>"%SCHEDULER_RETURN_VALUES%"
      |""".stripMargin
    else """
      |echo "Hej!"
      |echo THIS IS STDERR >&2
      |echo "var1=$SCHEDULER_PARAM_VAR1"
      |echo "result=TEST-RESULT-$SCHEDULER_PARAM_VAR1" >>"$SCHEDULER_RETURN_VALUES"
      |""".stripMargin

  private implicit val TestAkkaTimeout = Timeout(99.s)

  private case class Result(events: Seq[OrderEvent], stdoutStderr: Map[StdoutOrStderr, String], duration: FiniteDuration)



  private final class TestActor(dir: Path, jobKey: JobKey, workflowJob: WorkflowJob, terminatedPromise: Promise[Result], config: Config)
  extends Actor {
    import context.{actorOf, become, watch}
    override val supervisorStrategy = SupervisorStrategies.escalate
    if (!exists(dir / "tmp")) createDirectory(dir / "tmp")

    private val executorConf = JobExecutorConf(
      executablesDirectory = (dir / "config" / "executables").toRealPath(),
      temporaryDirectory = dir / "data" / "tmp",
      workingDirectory = dir / "data" / "tmp",
      killScript = None,
      scriptInjectionAllowed = false,
      globalIOX,
      blockingJobScheduler = globalIOX.scheduler)

    private val journalMeta = JournalMeta(AgentState, dir / "data" / "state" / "agent")

    private val journalActor = tag[JournalActor.type](actorOf(
      JournalActor.props[AgentState](journalMeta, JournalConf.fromConfig(config), new StampedKeyedEventBus, Scheduler.global, new EventIdGenerator),
      "Journal"))
    private val eventWatch = new JournalEventWatch(journalMeta, config)
    private val jobDriver = new JobDriver(
        JobConf(jobKey, workflowJob, Workflow.empty, ControllerId("CONTROLLER"),
          sigkillDelay = 5.s),
        executorConf,
        _ => Left(Problem("No JobResource here")))
    private val orderActor = watch(actorOf(
      OrderActor.props(TestOrder.id, Workflow.of(TestOrder.workflowId),
        journalActor = journalActor, OrderActor.Conf(config, JournalConf.fromConfig(config)),
        ControllerId("CONTROLLER")),
      s"Order-${TestOrder.id.string}"))

    private val orderChangeds = mutable.Buffer.empty[OrderActor.Output.OrderChanged]
    private val events = mutable.Buffer.empty[OrderEvent]
    private val stdoutStderr = (for (t <- StdoutOrStderr.values) yield t -> new StringBuilder).toMap
    private var orderDetached = false
    private var orderActorTerminated = false

    (journalActor ? JournalActor.Input.StartWithoutRecovery(JournalId.random(), Some(eventWatch)))
      .pipeTo(self)
    eventWatch.observe(EventRequest.singleClass[OrderEvent](timeout = Some(999.s))) foreach self.!
    val runningSince = now

    override def postStop(): Unit = {
      eventWatch.close()
      super.postStop()
    }

    def receive = {
      case JournalActor.Output.Ready(_) =>
        orderActor ! OrderActor.Command.Attach(TestOrder.copy(
          attachedState = Some(Order.Attached(TestAgentPath))))
        become(attaching)
    }

    private def attaching: Receive = receiveOrderEvent orElse {
      case Completed =>
        orderActor ! OrderActor.Input.StartProcessing(jobDriver, workflowJob, Map.empty)
        become(processing)
    }

    private def processing: Receive = receiveOrderEvent

    private def detachable: Receive = receiveOrderEvent

    private def detaching: Receive = receiveOrderEvent orElse {
      case "DETACHED" =>
        orderDetached = true
        checkTermination()

      case Terminated(`orderActor`) =>
        orderActorTerminated = true
        checkTermination()
    }

    private def terminating: Receive = receiveOrderEvent orElse {
      case Terminated(`orderActor`) =>
        orderActorTerminated = true
        checkTermination()
    }

    private def receiveOrderEvent: Receive = {
      case o: OrderActor.Output.OrderChanged =>
        orderChangeds += o
        checkTermination()

      case Stamped(_, _, KeyedEvent(TestOrder.id, event: OrderEvent)) =>  // Duplicate to OrderChanged, in unknown order
        event match {
          case OrderStdWritten(t, chunk) =>
            assert(events.last == OrderProcessingStarted)
            stdoutStderr(t) ++= chunk

          case _: OrderProcessed =>
            events += event
            orderActor ? OrderActor.Command.HandleEvents(OrderMoved(TestPosition) :: Nil) await 99.s

          case _: OrderMoved =>
            events += event
            orderActor ? OrderActor.Command.HandleEvents(OrderDetachable :: Nil) await 99.s
            become(detachable)

          case OrderDetachable =>
            events += event
            (orderActor ? OrderActor.Command.HandleEvents(OrderDetached :: Nil)).mapTo[Completed].map(_ => self ! "DETACHED")
            become(detaching)

          case OrderDetached =>
            events += event
            checkTermination()

          case _ =>
            events += event
        }
      }

    private def checkTermination(): Unit =
      if (orderDetached && orderActorTerminated && events.lastOption.contains(OrderDetached) && orderChangeds.lastOption.map(_.events.last).contains(OrderDetached)) {
        assert(events == orderChangeds.map(_.events.last))
        terminatedPromise.success(Result(events.toVector, stdoutStderr.view.mapValues(_.toString).toMap, runningSince.elapsed))
        context.stop(self)
      }
  }
}
