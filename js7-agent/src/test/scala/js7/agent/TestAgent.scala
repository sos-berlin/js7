package js7.agent

import akka.actor.ActorSystem
import cats.effect.{ExitCase, Resource}
import js7.agent.RunningAgent.TestWiring
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.ShutDown
import js7.base.auth.SessionToken
import js7.base.eventbus.StandardEventBus
import js7.base.io.process.ProcessSignal
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.RichMonixResource
import js7.base.problem.Checked
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichThrowable}
import js7.base.utils.{Allocated, ProgramTermination}
import js7.base.web.Uri
import js7.common.system.ThreadPools.standardSchedulerResource
import js7.common.system.startup.MainServices
import js7.core.command.CommandMeta
import js7.journal.watch.EventWatch
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

final class TestAgent(allocated: Allocated[Task, RunningAgent])
extends AutoCloseable {
  val agent = allocated.allocatedThing

  implicit val scheduler: Scheduler =
    agent.scheduler

  def close(): Unit =
    stop.await(99.s)

  def stop: Task[Unit] =
    allocated.stop

  def terminate(
    processSignal: Option[ProcessSignal] = None,
    clusterAction: Option[ShutDown.ClusterAction] = None,
    suppressSnapshot: Boolean = false)
  : Task[ProgramTermination] =
    agent
      .terminate(processSignal, clusterAction, suppressSnapshot)
      .guarantee(stop)

  def untilTerminated: Task[ProgramTermination] =
    agent.untilTerminated

  implicit def actorSystem: ActorSystem =
    agent.actorSystem

  def localUri: Uri =
    agent.localUri

  def sessionToken: SessionToken =
    agent.sessionToken

  def untilReady: Task[MainActor.Ready] =
    agent.untilReady

  def currentAgentState(): AgentState =
    agent.agentState.await(99.s).orThrow

  def eventWatch: EventWatch =
    agent.eventWatch

  def testEventBus: StandardEventBus[Any] =
    agent.testEventBus

  def executeCommandAsSystemUser(command: AgentCommand): Task[Checked[AgentCommand.Response]] =
    agent.executeCommandAsSystemUser(command)

  def executeCommand(cmd: AgentCommand, meta: CommandMeta): Task[Checked[AgentCommand.Response]] =
    agent.executeCommand1(cmd: AgentCommand, meta)

  //def blockingUse[R](stopTimeout: Duration)(body: TestAgent => R)(implicit scheduler: Scheduler)
  //: R = {
  //  val ac: AutoCloseable = () => stop.await(stopTimeout)
  //  autoClosing(ac)(_ => body(this))
  //}
}

object TestAgent {
  private val logger = Logger[this.type]

  def blockingRun(
    conf: AgentConfiguration,
    timeout: FiniteDuration = 99.s)
    (body: TestAgent => Unit)
  : ProgramTermination =
    MainServices.blockingRun(conf.name, conf.config, timeout = timeout)(
      resource = resource(conf)(_),
      use = (agent: RunningAgent, scheduler: Scheduler) =>
      try {
        implicit val s = scheduler
        val testAgent = new TestAgent(new Allocated(agent, agent.terminate().void))
        try body(testAgent)
        catch { case NonFatal(t) =>
          logger.debug(s"💥 ${t.toStringWithCauses}", t.nullIfNoStackTrace)
          agent.terminate().await(99.s)
          throw t
        }
        agent.terminate().await(99.s)
      } catch { case NonFatal(t) =>
        // Silent deadlock in case of failure?
        logger.error(t.toStringWithCauses, t.nullIfNoStackTrace)
        throw t
      })

  def start(conf: AgentConfiguration, testWiring: TestWiring = TestWiring.empty): Task[TestAgent] =
    CorrelId.bindNew(
      standardSchedulerResource[Task](conf.name, conf.config)
        .flatMap(js7Scheduler =>
          RunningAgent
            .resource(conf, testWiring)(js7Scheduler)
            .executeOn(js7Scheduler)))
      .toAllocated
      .map(new TestAgent(_))

  private def resource(conf: AgentConfiguration, testWiring: TestWiring = TestWiring.empty)
    (implicit scheduler: Scheduler)
  : Resource[Task, RunningAgent] =
    RunningAgent.resource(conf, testWiring)
      .flatMap(agent => Resource.makeCase(
        acquire = Task.pure(agent))(
        release = (agent, exitCase) => {
          exitCase match {
            case ExitCase.Error(throwable) =>
              logger.error(throwable.toStringWithCauses, throwable.nullIfNoStackTrace)
            case _ =>
          }
          // Avoid Akka 2.6 StackTraceError which occurs when agent.terminate() has not been executed:
          agent.untilTerminated.void
            .timeoutTo(3.s, Task.unit)
            .tapError(t => Task(
              logger.error(t.toStringWithCauses, t.nullIfNoStackTrace)))
        }))
}
