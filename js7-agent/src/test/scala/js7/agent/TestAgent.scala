package js7.agent

import akka.actor.ActorSystem
import cats.effect.{ExitCase, Resource}
import js7.agent.RunningAgent.TestWiring
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.base.auth.SessionToken
import js7.base.eventbus.StandardEventBus
import js7.base.io.process.ProcessSignal
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.base.utils.{Allocated, ProgramTermination}
import js7.base.web.Uri
import js7.common.system.startup.ServiceMain
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

  def stop: Task[Unit] =
    allocated.stop

  def terminate(
    processSignal: Option[ProcessSignal] = None,
    suppressSnapshot: Boolean = false)
  : Task[ProgramTermination] =
    agent.terminate(processSignal, suppressSnapshot)
      .guarantee(stop)

  def untilTerminated: Task[ProgramTermination] =
    agent.untilTerminated

  implicit def actorSystem: ActorSystem =
    agent.actorSystem

  def localUri: Uri =
    agent.localUri

  def sessionToken: SessionToken =
    agent.sessionToken

  def currentAgentState(): AgentState =
    agent.currentAgentState()

  def api: CommandMeta => DirectAgentApi =
    agent.api

  def eventWatch: EventWatch =
    agent.eventWatch

  def testEventBus: StandardEventBus[Any] =
    agent.testEventBus

  def executeCommandAsSystemUser(command: AgentCommand): Task[Checked[AgentCommand.Response]] =
    agent.executeCommandAsSystemUser(command)

  def executeCommand(cmd: AgentCommand, meta: CommandMeta): Task[Checked[AgentCommand.Response]] =
    agent.executeCommand(cmd: AgentCommand, meta)

  def close(): Unit =
    stop.await(99.s)
}

object TestAgent {
  private val logger = Logger[this.type]

  def blockingRun(
    conf: AgentConfiguration,
    timeout: FiniteDuration = 99.s)
    (body: TestAgent => Unit)
  : ProgramTermination =
    ServiceMain.blockingRun[RunningAgent](conf.name, conf.config, resource(conf)(_),
      use = agent => Task.defer {
        val testAgent = new TestAgent(new Allocated(agent, agent.terminate().void))
        try body(testAgent)
        catch { case NonFatal(t) =>
          logger.debug(s"💥 ${t.toStringWithCauses}", t.nullIfNoStackTrace)
          throw t
        }
        agent.terminate()
      },
      timeout = timeout)

  def start(
    conf: AgentConfiguration,
    testWiring: TestWiring = TestWiring.empty)(
    implicit scheduler: Scheduler)
  : Task[TestAgent] =
    resource(conf, testWiring).toAllocated.map(new TestAgent(_))

  private def resource(conf: AgentConfiguration, testWiring: TestWiring = TestWiring.empty)
    (implicit scheduler: Scheduler)
  : Resource[Task, RunningAgent] =
    RunningAgent.resource(conf, testWiring)
      .flatMap(agent => Resource.makeCase(
        acquire = Task.pure(agent))(
        release = {
          case (agent, exitCase) =>
            exitCase match {
              case ExitCase.Error(throwable) =>
                logger.error(throwable.toStringWithCauses, throwable.nullIfNoStackTrace)
              case _ =>
            }
            // Avoid Akka 2.6 StackTraceError which occurs when agent.terminate() has not been executed:
            Task.fromFuture(agent.terminated).void
              .timeoutTo(3.s, Task.unit)
              .tapError(t => Task(
                logger.error(t.toStringWithCauses, t.nullIfNoStackTrace)))
        }))
}
