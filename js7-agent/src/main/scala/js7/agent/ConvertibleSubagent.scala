package js7.agent

import cats.effect.Resource
import cats.effect.concurrent.Deferred
import js7.agent.ConvertibleSubagent.*
import js7.agent.configuration.AgentConfiguration
import js7.base.eventbus.StandardEventBus
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixResource
import js7.base.service.{MainService, Service}
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.{Allocated, ProgramTermination}
import js7.common.system.startup.StartUp.printlnWithClock
import js7.subagent.ConvertibleToDirector.{ConvertToDirector, convertibleToDirector}
import js7.subagent.{BareSubagent, ConvertibleToDirector, Subagent}
import monix.eval.Task

/** A bare Subagent which may become an Agent Director. */
final class ConvertibleSubagent private(
  convertible: ConvertibleToDirector,
  startedSubagentAllocated: Allocated[Task, Subagent],
  agentResource: Resource[Task, RunningAgent])
extends MainService with Service.StoppableByRequest
{
  private val terminated = Deferred.unsafe[Task, ProgramTermination]
  private var _subagentAllocated = startedSubagentAllocated
  private val director = Deferred.unsafe[Task, RunningAgent]
  @volatile private var _subagentOrDirector: Either[Subagent, RunningAgent] =
    Left(startedSubagentAllocated.allocatedThing)

  def untilTerminated = terminated.get

  protected def start = startService(
    convertible.use(_subagentAllocated).flatMap {
      case Left(ConvertToDirector) =>
        logger.info("Continue as Agent Director\n" + "─" * 80)
        printlnWithClock("Continue as Agent Director")
        // TODO Subagent mit laufenden Jobs lebendig übergeben,
        //  dabei Webserver und Journal (wie?) tauschen
        agentResource
          .use { agent =>
            _subagentOrDirector = Right(agent)
            _subagentAllocated = null // Release memory
            director.complete(agent) *>
              agent.untilTerminated
          }
          //.flatTap(terminated.complete) // Deadlock ???
          .void
          .guarantee(
            terminated.complete(ProgramTermination()).attempt.void)

      case Right(termination) =>
        logger.trace(s"### #$toString: $termination")
        _subagentAllocated.stop/*duplicate???*/ *>
          terminated.complete(termination)
    })

  def untilDirectorStarted: Task[RunningAgent] =
    director.get

  private def subagentOrDirector(): Either[Subagent, RunningAgent] =
    _subagentOrDirector

  // TODO Use same EventBus for both
  def testEventBus: StandardEventBus[Any] =
    _subagentOrDirector.fold(_.testEventBus, _.testEventBus)

  override def toString =
    s"ConvertibleSubagent(${_subagentOrDirector.fold(identity, identity)})"
}

object ConvertibleSubagent {
  private val logger = Logger(getClass)

  //private def resource(subagent: Subagent, agentConf: AgentConfiguration)
  //: Resource[Task, ConvertibleSubagent] =
  //  convertibleToDirector(convertible =>
  //    for {
  //      subagentDirectorService <- Service.resource(Task.deferAction(implicit s => Task(
  //        new ConvertibleSubagent(
  //          convertible,
  //          subagent,
  //          RunningAgent.resource(agentConf)))))
  //    } yield subagentDirectorService)

  def resource(agentConf: AgentConfiguration): Resource[Task, ConvertibleSubagent] =
    convertibleToDirector(convertible =>
      for {
        scheduler <- BareSubagent.threadPoolResource[Task](agentConf.subagentConf)
        //subagent <- convertible.resource(agentConf.subagentConf, scheduler)
        subagentDirectorService <- Service
          .resource(Task.deferAction(implicit s =>
            convertible
              .resource(agentConf.subagentConf, scheduler)
              .toAllocated
              .map(subagentAllocated =>
                new ConvertibleSubagent(
                  convertible,
                  subagentAllocated,
                  RunningAgent.resource(agentConf)))))
          .executeOn(scheduler)
      } yield subagentDirectorService)
}
