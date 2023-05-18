package js7.subagent

import cats.effect.Resource
import cats.effect.concurrent.Deferred
import cats.syntax.traverse.*
import js7.base.Js7Version
import js7.base.configutils.Configs.RichConfig
import js7.base.crypt.generic.DirectoryWatchingSignatureVerifier
import js7.base.eventbus.StandardEventBus
import js7.base.io.process.ProcessSignal
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.service.{MainService, Service}
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, ProgramTermination, SetOnce}
import js7.common.system.ThreadPools.unlimitedSchedulerResource
import js7.data.event.EventId
import js7.data.event.KeyedEvent.NoKey
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId}
import js7.data.subagent.Problems.{SubagentAlreadyDedicatedProblem, SubagentNotDedicatedProblem}
import js7.data.subagent.SubagentCommand.DedicateSubagent
import js7.data.subagent.SubagentEvent.SubagentShutdown
import js7.data.subagent.{SubagentId, SubagentRunId, SubagentState}
import js7.data.value.expression.Expression
import js7.journal.MemoryJournal
import js7.launcher.configuration.JobLauncherConf
import js7.subagent.Subagent.*
import js7.subagent.configuration.SubagentConf
import monix.eval.{Fiber, Task}
import monix.execution.Scheduler

final class Subagent private(
  val journal: MemoryJournal[SubagentState],
  signatureVerifier: DirectoryWatchingSignatureVerifier,
  val conf: SubagentConf,
  jobLauncherConf: JobLauncherConf,
  val testEventBus: StandardEventBus[Any])
extends MainService with Service.StoppableByRequest
{
  subagent =>

  val subagentRunId = SubagentRunId.fromJournalId(journal.journalId)
  private[subagent] val commandExecutor =
    new SubagentCommandExecutor(this, signatureVerifier)
  private val dedicatedAllocated =
    SetOnce[Allocated[Task, DedicatedSubagent]](SubagentNotDedicatedProblem)
  private val terminated = Deferred.unsafe[Task, ProgramTermination]

  protected def start =
    startService(Task
      .race(
        untilStopRequested *> shutdown(processSignal = None),
        untilTerminated)
      .void)

  def isShuttingDown: Boolean =
    dedicatedAllocated.toOption.fold(false)(_.allocatedThing.isShuttingDown)

  def untilTerminated: Task[ProgramTermination] =
    terminated.get

  def shutdown(
    processSignal: Option[ProcessSignal] = None,
    restart: Boolean = false,
    dontWaitForDirector: Boolean = false)
  : Task[ProgramTermination] = {
    logger.debugTask(
      "shutdown",
      Seq(processSignal, restart ? "restart", dontWaitForDirector ? "dontWaitForDirector")
        .flatten.mkString(" ")
    )(Task.defer {
      dedicatedAllocated
        .toOption
        .fold(Task.unit)(allocated =>
          allocated.allocatedThing
            .terminate(processSignal, dontWaitForDirector = dontWaitForDirector)
            .*>(allocated.release))
        .*>(journal
          // The event may get lost due to immediate shutdown !!!
          .persistKeyedEvent(NoKey <-: SubagentShutdown)
          .rightAs(())
          .map(_.onProblemHandle(problem => logger.warn(s"Shutdown: $problem")))
          .*>(Task.defer {
            logger.info(s"$subagent stopped")
            terminated.complete(ProgramTermination(restart = restart)).attempt.void
          }))
        .*>(untilTerminated)
    })
  }

  def executeDedicateSubagent(cmd: DedicateSubagent): Task[Checked[DedicateSubagent.Response]] =
    DedicatedSubagent
      .resource(cmd.subagentId, subagentRunId, journal, cmd.agentPath, cmd.controllerId,
        jobLauncherConf, conf)
      .toAllocated
      .flatMap(allocatedDedicatedSubagent => Task.defer {
        val isFirst = dedicatedAllocated.trySet(allocatedDedicatedSubagent)
        if (!isFirst) {
          // TODO Idempotent: Frisch gewidmeter Subagent ist okay. Kein Kommando darf eingekommen sein.
          //if (cmd.subagentId == dedicatedAllocated.orThrow.subagentId)
          //  Task.pure(Right(DedicateSubagent.Response(subagentRunId, EventId.BeforeFirst)))
          //else
          logger.warn(s"$cmd => $SubagentAlreadyDedicatedProblem: $dedicatedAllocated")
          Task.left(SubagentAlreadyDedicatedProblem)
        } else {
          // TODO Check agentPath, controllerId (handle in SubagentState?)
          logger.info(s"Subagent dedicated to be ${cmd.subagentId} in ${cmd.agentPath}, is ready")
          Task.right(
            DedicateSubagent.Response(subagentRunId, EventId.BeforeFirst, Some(Js7Version)))
        }
      })

  def startOrderProcess(
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression])
  : Task[Checked[Fiber[OrderProcessed]]] =
    Task(checkedDedicatedSubagent)
      .flatMapT(_.startOrderProcess(order, defaultArguments))

  def killProcess(orderId: OrderId, signal: ProcessSignal): Task[Checked[Unit]] =
    subagent.checkedDedicatedSubagent
      .traverse(_
        .killProcess(orderId, signal))

  def detachProcessedOrder(orderId: OrderId): Task[Checked[Unit]] =
    Task(checkedDedicatedSubagent)
      .flatMapT(_.detachProcessedOrder(orderId))

  def releaseEvents(eventId: EventId): Task[Checked[Unit]] =
    journal.releaseEvents(eventId)

  def subagentId: Option[SubagentId] =
    checkedDedicatedSubagent.toOption.map(_.subagentId)

  def isDedicated: Boolean =
    dedicatedAllocated.isDefined

  def checkIsDedicated: Checked[Unit] =
    dedicatedAllocated.checked.map(_ => ())

  private[subagent] def checkedDedicatedSubagent: Checked[DedicatedSubagent] =
    dedicatedAllocated.checked.map(_.allocatedThing)

  override def toString = s"Subagent(${checkedDedicatedSubagent.toOption getOrElse ""})"
}

object Subagent
{
  private val logger = Logger(getClass)

  def resource(
    conf: SubagentConf,
    js7Scheduler: Scheduler,
    iox: IOExecutor,
    testEventBus: StandardEventBus[Any])
  : Resource[Task, Subagent] =
    Resource.suspend(Task {
      import conf.config

      implicit val s: Scheduler = js7Scheduler
      val alarmClockCheckingInterval = config.finiteDuration("js7.time.clock-setting-check-interval")
        .orThrow
      val memoryJournalSize = config.getInt("js7.journal.in-memory.event-count")

      (for {
        // For BlockingInternalJob (thread-blocking Java jobs)
        blockingInternalJobScheduler <- unlimitedSchedulerResource[Task](
          "JS7 blocking job", conf.config)
        clock <- AlarmClock.resource[Task](Some(alarmClockCheckingInterval))
        journal = new MemoryJournal(SubagentState.empty,
          size = memoryJournalSize,
          waitingFor = "JS7 Agent Director")
        jobLauncherConf = conf.toJobLauncherConf(iox, blockingInternalJobScheduler, clock).orThrow
        signatureVerifier <- DirectoryWatchingSignatureVerifier.prepare(config)
          .orThrow
          .toResource(onUpdated = () => testEventBus.publish(ItemSignatureKeysUpdated))(iox)
        subagent <- Service.resource(Task(
          new Subagent(journal, signatureVerifier, conf, jobLauncherConf, testEventBus)))
      } yield subagent
      ).executeOn(js7Scheduler)
    })

  type ItemSignatureKeysUpdated = ItemSignatureKeysUpdated.type
  case object ItemSignatureKeysUpdated
}
