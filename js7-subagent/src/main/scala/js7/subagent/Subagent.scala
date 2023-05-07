package js7.subagent

import cats.effect.concurrent.Deferred
import cats.effect.{ExitCase, Resource}
import js7.base.Js7Version
import js7.base.configutils.Configs.RichConfig
import js7.base.eventbus.StandardEventBus
import js7.base.io.process.ProcessSignal
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.AsyncMap
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.service.{MainService, Service}
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.RightUnit
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, ProgramTermination, SetOnce}
import js7.common.system.ThreadPools.unlimitedSchedulerResource
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.event.EventId
import js7.data.event.KeyedEvent.NoKey
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.subagent.Problems.{SubagentAlreadyDedicatedProblem, SubagentIdMismatchProblem, SubagentIsShuttingDownProblem, SubagentNotDedicatedProblem, SubagentRunIdMismatchProblem}
import js7.data.subagent.SubagentCommand.{CoupleDirector, DedicateSubagent}
import js7.data.subagent.SubagentEvent.SubagentShutdown
import js7.data.subagent.{SubagentId, SubagentRunId, SubagentState}
import js7.data.value.expression.Expression
import js7.data.workflow.position.WorkflowPosition
import js7.journal.MemoryJournal
import js7.launcher.configuration.JobLauncherConf
import js7.subagent.Subagent.*
import js7.subagent.configuration.SubagentConf
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.Atomic

final class Subagent private(
  val journal: MemoryJournal[SubagentState],
  val conf: SubagentConf,
  jobLauncherConf: JobLauncherConf,
  val testEventBus: StandardEventBus[Any])
extends MainService with Service.StoppableByRequest
{
  subagent =>

  private val dedicatedOnce = SetOnce[Dedicated](SubagentNotDedicatedProblem)
  val subagentRunId = SubagentRunId.fromJournalId(journal.journalId)
  private val subagentDriverConf = SubagentDriver.Conf.fromConfig(conf.config,
    commitDelay = 0.s)
  private val orderToProcessing = AsyncMap.stoppable[OrderId, Processing]()

  private val shuttingDown = Atomic(false)
  private val terminated = Deferred.unsafe[Task, ProgramTermination]
  @volatile private var _dontWaitForDirector = false

  protected def start =
    startService(Task
      .race(
        untilStopRequested *> shutdown(processSignal = None),
        untilTerminated)
      .void)

  def isShuttingDown: Boolean =
    shuttingDown()

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
      _dontWaitForDirector |= dontWaitForDirector
      val first = !shuttingDown.getAndSet(true)
      Task
        .when(first) {
          (dedicatedOnce
            .toOption
            .fold(Task.unit)(_.terminate(processSignal))
            .*>(orderToProcessing.initiateStopWithProblem(SubagentIsShuttingDownProblem))
            .*>(Task.defer {
              val orderIds = orderToProcessing.toMap.keys.toVector.sorted
              if (dontWaitForDirector) Task {
                for (orderId <- orderIds) logger.warn(
                  s"Shutdown: Agent Director has not yet acknowledged processing of $orderId")
              } else Task.defer {
                for (orderId <- orderIds) logger.info(
                  s"🟡 Delaying shutdown until Agent Director has acknowledged processing of $orderId")
                // Await process termination and DetachProcessedOrder commands
                orderToProcessing.whenStopped
                  .logWhenItTakesLonger("Director-acknowledged Order processes")
              }
            })
            .*>(journal
              // The event may get lost due to immediate shutdown !!!
              .persistKeyedEvent(NoKey <-: SubagentShutdown)
              .rightAs(())
              .map(_.onProblemHandle(problem => logger.warn(s"Shutdown: $problem"))))
            .*>(Task.defer {
              logger.info(s"$subagent stopped")
              terminated.complete(ProgramTermination(restart = restart)).attempt.void
            }))
        }
        .*>(untilTerminated)
    })
  }

  def executeDedicateSubagent(cmd: DedicateSubagent)
  : Task[Checked[DedicateSubagent.Response]] =
    allocateLocalSubagentDriver(cmd.subagentId, cmd.agentPath, cmd.controllerId)
      .flatMap(allocatedLocalSubagentDriver => Task.defer {
        val isFirst = dedicatedOnce.trySet(
          new Dedicated(allocatedLocalSubagentDriver))
        if (!isFirst) {
          // TODO Idempotent: Frisch gewidmeter Subagent ist okay. Kein Kommando darf eingekommen sein.
          //if (cmd.subagentId == dedicatedOnce.orThrow.subagentId)
          //  Task.pure(Right(DedicateSubagent.Response(subagentRunId, EventId.BeforeFirst)))
          //else
          logger.warn(s"$cmd => $SubagentAlreadyDedicatedProblem: $dedicatedOnce")
          Task.left(SubagentAlreadyDedicatedProblem)
        } else {
          // TODO Check agentPath, controllerId (handle in SubagentState?)
          logger.info(s"Subagent dedicated to be ${cmd.subagentId} in ${cmd.agentPath}, is ready")
          Task.right(
            DedicateSubagent.Response(subagentRunId, EventId.BeforeFirst, Some(Js7Version)))
        }
      })

  private def allocateLocalSubagentDriver(
    subagentId: SubagentId,
    agentPath: AgentPath,
    controllerId: ControllerId)
  : Task[Allocated[Task, LocalSubagentDriver[SubagentState]]] =
    LocalSubagentDriver
      .resource[SubagentState](
        subagentId,
        journal,
        agentPath,
        controllerId,
        jobLauncherConf,
        subagentDriverConf,
        conf)
      .toAllocated

  def executeCoupleDirector(cmd: CoupleDirector): Task[Checked[Unit]] =
    Task {
      for {
        _ <- checkSubagentId(cmd.subagentId)
        _ <- checkSubagentRunId(cmd.subagentRunId)
        _ <- journal.eventWatch.checkEventId(cmd.eventId)
      } yield ()
    }

  private def checkSubagentId(requestedSubagentId: SubagentId): Checked[Unit] =
    dedicatedOnce.checked.flatMap(dedicated =>
      if (requestedSubagentId != dedicated.subagentId)
        Left(SubagentIdMismatchProblem(requestedSubagentId, dedicated.subagentId))
      else
        RightUnit)

  def checkSubagentRunId(requestedSubagentRunId: SubagentRunId): Checked[Unit] =
    dedicatedOnce.checked.flatMap(dedicated =>
      if (requestedSubagentRunId != this.subagentRunId) {
        val problem = SubagentRunIdMismatchProblem(dedicated.subagentId)
        logger.warn(
          s"$problem, requestedSubagentRunId=$requestedSubagentRunId, " +
            s"agentRunId=${this.subagentRunId}")
        Left(problem)
      } else
        Checked.unit)

  def startOrderProcess(
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression])
  : Task[Checked[Unit]] =
    Task.defer {
      orderToProcessing
        .updateChecked(order.id, {
          case Some(existing) =>
            Task.pure(
              if (existing.workflowPosition != order.workflowPosition) {
                val problem = Problem.pure("Duplicate SubagentCommand.StartOrder with different Order position")
                logger.warn(s"$problem:")
                logger.warn(s"  Added order   : ${order.id} ${order.workflowPosition}")
                logger.warn(s"  Existing order: ${order.id} ${existing.workflowPosition}")
                Left(problem)
              } else
                Right(existing)) // Idempotency: Order process has already been started

          case None =>
            Task(dedicatedOnce.checked).flatMapT(dedicated =>
              processOrder(dedicated.localSubagentDriver, order, defaultArguments)
                .guaranteeCase {
                  case ExitCase.Completed =>
                    Task.unless(!_dontWaitForDirector) {
                      logger.warn(
                        s"dontWaitForDirector: ${order.id} <-: OrderProcessed event may get lost")
                      orderToProcessing.remove(order.id).void
                    }

                  case _ => orderToProcessing.remove(order.id).void // Tidy-up on failure
                }
                .startAndForget
                // TODO Asynchronous startOrderProcessing should not execute AFTER shutdown
                .as(Right(Processing(order.workflowPosition))))
        })
        .rightAs(())
    }

  private def processOrder(
    driver: LocalSubagentDriver[SubagentState],
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression])
  : Task[EventId] =
    driver
      .processOrder2(order, defaultArguments)
      .onErrorHandle(Outcome.Failed.fromThrowable)
      .flatMap { outcome =>
        val orderProcessed = order.id <-: OrderProcessed(outcome)
        journal
          .persistKeyedEvent(orderProcessed)
          .map(_.orThrow._1.eventId)
      }

  def detachProcessedOrder(orderId: OrderId): Task[Checked[Unit]] =
    orderToProcessing.remove(orderId).as(Checked.unit)

  def releaseEvents(eventId: EventId): Task[Checked[Unit]] =
    journal.releaseEvents(eventId)

  private[subagent] def checkedDedicated: Checked[Dedicated] =
    dedicatedOnce.checked

  def subagentId: Option[SubagentId] =
    dedicatedOnce.toOption.map(_.subagentId)

  override def toString = s"Subagent(${dedicatedOnce.toOption getOrElse ""})"
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
        subagent <- Service.resource(Task(
          new Subagent(journal, conf, jobLauncherConf, testEventBus)))
      } yield subagent
      ).executeOn(js7Scheduler)
    })

  private[subagent] final class Dedicated(
    val allocatedLocalSubagentDriver: Allocated[Task, LocalSubagentDriver[SubagentState]])
  {
    val localSubagentDriver: LocalSubagentDriver[SubagentState] =
      allocatedLocalSubagentDriver.allocatedThing

    val subagentId = localSubagentDriver.subagentId

    def terminate(signal: Option[ProcessSignal]) =
      localSubagentDriver.terminate(signal) *>
        allocatedLocalSubagentDriver.stop

    override def toString = {
      import localSubagentDriver.{agentPath, controllerId}
      s"Dedicated($subagentId $agentPath $controllerId)"
    }
  }

  private final case class Processing(
    workflowPosition: WorkflowPosition/*for check only*/)

  type ItemSignatureKeysUpdated = ItemSignatureKeysUpdated.type
  case object ItemSignatureKeysUpdated
}
