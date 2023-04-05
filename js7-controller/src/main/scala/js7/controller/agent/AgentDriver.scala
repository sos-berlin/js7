package js7.controller.agent

import akka.actor.ActorSystem
import cats.data.EitherT
import cats.effect.Resource
import cats.syntax.flatMap.*
import com.typesafe.config.ConfigUtil
import js7.agent.client.AgentClient
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{CoupleController, DedicateAgentDirector}
import js7.agent.data.event.AgentEvent
import js7.base.auth.UserAndPassword
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.crypt.Signed
import js7.base.generic.{Completed, SecretString}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked.*
import js7.base.problem.Problems.InvalidSessionTokenProblem
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, AsyncLock, SetOnce}
import js7.base.web.Uri
import js7.cluster.watch.ClusterWatchService
import js7.common.http.RecouplingStreamReader
import js7.controller.agent.AgentDriver.*
import js7.controller.agent.CommandQueue.QueuedInputResponse
import js7.controller.configuration.ControllerConfiguration
import js7.data.agent.AgentRefStateEvent.{AgentCoupled, AgentCouplingFailed, AgentDedicated, AgentReset}
import js7.data.agent.Problems.AgentNotDedicatedProblem
import js7.data.agent.{AgentPath, AgentRef, AgentRefState, AgentRunId}
import js7.data.cluster.ClusterWatchId
import js7.data.controller.ControllerState
import js7.data.delegate.DelegateCouplingState.{Coupled, Resetting}
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, KeyedEvent, Stamped}
import js7.data.item.ItemAttachedState.{Attachable, Attached}
import js7.data.item.{InventoryItemEvent, InventoryItemKey, SignableItem, UnsignedItem}
import js7.data.node.NodeId
import js7.data.order.OrderEvent.{OrderAttachedToAgent, OrderDetached}
import js7.data.order.{Order, OrderEvent, OrderId, OrderMark}
import js7.data.orderwatch.OrderWatchEvent
import js7.data.subagent.{SubagentId, SubagentItemStateEvent}
import js7.journal.state.StatePersistence
import monix.eval.Task
import monix.execution.atomic.AtomicInt
import monix.execution.{Cancelable, CancelableFuture, Scheduler}
import monix.reactive.Observable
import scala.concurrent.Promise
import scala.util.chaining.scalaUtilChainingOps
import scala.util.control.NoStackTrace

final class AgentDriver private(
  initialAgentRef: AgentRef,
  initialAgentRunId: Option[AgentRunId],
  initialEventId: EventId,
  onEvents: (AgentRunId, Seq[Stamped[AnyKeyedEvent]]) => Task[Option[EventId]],  // TODO Stream
  onOrderMarked: Map[OrderId, OrderMark] => Task[Unit],
  persistence: StatePersistence[ControllerState],
  conf: AgentDriverConfiguration,
  controllerConfiguration: ControllerConfiguration,
  actorSystem: ActorSystem)
  (implicit protected val scheduler: Scheduler)
extends Service.StoppableByRequest
{
  agentDriver =>

  import controllerConfiguration.controllerId

  private val agentPath = initialAgentRef.path
  private val logger = Logger.withPrefix[this.type](agentPath.string)
  private val agentUserAndPassword: Option[UserAndPassword] =
    controllerConfiguration.config
      .optionAs[SecretString]("js7.auth.agents." + ConfigUtil.joinPath(agentPath.string))
      .map(password => UserAndPassword(controllerConfiguration.controllerId.toUserId, password))

  private val clusterWatchId = ClusterWatchId(controllerId.string + "/" +
    controllerConfiguration.clusterConf.ownId.string + "/" + agentPath.string)

  private val agentRunIdOnce = SetOnce.fromOption(initialAgentRunId)
  private var isTerminating = false
  private val eventFetcherTerminated = Promise[Unit]()
  private val sessionNumber = AtomicInt(0)
  @volatile private var lastCouplingFailed: Option[AgentCouplingFailed] = None
  private var noJournal = false

  private object state {
    val lock = AsyncLock()
    var director: Option[SubagentId] = initialAgentRef.director
    var client = newAgentClient(persistence.unsafeCurrentState().agentToUri(agentPath)
      .getOrElse(Uri(s"unknown-uri://$agentPath"/*should not happen ???*/)))
    var passiveClient = persistence.unsafeCurrentState().agentToUris(agentPath)
      .tail.headOption.map(newAgentClient(_))
    /** Only filled when coupled */
    var lastFetchedEventId = initialEventId
    var lastCommittedEventId = initialEventId
    @deprecated var currentFetchedFuture: Option[CancelableFuture[Completed]] = None
    var releaseEventsCancelable: Option[Cancelable] = None
    var delayNextReleaseEvents = false
    var changingUri: Option[Uri] = None
    var isReset = false
    var allocatedClusterWatchService: Option[Allocated[Task, ClusterWatchService]] = None
  }

  private val eventFetcher = new RecouplingStreamReader[EventId, Stamped[AnyKeyedEvent], AgentClient](
    _.eventId, conf.recouplingStreamReader)
  {
    private var attachedOrderIds: Set[OrderId] = null

    override protected def couple(eventId: EventId) =
      Task(persistence.unsafeCurrentState().keyTo(AgentRefState).checked(agentPath))
        .flatMapT(agentRefState =>
          ((agentRefState.couplingState, agentRefState.agentRunId) match {
            case (Resetting(false), None) =>
              Task.pure(Left(Problem.pure("Resetting, but no AgentRunId?")))  // Invalid state

            case (Resetting(force), maybeAgentRunId) if force || maybeAgentRunId.isDefined =>
              state.client
                .retryIfSessionLost()(
                  state.client.commandExecute(AgentCommand.Reset(maybeAgentRunId)))
                .map {
                  case Left(AgentNotDedicatedProblem) => Checked.unit  // Already reset
                  case o => o
                }
                .flatMapT(_ =>
                  persistence.persistKeyedEvent(agentPath <-: AgentReset))

            case _ =>
              Task.pure(Checked.unit)
          })
        .flatMapT(_ => dedicateAgentIfNeeded)
        .flatMapT { case (agentRunId, agentEventId) =>
          state.client.commandExecute(CoupleController(agentPath, agentRunId, eventId = agentEventId))
            .flatMapT { case CoupleController.Response(orderIds) =>
              logger.trace(s"CoupleController returned attached OrderIds={${orderIds.toSeq.sorted.mkString(" ")}}")
              attachedOrderIds = orderIds
              persistence
                .lock(agentPath)(
                  persistence.persist(controllerState =>
                    for (a <- controllerState.keyTo(AgentRefState).checked(agentPath)) yield
                      (a.couplingState != Coupled || a.problem.nonEmpty)
                        .thenList(agentPath <-: AgentCoupled)))
                .rightAs(agentEventId)
            }
        })

    protected def getObservable(api: AgentClient, after: EventId) =
      Task { logger.debug(s"getObservable(after=$after)") } *>
        api.eventObservable(EventRequest[Event](EventClasses, after = after,
          timeout = Some(requestTimeout)))

    override protected def onCouplingFailed(api: AgentClient, problem: Problem) =
      Task.defer {
        val agentCouplingFailed = AgentCouplingFailed(problem)
        if (lastCouplingFailed contains agentCouplingFailed) {
          logger.debug(s"Coupling failed: $problem")
          Task.pure(!isTerminating)
        } else {
          lastCouplingFailed = Some(agentCouplingFailed)
          if (agentCouplingFailed.problem is InvalidSessionTokenProblem) {
            logger.debug(s"Coupling failed: $problem")
            Task.pure(!isTerminating)
          } else {
            logger.warn(s"Coupling failed: $problem")
            for (t <- problem.throwableOption if t.getStackTrace.nonEmpty) logger.debug(s"Coupling failed: $problem", t)
            if (noJournal)
              Task.pure(!isTerminating)
            else
              persistence.persistKeyedEvent(agentPath <-: agentCouplingFailed)
                .map(_.map(_ => !isTerminating))  // recouple and continue after onCouplingFailed
                .map(_.orThrow)
          }
        }
      }

    override protected def onCoupled(api: AgentClient, after: EventId) =
      Task.defer {
        logger.info(s"Coupled with $api after=${EventId.toString(after)}")
        sessionNumber += 1
        assertThat(attachedOrderIds != null)
        state.lock
          .lock(Task {
            lastCouplingFailed = None
            state.delayNextReleaseEvents = false
          })
          .*>(commandQueue.onCoupled(attachedOrderIds))
          .*>(Task {
            attachedOrderIds = null
          })
          .as(Completed)
      }

    override protected def onDecoupled =
      Task.defer {
        logger.debug("onDecoupled")
        sessionNumber += 1
        commandQueue.onDecoupled()
          .as(Completed)
      }

    protected def stopRequested = false
  }

  private val commandQueue: CommandQueue = new CommandQueue(
    agentPath,
    batchSize = conf.commandBatchSize,
    conf.commandErrorDelay
  ) {
    protected def commandParallelism = conf.commandParallelism

    protected def executeCommand(command: AgentCommand.Batch) = logger.traceTask {
      val expectedSessionNumber: Int = sessionNumber.get()
      for {
        checkedApi <- eventFetcher.coupledApi.map(_.toRight(DecoupledProblem))
        response <-
          (for {
            // Fail on recoupling, later read restarted Agent's attached OrderIds before issuing again AttachOrder
            api <- EitherT(Task.pure(checkedApi))
            response <- EitherT(
              if (sessionNumber.get() != expectedSessionNumber)
                Task.left(DecoupledProblem)
              else
                // TODO Still a small possibility for race-condition? May log a AgentDuplicateOrder
                api.commandExecute(command))
          } yield response).value
      } yield response.map(_.asInstanceOf[AgentCommand.Batch.Response]/*???*/)
    }

    protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]) =
      Task.defer {
        lastCouplingFailed = None
        handleBatchSucceeded(queuedInputResponses)
          .flatMap(succeededInputs => Task.defer {
            val markedOrders = succeededInputs.view
              .collect { case o: Queueable.MarkOrder => o.orderId -> o.mark }
              .toMap
            Task
              .when(markedOrders.nonEmpty)(
                onOrderMarked(markedOrders))
              .*>(Task.defer {
                val releaseEvents = succeededInputs collect { case o: Queueable.ReleaseEventsQueueable => o }
                if (releaseEvents.nonEmpty) {
                  state.releaseEventsCancelable foreach (_.cancel())
                  state.releaseEventsCancelable = None
                }
                stopIfTerminated
              })
          })
      }

    protected def asyncOnBatchFailed(inputs: Vector[Queueable], problem: Problem) =
      Task.defer(
        if (problem == DecoupledProblem/*avoid loop*/ || isTerminating) {
          onBatchFailed(inputs, problem)
        } else
          eventFetcher.invalidateCoupledApi
            .*>(Task { state.currentFetchedFuture.foreach(_.cancel()) })
            .*>(cancelObservationAndAwaitTermination)
            .*>(eventFetcher.decouple)
            .*>(onBatchFailed(inputs, problem))
            .tapError(t => Task {
              logger.error("asyncOnBatchFailed: " + t.toStringWithCauses, t.nullIfNoStackTrace)
            })
      )

    private def onBatchFailed(inputs: Seq[Queueable], problem: Problem): Task[Unit] =
      Task.defer {
        problem match {
          case DecoupledProblem |
               InvalidSessionTokenProblem |
               RecouplingStreamReader.TerminatedProblem =>
            logger.debug(s"Command batch failed: $problem")
          case _ =>
            logger.warn(s"Command batch failed: $problem")
        }
        commandQueue.handleBatchFailed(inputs) *>
          stopIfTerminated
      }
  }

  protected def start =
    startClusterWatch *>
      startService(
        untilStopRequested
          .*>(Task(eventFetcher.markAsStopped()))
          .*>(eventFetcher.terminateAndLogout)
          .*>(Task(state.currentFetchedFuture.foreach(_.cancel())))
          .*>(state.allocatedClusterWatchService.fold(Task.unit)(_.stop)))

  private def stopThis: Task[Unit] =
    stop // ???

  private def newAgentClient(uri: Uri): AgentClient =
    AgentClient(uri, agentUserAndPassword, label = agentPath.toString,
      controllerConfiguration.httpsConfig)(actorSystem)

  def send(input: Queueable): Task[Unit] =
    logger.traceTask("send", input.toShortString)(Task.defer {
      if (isTerminating)
        Task.raiseError(new IllegalStateException(s"$toString is terminating"))
      else
        commandQueue.enqueue(input)
          .flatMap(ok =>
            Task.when(ok)(
              // (Even with commandBatchDelay == 0) delay maySend() such that Queueable pending
              // in actor's mailbox can be queued
              // Send command when stop requested — or cancel the timer ???
              untilStopRequested
                // TODO Set timer only for the first send!
                .timeoutTo(conf.commandBatchDelay, Task.unit)
                .*>(commandQueue.maySend)
                .onErrorHandle(t => Task(logger.error(
                  s"send(${input.toShortString}) => ${t.toStringWithCauses}", t)))
                .startAndForget))
    })

  def changeUri(agentRef: AgentRef, uri: Uri): Task[Unit] =
    logger.traceTask(state.lock.lock(Task.defer {
      state.director = agentRef.director
      if (uri != state.client.baseUri || state.isReset) {
        logger.debug(s"changeUri $uri")
        // TODO Changing URI in quick succession is not properly solved
        state.isReset = false
        for (u <- state.changingUri) logger.warn(s"Already changing URI to $u ?")
        state.changingUri = Some(uri)
        cancelObservationAndAwaitTermination
          .*>(eventFetcher.decouple)
          .onErrorHandle(t =>
            logger.error("changeUri => " + t.toStringWithCauses, t.nullIfNoStackTrace))
          .startAndForget
      } else
        Task.unit
    }))

  private def onUriChanged: Task[Unit] =
    logger.traceTask(state.lock.lock(Task {
      for (uri <- state.changingUri) {
        state.client.close()
        logger.debug(s"new AgentClient($uri)")
        state.client = newAgentClient(uri)
        stopClusterWatch
          .flatMap(_ => startClusterWatch)
          .void
          .attempt
          .flatMap { tried =>
            state.changingUri = None
            tried
              .match_ {
                case Left(throwable) =>
                  logger.error(s"Internal.UriChanged($uri) => ${throwable.toStringWithCauses}",
                    throwable.nullIfNoStackTrace)
                  stopThis
                case Right(()) => Task.unit
              }
              .*>(startFetchingEvents)
          }
          .onErrorHandle(t => Task(logger.error(t.toStringWithCauses, t)))
          .runAsyncAndForget // TODO
      }
    }))

  def terminate(noJournal: Boolean = false, reset: Boolean = false): Task[Unit] =
    logger.traceTask(state.lock.lock(Task.defer {
      this.noJournal |= noJournal
      // Wait until all pending Agent commands are responded, and do not accept further commands
      Task.unless(isTerminating) {
        logger.debug(s"Terminate" + (noJournal ?? " noJournal") + (reset ?? " reset"))
        isTerminating = true
        commandQueue.terminate.*>(Task.defer {
          state.currentFetchedFuture.foreach(_.cancel())
          eventFetcherTerminated.completeWith(
            eventFetcher.terminateAndLogout.runToFuture) // Rejects current commands waiting for coupling
          agentRunIdOnce.toOption match {
            case Some(agentRunId) if reset =>
              // Required only for ItemDeleted, redundant for ResetAgent
              // Because of terminateAndLogout, we must login again to issue the Reset command
              Task
                .fromFuture(eventFetcherTerminated.future)
                .logWhenItTakesLonger(s"$agentPath whenEventFetcherTerminated")
                .onErrorHandle(_ => ())
                .*>(state.client
                  .retryIfSessionLost()(
                    state.client.commandExecute(AgentCommand.Reset(Some(agentRunId))))
                  .raceWith(untilStopRequested)
                  .map(_.fold(identity, _ => Left(Problem("AgentDriver is stopping"))))
                  .materializeIntoChecked
                  .map {
                    case Left(problem) => logger.error(s"Reset command failed: $problem")
                    case Right(_) =>
                  })
                .*>(
                  state.lock.lock(
                    // Ignore any Problem or Exception from Reset command
                    stopIfTerminated))
                .onErrorHandle(t => Task(logger.error(t.toStringWithCauses, t)))
                .startAndForget // TODO

            case _ =>
              stopIfTerminated
                .onErrorHandle(t => Task(logger.error(t.toStringWithCauses, t)))
                .startAndForget // TODO
          }
        })
      }
    }))

  def reset(force: Boolean): Task[Boolean] =
    logger.traceTask(/*lock.lock*/(Task.defer {
      state.isReset = true
      commandQueue.reset
        .*>(eventFetcher.coupledApi)
        .flatMap {
          case None => Task.pure(false)
          case Some(api) =>
            api
              .retryIfSessionLost()(
                api.commandExecute(AgentCommand.Reset(!force ? agentRunIdOnce.orThrow)))
              .materializeIntoChecked
              .map {
                case Left(problem) =>
                  logger.warn(s"Reset: $problem")
                  false
                case Right(_) =>
                  true
              }
        }
        .<*(eventFetcher.terminateAndLogout)
        .<*(Task(state.currentFetchedFuture.foreach(_.cancel())))
        .<*(cancelObservationAndAwaitTermination)
        .<*(eventFetcher.decouple)
        .<*(stopThis
          .onErrorHandle(t =>
            logger.error("stopThis => " + t.toStringWithCauses, t.nullIfNoStackTrace))
          .startAndForget/*TODO*/)
    }))

  def startFetchingEvents: Task[Unit] =
    logger.traceTask(state.lock.lock(Task {
      assertThat(state.currentFetchedFuture.isEmpty, "Duplicate fetchEvents")
      state.currentFetchedFuture = Some(
        observeAndConsumeEvents
          .onCancelRaiseError(CancelledMarker)
          .onErrorHandle(t => Task(logger.error(t.toStringWithCauses, t)))
          .guarantee(onFetchFinished)
          .onCancelRaiseError(CancelledMarker)
          .as(Completed)
          .runToFuture)
    }))

  private def onFetchFinished: Task[Unit] =
    logger.traceTask("onFetchFinished")(/*lock.lock???*/(Task.defer {
      // Message is expected only after ChangeUri or after InvalidSessionTokenProblem while executing a command
      state.currentFetchedFuture = None
      eventFetcher.decouple
        .*>(eventFetcher.pauseBeforeNextTry(conf.recouplingStreamReader.delay))
        .materialize
        .onErrorHandle(t => Task(logger.error(t.toStringWithCauses, t)))
        .*>(Task.defer(Task
          .unless(isTerminating)(
            if (state.changingUri.isDefined)
              onUriChanged
            else
              startFetchingEvents)))
        .onErrorHandle(t =>
          logger.error("onFetchFinished => " + t.toStringWithCauses, t.nullIfNoStackTrace))
        .startAndForget /*TODO*/
    }))

  private def onEventsFetched(stampedEvents: Seq[Stamped[AnyKeyedEvent]]): Task[Unit] = {
    assertThat(stampedEvents.nonEmpty)
    val reducedStampedEvents = stampedEvents dropWhile { stamped =>
      val drop = stamped.eventId <= state.lastFetchedEventId
      if (drop) logger.debug(s"Drop duplicate received event: $stamped")
      drop
    }
    val lastEventId = stampedEvents.last.eventId
    state.lock
      .lock(Task {
        // The events must be journaled and handled by ControllerOrderKeeper
        state.lastFetchedEventId = lastEventId
      })
      .*>(commandQueue.onOrdersAttached(
        reducedStampedEvents.view.collect {
          case Stamped(_, _, KeyedEvent(orderId: OrderId, _: OrderAttachedToAgent)) => orderId
        }))
      .*>(commandQueue.onOrdersDetached(
        reducedStampedEvents.view.collect {
          case Stamped(_, _, KeyedEvent(orderId: OrderId, OrderDetached)) => orderId
        }))
      .*>(onEvents(agentRunIdOnce.orThrow, reducedStampedEvents))
      .flatMap(_.fold(Task.unit)(
        releaseEvents))
      .onErrorHandle(t =>
        logger.error(s"$agentDriver.onEvents => " + t.toStringWithCauses, t.nullIfNoStackTrace))
      .logWhenItTakesLonger(s"$agentDriver.onEvents")
  }

  private def releaseEvents(lastEventId: EventId): Task[Unit] =
    state.lock.lock(Task {
      state.lastCommittedEventId = lastEventId
      if (state.releaseEventsCancelable.isEmpty) {
        val delay = if (state.delayNextReleaseEvents) conf.releaseEventsPeriod else ZeroDuration
        state.releaseEventsCancelable = Some(scheduler.scheduleOnce(delay) {
          Task.unless(isTerminating)(
            commandQueue.enqueue(Queueable.ReleaseEventsQueueable(state.lastCommittedEventId)).void)
            .onErrorHandle(t => Task(logger.error(t.toStringWithCauses, t)))
            .runAsyncAndForget
        })
        state.delayNextReleaseEvents = true
      }
    })

  private def cancelObservationAndAwaitTermination: Task[Completed] =
    logger.traceTask(Task.defer {
      state.currentFetchedFuture match {
        case None => Task.completed
        case Some(fetchedFuture) =>
          fetchedFuture.cancel()
          Task.fromFuture(fetchedFuture)
            .onErrorRecover { case t =>
              if (t ne CancelledMarker) logger.warn(t.toStringWithCauses)
              Completed
            }
      }
    })

  private def observeAndConsumeEvents: Task[Completed] =
    logger.traceTask(Task.defer {
      val delay = conf.eventBufferDelay max conf.commitDelay
      eventFetcher.observe(state.client, after = state.lastFetchedEventId)
        .pipe(obs =>
          if (delay.isZeroOrBelow)
            obs.bufferIntrospective(conf.eventBufferSize)
          else obs
            .buffer(
              Some(conf.eventBufferDelay max conf.commitDelay),
              maxCount = conf.eventBufferSize)  // ticks
            .filter(_.nonEmpty))   // Ignore empty ticks
        .flatMap(o => Observable
          // When the other cluster node may have failed-over,
          // wait until we know that it hasn't (or this node is aborted).
          // Avoids "Unknown OrderId" failures due to double activation.
          .fromTask(persistence.whenNoFailoverByOtherNode
            .logWhenItTakesLonger("whenNoFailoverByOtherNode"))
          .map(_ => o))
        .mapEval(onEventsFetched)
        .completedL
        .map(_ => Completed)
    })

  private def dedicateAgentIfNeeded: Task[Checked[(AgentRunId, EventId)]] =
    logger.traceTask(Task.defer {
      agentRunIdOnce.toOption match {
        case Some(agentRunId) => Task.pure(Right(agentRunId -> state.lastFetchedEventId))
        case None =>
          state.client
            .commandExecute(
              DedicateAgentDirector(state.director, controllerId, agentPath))
            .flatMapT { case DedicateAgentDirector.Response(agentRunId, agentEventId) =>
              (if (noJournal)
                Task.pure(Checked.unit)
              else
                persistence
                  .persistKeyedEvent(
                    agentPath <-: AgentDedicated(agentRunId, Some(agentEventId)))
                  .map(_.map { _ =>
                    // Asynchronous assignment
                    agentRunIdOnce := agentRunId
                    reattachSubagents()
                  }))
              .rightAs(agentRunId -> agentEventId)
            }
      }
    })

  private def reattachSubagents(): Unit = {
    val controllerState = persistence.unsafeCurrentState()
    controllerState.itemToAgentToAttachedState
      .foreach {
        case (subagentId: SubagentId, agentToAttachedState) =>
          // After Agent Reset, re-attach SubagentItems
          controllerState.pathToUnsignedSimpleItem.get(subagentId)
            .foreach(item => agentToAttachedState.get(agentPath)
              .foreach {
                case Attachable =>
                  send(Queueable.AttachUnsignedItem(item))
                    .onErrorHandle(t => Task(logger.error(t.toStringWithCauses, t)))
                    .runAsyncAndForget // TODO

                case Attached(rev) =>
                  if (item.itemRevision == rev) {
                    send(Queueable.AttachUnsignedItem(item))
                      .onErrorHandle(t => Task(logger.error(t.toStringWithCauses, t)))
                      .runAsyncAndForget // TODO
                  }

                case _ =>
              })

        case _ =>
      }
  }

  private def stopIfTerminated: Task[Unit] =
    Task.defer(Task.when(commandQueue.isTerminated) {
      logger.debug("Stop")
      state.currentFetchedFuture.foreach(_.cancel())
      state.releaseEventsCancelable.foreach(_.cancel())
      Task.fromFuture(eventFetcherTerminated.future)
        .onErrorHandle(t => logger.debug(t.toStringWithCauses))
        .*>(closeClient)
        .*>(stopClusterWatch)
        .onErrorHandle(t =>
          logger.error("stopIfTerminated => " + t.toStringWithCauses, t.nullIfNoStackTrace))
        .*>(stopThis.startAndForget/*TODO*/)
    })

  private def closeClient: Task[Unit] =
    eventFetcher.invalidateCoupledApi
      .materialize
      .flatMap(_ => state.client.tryLogout.void)
      .guarantee(Task {
        state.client.close()
      })

  private def startClusterWatch: Task[Allocated[Task, ClusterWatchService]] =
    Task.defer {
      assertThat(state.allocatedClusterWatchService.isEmpty)
      ClusterWatchService
        .resource(
          clusterWatchId,
          Resource.eval(Task.pure(Nel.fromListUnsafe(
            state.client :: state.passiveClient.toList))),
          controllerConfiguration.config)
        .toAllocated
        .flatTap(service => Task {
          state.allocatedClusterWatchService = Some(service)
        })
    }

  private def stopClusterWatch: Task[Unit] =
    Task.defer(state.allocatedClusterWatchService.fold(Task.unit)(_
      .stop
      .guarantee(Task {
        state.allocatedClusterWatchService = None
      })))

  override def toString = s"AgentDriver($agentPath)"
}

private[controller] object AgentDriver
{
  private val EventClasses = Set[Class[? <: Event]](
    classOf[OrderEvent],
    classOf[AgentEvent.AgentReady],
    AgentEvent.AgentShutDown.getClass,
    classOf[SubagentItemStateEvent],
    classOf[InventoryItemEvent],
    classOf[OrderWatchEvent])

  private val DecoupledProblem = Problem.pure("Agent has been decoupled")

  def resource(
    agentRef: AgentRef, agentRunId: Option[AgentRunId], eventId: EventId,
    onEvents: (AgentRunId, Seq[Stamped[AnyKeyedEvent]]) => Task[Option[EventId]], // TODO Stream
    onOrderMarked: Map[OrderId, OrderMark] => Task[Unit],
    persistence: StatePersistence[ControllerState],
    agentDriverConf: AgentDriverConfiguration, controllerConf: ControllerConfiguration,
    actorSystem: ActorSystem)
    (implicit s: Scheduler)
  : Resource[Task, AgentDriver] =
    Service.resource(Task(
      new AgentDriver(
        agentRef, agentRunId, eventId,
        onEvents, onOrderMarked,
        persistence, agentDriverConf, controllerConf, actorSystem)))

  sealed trait Queueable {
    def toShortString = toString
  }
  object Queueable {
    final case class AttachUnsignedItem(item: UnsignedItem)
    extends Queueable

    final case class AttachSignedItem(signed: Signed[SignableItem])
    extends Queueable

    final case class DetachItem(key: InventoryItemKey)
    extends Queueable

    final case class AttachOrder(order: Order[Order.IsFreshOrReady], agentPath: AgentPath)
    extends Queueable {
      override lazy val hashCode = order.id.hashCode

      def orderId = order.id
      override def toShortString =
        s"AttachOrder($orderId, ${order.workflowPosition}, ${order.state.getClass.simpleScalaName})"
    }

    final case class DetachOrder(orderId: OrderId) extends Queueable

    final case class MarkOrder(orderId: OrderId, mark: OrderMark) extends Queueable

    private[agent] final case class ReleaseEventsQueueable(agentEventId: EventId) extends Queueable

    final case class ResetSubagent(subagentId: SubagentId, force: Boolean) extends Queueable

    final case class ClusterAppointNodes(idToUri: Map[NodeId, Uri], activeId: NodeId) extends Queueable

    case object ClusterSwitchOver extends Queueable
  }

  private case object CancelledMarker extends Exception with NoStackTrace
}
