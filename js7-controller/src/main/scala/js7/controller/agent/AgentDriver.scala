package js7.controller.agent

import akka.actor.ActorSystem
import cats.effect.Resource
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import com.typesafe.config.ConfigUtil
import js7.agent.client.AgentClient
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.DedicateAgentDirector
import js7.base.auth.UserAndPassword
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.crypt.Signed
import js7.base.generic.{Completed, SecretString}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.AsyncVariable
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
import js7.data.agent.AgentRefStateEvent.{AgentCouplingFailed, AgentDedicated}
import js7.data.agent.Problems.AgentNotDedicatedProblem
import js7.data.agent.{AgentPath, AgentRef, AgentRunId}
import js7.data.cluster.ClusterWatchId
import js7.data.controller.ControllerState
import js7.data.event.{AnyKeyedEvent, EventId, KeyedEvent, Stamped}
import js7.data.item.ItemAttachedState.{Attachable, Attached}
import js7.data.item.{InventoryItemKey, ItemAttachedState, SignableItem, UnsignedItem}
import js7.data.node.NodeId
import js7.data.order.OrderEvent.{OrderAttachedToAgent, OrderDetached}
import js7.data.order.{Order, OrderId, OrderMark}
import js7.data.subagent.SubagentId
import js7.journal.state.StatePersistence
import monix.eval.Task
import monix.execution.atomic.AtomicInt
import monix.execution.{Cancelable, Scheduler}

final class AgentDriver private(
  initialAgentRef: AgentRef,
  initialAgentRunId: Option[AgentRunId],
  initialEventId: EventId,
  onEvents: (AgentRunId, Seq[Stamped[AnyKeyedEvent]]) => Task[Option[EventId]],
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
  private val clusterWatchId = ClusterWatchId(controllerId.string + "/" +
    controllerConfiguration.clusterConf.ownId.string + "/" + agentPath.string)

  private val agentRunIdOnce = SetOnce.fromOption(initialAgentRunId)
  private var isTerminating = false
  private val sessionNumber = AtomicInt(0) // Do we still need this ???
  @volatile private var lastCouplingFailed: Option[AgentCouplingFailed] = None
  private var noJournal = false
  private val directorDriverAllocated = AsyncVariable(
    null.asInstanceOf[Allocated[Task, DirectorDriver]])

  private object state {
    val lock = AsyncLock()
    var director: Option[SubagentId] = initialAgentRef.director
    /** Only filled when coupled */
    var lastFetchedEventId = initialEventId
    var lastCommittedEventId = initialEventId
    var releaseEventsCancelable: Option[Cancelable] = None
    var delayNextReleaseEvents = false
    //?var isReset = false
  }

  private def onCouplingFailed(problem: Problem): Task[Boolean] =
    Task.defer {
      val agentCouplingFailed = AgentCouplingFailed(problem)
      if (lastCouplingFailed contains agentCouplingFailed) {
        logger.debug(s"Coupling failed: $problem")
        Task.unit
      } else {
        lastCouplingFailed = Some(agentCouplingFailed)
        if (agentCouplingFailed.problem is InvalidSessionTokenProblem) {
          logger.debug(s"Coupling failed: $problem")
          Task.unit
        } else {
          logger.warn(s"Coupling failed: $problem")
          for (t <- problem.throwableOption if t.getStackTrace.nonEmpty) logger.debug(
            s"Coupling failed: $problem", t)
          Task.unless(noJournal)(
            persistence.persistKeyedEvent(agentPath <-: agentCouplingFailed)
              .map(_.orThrow))
        }
      }
    } *>
      Task(!isTerminating)

  private def onCoupled(attachedOrderIds: Set[OrderId]): Task[Unit] =
    Task.defer {
      assertThat(attachedOrderIds != null)
      onCoupled(attachedOrderIds)
      sessionNumber += 1
      state
        .lock.lock(Task {
          lastCouplingFailed = None
          state.delayNextReleaseEvents = false
        })
        .*>(commandQueue.onCoupled(attachedOrderIds))
        .<*(attachAttachbles)
        .as(Completed)
    }

  // TODO For v2.6 inserted, but maybe duplicate
  private def attachAttachbles: Task[Unit] =
    persistence.state.flatMap(controllerState => controllerState
      .itemToAgentToAttachedState
      .view
      .flatMap { case (itemKey, agentPathToAttachedState) =>
        agentPathToAttachedState
          .get(agentPath)
          .view
          .collect { case ItemAttachedState.Attachable => itemKey }
          .flatMap(controllerState.keyToItem.get)
          .collect {
            case item: UnsignedItem => Queueable.AttachUnsignedItem(item)
            //??? case signedItem: SignableItem => Queueable.AttachSignedItem(signedItem)
          }
      }
      .toVector
      .traverse(commandQueue.enqueue)
      .*>(commandQueue.maybeStartSending))

  private def onDecoupled =
    Task.defer {
      sessionNumber += 1
      commandQueue.onDecoupled()
    }

  private val commandQueue: CommandQueue = new CommandQueue(
    agentPath,
    batchSize = conf.commandBatchSize,
    conf.commandErrorDelay
  ) {
    protected def commandParallelism = conf.commandParallelism

    protected def executeCommand(command: AgentCommand.Batch) =
      logger.traceTask(Task.defer {
        val expectedSessionNumber = sessionNumber.get()
        useDirectorDriver(directorDriver =>
          // Fail on recoupling, later read restarted Agent's attached OrderIds before issuing again AttachOrder
          if (sessionNumber.get() != expectedSessionNumber)
            Task.left(DecoupledProblem)
          else
            directorDriver.executeCommand(command, mustBeCoupled = true))
      })

    protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]) =
      Task.defer {
        lastCouplingFailed = None
        handleBatchSucceeded(queuedInputResponses)
          .flatMap { succeededInputs =>
            val markedOrders = succeededInputs.view
              .collect { case o: Queueable.MarkOrder => o.orderId -> o.mark }
              .toMap
            Task
              .when(markedOrders.nonEmpty)(
                onOrderMarked(markedOrders))
              .*>(Task.defer {
                val releaseEvents = succeededInputs.collect {
                  case o: Queueable.ReleaseEventsQueueable => o
                }
                if (releaseEvents.nonEmpty) {
                  state.releaseEventsCancelable.foreach(_.cancel())
                  state.releaseEventsCancelable = None
                }
                stopIfTerminated
              })
          }
      }

    protected def asyncOnBatchFailed(queueables: Vector[Queueable], problem: Problem) =
      Task.defer(
        onBatchFailed(queueables, problem) *>
          startNewDirectorDriver
            .onErrorHandle(t => Task(logger.error(
              s"asyncOnBatchFailed => ${t.toStringWithCauses}", t)))
            .startAndForget)

    private def onBatchFailed(queueables: Seq[Queueable], problem: Problem): Task[Unit] =
      Task.defer {
        problem match {
          case DecoupledProblem |
               //InvalidSessionTokenProblem |
               RecouplingStreamReader.TerminatedProblem =>
            logger.debug(s"Command batch failed: $problem")
          case _ =>
            logger.warn(s"Command batch failed: $problem")
        }
        commandQueue.handleBatchFailed(queueables) *>
          stopIfTerminated
      }
  }

  protected def start =
    for {
      _ <- startNewDirectorDriver
      started <- startService(
        untilStopRequested
          .*>(Task {
            state.releaseEventsCancelable.foreach(_.cancel())
          })
          .*>(directorDriverAllocated.value.flatMap(_.stop)))
    } yield started

  def send(input: Queueable): Task[Unit] =
    logger.traceTask("send", input.toShortString)(Task.defer {
      if (isTerminating)
        Task.raiseError(new IllegalStateException(s"$agentDriver is terminating"))
      else
        commandQueue.enqueue(input)
          .flatMap(ok =>
            Task.when(ok)(Task
              .race(
                untilStopRequested,
                Task.sleep(conf.commandBatchDelay)/*TODO Set timer only for the first send*/)
              .flatMap {
                case Left(()) => Task.unit // stop requested
                case Right(()) => commandQueue.maybeStartSending
              }
              .onErrorHandle(t => Task(logger.error(
                s"send(${input.toShortString}) => ${t.toStringWithCauses}", t)))
              .startAndForget))
    })

  def changeAgentRef(agentRef: AgentRef): Task[Unit] =
    logger.traceTask(
      //directorDriverAllocated.use { directorDriverAllocated =>
        //val directorDriver = directorDriverAllocated.allocatedThing
        state.lock.lock(Task.defer {
          state.director = agentRef.director
          // TODO Restart DirectorDriver only if one of the URIs has changed
          //Task.when(uri != directorDriver.client.baseUri || state.isReset) {
            //logger.debug(s"changeAgentRef $uri")
            //?state.isReset = false
            startNewDirectorDriver
              .onErrorHandle(t => Task(logger.error(
                s"startNewDirectorDriver => ${t.toStringWithCauses}", t)))
              .startAndForget/*Not locked*/
          }))

  def terminate(noJournal: Boolean = false, reset: Boolean = false): Task[Unit] =
    logger.traceTask(Task.defer {
      this.noJournal |= noJournal
      // Wait until all pending Agent commands are responded, and do not accept further commands
      Task.unless(isTerminating)(Task.defer {
        logger.debug(s"Terminate${noJournal ?? " noJournal"}${reset ?? " reset"}")
        isTerminating = true
        Task.when(reset)(
          agentRunIdOnce.toOption.fold(Task.unit)(agentRunId =>
            // Required only for ItemDeleted, redundant for ResetAgent
            resetAgent(Some(agentRunId)).void))
      })
    } *>
      stop)

  def reset(force: Boolean): Task[Checked[Unit]] =
    if (force)
      resetAgent(None)
    else
      Task.defer {
        agentRunIdOnce.toOption match {
          case None => Task.left(AgentNotDedicatedProblem /*Nothing to reset*/)
          case Some(agentRunId) => resetAgent(Some(agentRunId)).map(Right(_))
        }
      }

  private def resetAgent(agentRunId: Option[AgentRunId]): Task[Checked[Unit]] =
    logger.traceTask(
      directorDriverAllocated.value
        .map(_.allocatedThing)
        .flatMap(_.resetAgentAndStop(agentRunId)) // Stops the directorDriver, too
        .flatTapT(_ => stop.map(Right(_))))

  private def onEventsFetched(stampedEvents: Seq[Stamped[AnyKeyedEvent]], lastEventId: EventId): Task[Unit] = {
    assertThat(stampedEvents.nonEmpty)
    Task.defer {
      state.lastFetchedEventId = lastEventId
      commandQueue.onOrdersAttached(
        stampedEvents.view.collect {
          case Stamped(_, _, KeyedEvent(orderId: OrderId, _: OrderAttachedToAgent)) => orderId
        })
        .*>(
          commandQueue.onOrdersDetached(
            stampedEvents.view.collect {
              case Stamped(_, _, KeyedEvent(orderId: OrderId, OrderDetached)) => orderId
            }))
        .*>(onEvents(agentRunIdOnce.orThrow, stampedEvents))
        .flatMap(_.fold(Task.unit)(
          releaseEvents))
        .onErrorHandle(t =>
          logger.error(s"$agentDriver.onEvents => " + t.toStringWithCauses, t.nullIfNoStackTrace))
        .logWhenItTakesLonger(s"$agentDriver.onEvents")
    }
  }

  private def releaseEvents(lastEventId: EventId): Task[Unit] =
    state.lock.lock(Task {
      state.lastCommittedEventId = lastEventId
      if (state.releaseEventsCancelable.isEmpty) {
        val delay = if (state.delayNextReleaseEvents) conf.releaseEventsPeriod else ZeroDuration
        state.delayNextReleaseEvents = true
        state.releaseEventsCancelable = Some(scheduler.scheduleOnce(delay) {
          Task
            .unless(isTerminating)(
              commandQueue
                .enqueue(Queueable.ReleaseEventsQueueable(state.lastCommittedEventId)).void
                .onErrorHandle(t => Task(logger.error(t.toStringWithCauses, t))))
            .runAsyncAndForget
        })
      }
    })

  private def dedicateAgentIfNeeded: Task[Checked[(AgentRunId, EventId)]] =
    logger.traceTask(Task.defer {
      agentRunIdOnce.toOption match {
        case Some(agentRunId) => Task.right(agentRunId -> state.lastFetchedEventId)
        case None =>
          persistence.state
            .map(_.keyToItem(AgentRef).checked(agentPath))
            .flatMapT(agentRef =>
              useDirectorDriver(directorDriver =>
                directorDriver
                  .executeCommand(
                    DedicateAgentDirector(agentRef.director, controllerId, agentPath))
                  .flatMapT { case DedicateAgentDirector.Response(agentRunId, agentEventId) =>
                    (if (noJournal)
                      Task.pure(Checked.unit)
                    else
                      persistence
                        .persistKeyedEvent(
                          agentPath <-: AgentDedicated(agentRunId, Some(agentEventId)))
                        .flatMapT { _ =>
                          agentRunIdOnce := agentRunId
                          reattachSubagents().map(Right(_))
                        })
                    .rightAs(agentRunId -> agentEventId)
                  }))
      }
    })

  private def reattachSubagents(): Task[Unit] =
    persistence.state.flatMap(controllerState =>
      controllerState
        .itemToAgentToAttachedState
        .toVector
        .flatMap {
          case (subagentId: SubagentId, agentToAttachedState) =>
            // After Agent Reset, re-attach SubagentItems
            controllerState.pathToUnsignedSimpleItem.get(subagentId)
              .flatMap(item => agentToAttachedState.get(agentPath).map(item -> _))
          case _ => Nil
        }
        .traverse {
          case (item, Attachable) =>
            send(Queueable.AttachUnsignedItem(item))

          case (item, Attached(rev)) =>
            Task.when(item.itemRevision == rev) {
              send(Queueable.AttachUnsignedItem(item))
            }

          case _ =>
            Task.unit
        }.map(_.combineAll))

  private def stopIfTerminated: Task[Unit] = // ???
    Task.unit
    //Task(if (commandQueue.isTerminated) {
    //  logger.debug("Stop")
    //  //state.currentFetchedFuture.foreach(_.cancel())
    //  state.releaseEventsCancelable.foreach(_.cancel())
    //  //Task.fromFuture(eventFetcherTerminated.future)
    //  //  .onErrorHandle(t => logger.debug(t.toStringWithCauses))
    //  //  .*>(stopThis.startAndForget/*TODO*/)
    //})

  private def startNewDirectorDriver: Task[Unit] =
    logger.debugTask(
      directorDriverAllocated
        .update(allocated =>
          Task.when(allocated != null)(allocated.stop) *>
            directorDriverResource.toAllocated)
        .void)

  private def directorDriverResource: Resource[Task, DirectorDriver] =
    for {
      clients <- clientsResource
      _ <- clusterWatchResource(clients)
      directorDriver <- DirectorDriver.resource(
        agentPath, state.lastFetchedEventId, clients.head/*TODO active cluster node*/,
        dedicateAgentIfNeeded, onCouplingFailed, onCoupled, onDecoupled,
        onEventsFetched,
        persistence, conf)
    } yield directorDriver

  private def clusterWatchResource(clients: Nel[AgentClient]): Resource[Task, ClusterWatchService] =
    ClusterWatchService.resource(
      clusterWatchId,
      Resource.eval(Task.pure(clients)),
      controllerConfiguration.config)

  // TODO Detect active and passive node !
  private def clientsResource: Resource[Task, Nel[AgentClient]] =
    Resource
      .eval(persistence.state.map(_.agentToUris(agentPath)))
      .flatMap(_.traverse(clientResource))

  private def clientResource(uri: Uri): Resource[Task, AgentClient] =
    Resource.make(
      acquire = Task {
        val agentUserAndPassword = controllerConfiguration.config
          .optionAs[SecretString]("js7.auth.agents." + ConfigUtil.joinPath(agentPath.string))
          .map(password => UserAndPassword(controllerConfiguration.controllerId.toUserId, password))
        AgentClient(uri, agentUserAndPassword, label = agentPath.toString,
          controllerConfiguration.httpsConfig)(actorSystem)
      })(
      release = client => Task(client.close()))

  private def useDirectorDriver[A](body: DirectorDriver => Task[A])
    (implicit src: sourcecode.Enclosing)
  : Task[A] =
    directorDriverAllocated.use(allocated => body(allocated.allocatedThing))

  override def toString = s"AgentDriver($agentPath)"
}

private[controller] object AgentDriver
{
  def resource(
    agentRef: AgentRef, agentRunId: Option[AgentRunId], eventId: EventId,
    onEvents: (AgentRunId, Seq[Stamped[AnyKeyedEvent]]) => Task[Option[EventId]],
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

  private[agent] val DecoupledProblem = Problem.pure("Agent is not coupled")
}
