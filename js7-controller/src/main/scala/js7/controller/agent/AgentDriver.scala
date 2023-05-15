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
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked.*
import js7.base.problem.Problems.InvalidSessionTokenProblem
import js7.base.problem.{Checked, Problem, ProblemException}
import js7.base.service.Service
import js7.base.session.SessionApi
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{AsyncLock, MutableAllocated, SetOnce}
import js7.base.web.Uri
import js7.cluster.watch.ClusterWatchService
import js7.cluster.watch.api.ActiveClusterNodeSelector
import js7.common.http.RecouplingStreamReader
import js7.controller.agent.AgentDriver.*
import js7.controller.agent.CommandQueue.QueueableResponse
import js7.controller.configuration.ControllerConfiguration
import js7.data.agent.AgentRefStateEvent.{AgentCouplingFailed, AgentDedicated}
import js7.data.agent.Problems.AgentNotDedicatedProblem
import js7.data.agent.{AgentClusterConf, AgentPath, AgentRef, AgentRefState, AgentRunId}
import js7.data.cluster.ClusterState.HasNodes
import js7.data.cluster.ClusterWatchId
import js7.data.controller.ControllerState
import js7.data.event.{AnyKeyedEvent, EventId, KeyedEvent, Stamped}
import js7.data.item.ItemAttachedState.{Attachable, Attached}
import js7.data.item.{InventoryItemKey, ItemAttachedState, SignableItem, UnsignedItem}
import js7.data.node.NodeId
import js7.data.order.OrderEvent.{OrderAttachedToAgent, OrderDetached}
import js7.data.order.{Order, OrderId, OrderMark}
import js7.data.subagent.SubagentId
import js7.journal.state.Journal
import monix.eval.Task
import monix.execution.atomic.AtomicInt
import monix.execution.{Cancelable, Scheduler}

final class AgentDriver private(
  initialAgentRef: AgentRef,
  initialAgentRunId: Option[AgentRunId],
  initialEventId: EventId,
  onEvents: (AgentRunId, Seq[Stamped[AnyKeyedEvent]]) => Task[Option[EventId]],
  onOrderMarked: Map[OrderId, OrderMark] => Task[Unit],
  journal: Journal[ControllerState],
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
  private val clusterWatchId = ClusterWatchId(
    controllerId.string + "/" +
      controllerConfiguration.clusterConf.ownId.string + "/" +
      agentPath.string)

  private val agentRunIdOnce = SetOnce.fromOption(initialAgentRunId)
  private var isTerminating = false
  private val sessionNumber = AtomicInt(0) // Do we still need this ???
  @volatile private var lastCouplingFailed: Option[AgentCouplingFailed] = None
  private var noJournal = false
  private val clusterWatchAllocated = new MutableAllocated[ClusterWatchService]
  private val directorDriverAllocated = new MutableAllocated[DirectorDriver]
  private var clusterState: Option[HasNodes] = None

  private object state {
    val lock = AsyncLock()
    var directors: Seq[SubagentId] = initialAgentRef.directors
    /** Only filled when coupled */
    var lastFetchedEventId = initialEventId
    var lastCommittedEventId = initialEventId
    var releaseEventsCancelable: Option[Cancelable] = None
    var delayNextReleaseEvents = false
  }

  private def onCouplingFailed(problem: Problem): Task[Boolean] =
    Task.defer {
      // TODO Differentiate between the two Directors
      //  Lesser problem when the active one is reachable.
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
            journal.persistKeyedEvent(agentPath <-: agentCouplingFailed)
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
        .<*(attachAttachables)
        .as(Completed)
    }

  // TODO For v2.6 inserted, but maybe duplicate
  private def attachAttachables: Task[Unit] =
    journal.state.flatMap(controllerState => controllerState
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
        directorDriverAllocated.checked.flatMapT(directorDriver =>
          // Fail on recoupling, later read restarted Agent's attached OrderIds before issuing again AttachOrder
          if (sessionNumber.get() != expectedSessionNumber)
            Task.left(DecoupledProblem)
          else
            directorDriver.executeCommand(command, mustBeCoupled = true))
      })

    protected def asyncOnBatchSucceeded(queueableResponses: Seq[QueueableResponse]) =
      Task.defer {
        lastCouplingFailed = None
        handleBatchSucceeded(queueableResponses)
          .flatMap { succeededInputs =>
            val markedOrders = succeededInputs.view
              .collect { case o: Queueable.MarkOrder => o.orderId -> o.mark }
              .toMap
            Task
              .when(markedOrders.nonEmpty)(
                onOrderMarked(markedOrders))
              .*>(Task {
                val releaseEvents = succeededInputs.collect {
                  case o: Queueable.ReleaseEventsQueueable => o
                }
                if (releaseEvents.nonEmpty) {
                  state.releaseEventsCancelable.foreach(_.cancel())
                  state.releaseEventsCancelable = None
                }
              })
          }
      }

    protected def asyncOnBatchFailed(queueables: Vector[Queueable], problem: Problem) =
      Task.defer(
        onBatchFailed(queueables, problem) *>
          startAndForgetDirectorDriver)

    private def onBatchFailed(queueables: Seq[Queueable], problem: Problem): Task[Unit] =
      Task.defer {
        problem match {
          case DecoupledProblem | RecouplingStreamReader.TerminatedProblem =>
            logger.debug(s"Command batch failed: $problem")
          case _ =>
            logger.warn(s"Command batch failed: $problem")
        }
        commandQueue.handleBatchFailed(queueables)
      }
  }

  protected def start =
    startService(
      startNewClusterWatch
        .*>(startAndForgetDirectorDriver)
        .*>(untilStopRequested)
        .*>(Task {
          state.releaseEventsCancelable.foreach(_.cancel())
        })
        .*>(directorDriverAllocated.release)
        .*>(clusterWatchAllocated.release))

  def send(input: Queueable): Task[Unit] =
    /*logger.traceTask("send", input.toShortString)*/(Task.defer {
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
              .raceFold(untilStopRequested)
              .startAndForget))
    })

  def changeAgentRef(agentRef: AgentRef): Task[Unit] =
    logger.traceTask(
      state.lock.lock(Task {
        state.directors = agentRef.directors
      }) *>
        // FIXME Handle Cluster node URI change or forbid this
        // TODO Restart DirectorDriver only if one of the URIs has changed
        startNewClusterWatch *>
        startAndForgetDirectorDriver)

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

  private def dedicateAgentIfNeeded(directorDriver: DirectorDriver): Task[Checked[(AgentRunId, EventId)]] =
    logger.traceTask(Task.defer {
      journal.state
        .map(_.keyTo(AgentRefState).checked(agentPath))
        .flatMapT { agentRefState =>
          import agentRefState.agentRef.directors
          agentRefState.agentRunId match {
            case Some(agentRunId) => Task.right(agentRunId -> state.lastFetchedEventId)
            case None =>
              directorDriver
                .executeCommand(
                  DedicateAgentDirector(directors, controllerId, agentPath))
                .flatMapT { case DedicateAgentDirector.Response(agentRunId, agentEventId) =>
                  (if (noJournal)
                    Task.pure(Checked.unit)
                  else
                    journal
                      .persistKeyedEvent(
                        agentPath <-: AgentDedicated(agentRunId, Some(agentEventId)))
                      .flatMapT { _ =>
                        agentRunIdOnce := agentRunId
                        reattachSubagents().map(Right(_))
                      })
                    .rightAs(agentRunId -> agentEventId)
                }
          }
        }
    })

  private def reattachSubagents(): Task[Unit] =
    journal.state.flatMap(controllerState =>
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

  private def startNewClusterWatch: Task[Unit] =
    logger.debugTask(
      clusterWatchAllocated
        .acquire(clusterWatchResource)
        .*>(journal.state
          .map(_.agentToUris(agentPath).toList)
          .flatMap(uris =>
            Task.when(uris.lengthIs == 2)(
              // TODO Nur senden, wenn ClusterState.Empty !
              appointClusterNodes(uris(0), uris(1))))))

  private def appointClusterNodes(primaryUri: Uri, backupUri: Uri): Task[Unit] =
    commandQueue
      .enqueue(
        Queueable.ClusterAppointNodes(
          Map(
            AgentClusterConf.primaryNodeId -> primaryUri,
            AgentClusterConf.backupNodeId -> backupUri),
          NodeId("Primary")))
      .void

  private def startAndForgetDirectorDriver(implicit src: sourcecode.Enclosing): Task[Unit] =
    startNewDirectorDriver
      .onErrorHandle(t => Task(logger.error(
        s"${src.value} startDirectorDriver => ${t.toStringWithCauses}", t)))
      .raceFold(untilStopRequested)
      .startAndForget

  private def startNewDirectorDriver: Task[Unit] =
    logger.debugTask(
      directorDriverAllocated
        .acquire(directorDriverResource)
        .void)

  private def directorDriverResource: Resource[Task, DirectorDriver] =
    for {
      client <- activeClientResource
      directorDriver <- DirectorDriver.resource(
        agentPath, state.lastFetchedEventId,
        client,
        dedicateAgentIfNeeded, onCouplingFailed, onCoupled, onDecoupled,
        onEventsFetched,
        journal, conf)
    } yield directorDriver

  private def clusterWatchResource: Resource[Task, ClusterWatchService] =
    for {
      clients <- clientsResource
      clusterWatchService <- ClusterWatchService.resource(
        clusterWatchId,
        Resource.eval(Task.pure(clients)),
        controllerConfiguration.config,
        label = agentPath.toString,
        onClusterStateChanged = onClusterStateChanged)
    } yield clusterWatchService

  private def onClusterStateChanged(hasNodes: HasNodes): Unit =
    if (!this.clusterState.contains(hasNodes)) {
      logger.info(hasNodes.toShortString)
      val activeNodeChanged = this.clusterState.forall(_.activeId != hasNodes.activeId)
      this.clusterState = Some(hasNodes)
      if (activeNodeChanged) {
        startAndForgetDirectorDriver
          .runAsyncAndForget // ???
      }
    }

  private def activeClientResource: Resource[Task, AgentClient] = {
    ActiveClusterNodeSelector.selectActiveNodeApi[AgentClient](
      clientsResource,
      failureDelay = conf.recouplingStreamReader.failureDelay,
      onCouplingError = _ => throwable =>
        onCouplingFailed(throwable match {
          case ProblemException(problem) => problem
          case t => Problem.fromThrowable(t)
        }).void)
  }

  private def clientsResource: Resource[Task, Nel[AgentClient]] =
    Resource
      .eval(journal.state.map(_.agentToUris(agentPath)))
      .flatMap(_.traverse(clientResource))

  private def clientResource(uri: Uri): Resource[Task, AgentClient] =
    SessionApi.resource(Task {
      val agentUserAndPassword = controllerConfiguration.config
        .optionAs[SecretString]("js7.auth.agents." + ConfigUtil.joinPath(agentPath.string))
        .map(password => UserAndPassword(controllerId.toUserId, password))
      AgentClient(uri, agentUserAndPassword, label = agentPath.toString,
        controllerConfiguration.httpsConfig)(actorSystem)
    })

  override def toString = s"AgentDriver($agentPath)"
}

private[controller] object AgentDriver
{
  def resource(
    agentRef: AgentRef, agentRunId: Option[AgentRunId], eventId: EventId,
    onEvents: (AgentRunId, Seq[Stamped[AnyKeyedEvent]]) => Task[Option[EventId]],
    onOrderMarked: Map[OrderId, OrderMark] => Task[Unit],
    journal: Journal[ControllerState],
    agentDriverConf: AgentDriverConfiguration, controllerConf: ControllerConfiguration,
    actorSystem: ActorSystem)
    (implicit s: Scheduler)
  : Resource[Task, AgentDriver] =
    Service.resource(Task(
      new AgentDriver(
        agentRef, agentRunId, eventId,
        onEvents, onOrderMarked,
        journal, agentDriverConf, controllerConf, actorSystem)))

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

    @deprecated // ???
    final case class ClusterAppointNodes(idToUri: Map[NodeId, Uri], activeId: NodeId) extends Queueable

    case object ClusterSwitchOver extends Queueable
  }

  private[agent] val DecoupledProblem = Problem.pure("Agent is not coupled")
}
