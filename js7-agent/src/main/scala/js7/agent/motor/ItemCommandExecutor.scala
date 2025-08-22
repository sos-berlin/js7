package js7.agent.motor

import cats.effect.IO
import cats.syntax.flatMap.*
import cats.syntax.traverse.*
import io.circe.syntax.EncoderOps
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{AttachItem, AttachSignedItem, DetachItem, Response}
import js7.agent.motor.ItemCommandExecutor.*
import js7.base.catsutils.CatsEffectExtensions.{joinStd, left, right, startAndForget, startAndLogError}
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.crypt.Signed
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger.syntax.*
import js7.base.log.{BlockingSymbol, Logger}
import js7.base.monixutils.AsyncVariable
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTime.extensions.toZoneId
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.pureFiberIO
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.cluster.WorkingClusterNode
import js7.data.Problems.PassiveClusterNodeUrlChangeableOnlyWhenNotCoupledProblem
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.calendar.Calendar
import js7.data.cluster.ClusterState
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, EventCalc, KeyedEvent, TimeCtx}
import js7.data.item.BasicItemEvent.{ItemAttachedToMe, ItemDetached, ItemDetachingFromMe, SignedItemAttachedToMe}
import js7.data.item.{InventoryItemEvent, InventoryItemKey, SignableItem, UnsignedItem}
import js7.data.job.JobResource
import js7.data.node.NodeId
import js7.data.orderwatch.{FileWatch, OrderWatchPath}
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentId, SubagentItem}
import js7.data.workflow.{Workflow, WorkflowControl, WorkflowId, WorkflowPathControl}
import js7.journal.{FileJournal, Persisted}
import js7.launcher.configuration.Problems.SignedInjectionNotAllowed
import js7.subagent.Subagent
import js7.subagent.director.SubagentKeeper
import scala.concurrent.duration.Deadline.now

private final class ItemCommandExecutor(
  forDirector: Subagent.ForDirector,
  agentPath: AgentPath,
  subagentKeeper: SubagentKeeper[AgentState],
  fileWatchManager: FileWatchManager,
  workingClusterNode: WorkingClusterNode[AgentState],
  conf: AgentConfiguration,
  orderMotor: OrderMotor,
  journal: FileJournal[AgentState]):

  @volatile private var changeSubagentAndClusterNodeAndProceedFiberStop = false
  private val changeSubagentAndClusterNodeAndProceedFiber =
    AsyncVariable(pureFiberIO(Checked.unit))

  def executeItemCommand(cmd: AgentCommand.IsItemCommand): IO[Checked[AgentCommand.Response]] =
    cmd match
      case AttachItem(item) =>
        attachUnsignedItem(item)

      case AttachSignedItem(signed) =>
        attachSignedItem(signed)

      case DetachItem(itemKey) =>
        detachItem(itemKey)

  private def attachUnsignedItem(item: UnsignedItem): IO[Checked[Response.Accepted]] =
    item match
      case agentRef: AgentRef =>
        if agentRef.path != agentPath then
          IO.left(Problem(s"Alien AgentRef(${agentRef.path})"))
        else
          changeSubagentAndClusterNodeThenProceed(ItemAttachedToMe(agentRef))
            .rightAs(AgentCommand.Response.Accepted)

      case item: SubagentItem =>
        changeSubagentAndClusterNodeThenProceed(ItemAttachedToMe(item))
          .rightAs(AgentCommand.Response.Accepted)

      case fileWatch: FileWatch =>
        if !conf.subagentConf.scriptInjectionAllowed then
          IO.left(SignedInjectionNotAllowed)
        else
          fileWatchManager.update(fileWatch)
            .map(_.rightAs(AgentCommand.Response.Accepted))

      case item @ (_: AgentRef | _: Calendar | _: SubagentBundle |
                   _: WorkflowPathControl | _: WorkflowControl) =>
        persist(ItemAttachedToMe(item))
          .rightAs(AgentCommand.Response.Accepted)

      case _ =>
        IO.left(Problem.pure(s"AgentCommand.AttachItem(${item.key}) for unknown InventoryItem"))

  private def attachSignedItem(signed: Signed[SignableItem]): IO[Checked[Response.Accepted]] =
    forDirector.signatureVerifier.verify(signed.signedString) match
      case Left(problem) =>
        logger.warn(s"${signed.value.key} could not be verified: $problem")
        IO.left(problem)
      case Right(signerIds) =>
        logger.info(Logger.SignatureVerified,
          s"Verified ${signed.value.key}, signed by ${signerIds.mkString(", ")}")
        persist: agentState =>
          signed.value match
            case workflow: Workflow =>
              agentState.idToWorkflow.get(workflow.id) match
                case None =>
                  // IMPORTANT: Check ZoneId to allow problem-free access after being persisted
                  workflow.timeZone.toZoneId.map: _ =>
                    SignedItemAttachedToMe(signed) :: Nil

                case Some(registeredWorkflow) =>
                  if workflow.withoutSource.reduceForAgent(agentPath) !=
                    registeredWorkflow.withoutSource
                  then
                    logger.warn(s"AttachSignedItem: Different duplicate ${workflow.id}:")
                    logger.warn(s"AttachSignedItem  ${workflow.withoutSource.asJson.toPrettyString}")
                    logger.warn(s"But registered is ${registeredWorkflow.withoutSource.asJson.toPrettyString}")
                    Left(Problem.pure(s"Different duplicate ${workflow.id}"))
                  else
                    Right(Nil)

            case _: JobResource =>
              Right:
                SignedItemAttachedToMe(signed) :: Nil

            case _ =>
              Left(Problem.pure:
                s"AgentCommand.AttachSignedItem(${signed.value.key}) for unknown SignableItem")
        .rightAs(AgentCommand.Response.Accepted)

  private def detachItem(itemKey: InventoryItemKey): IO[Checked[AgentCommand.Response]] =
    def persistItemDetachedIfExists =
      persist: agentState =>
        ifItemKeyExists(agentState):
          ItemDetached(itemKey, agentPath)

    def ifItemKeyExists[E <: InventoryItemEvent](agentState: AgentState)(event: => E) =
      Right:
        if !agentState.keyToItem.contains(itemKey) then
          logger.warn(s"DetachItem($itemKey) but item is unknown")
          Nil
        else
          (NoKey <-: event) :: Nil

    itemKey match
      case path: OrderWatchPath =>
        fileWatchManager.remove(path)
          .rightAs(AgentCommand.Response.Accepted)

      case subagentId: SubagentId =>
        journal.persist: agentState =>
          ifItemKeyExists(agentState):
            ItemDetachingFromMe(subagentId)
        .ifPersisted: _ =>
          subagentKeeper.removeSubagent(subagentId)
            .handleError[Unit]: t =>
              logger.error(s"removeSubagent($subagentId) => $t")
            .startAndForget // May take a short time
        // SubagentKeeper will eventually emit ItemDetached event
        .rightAs(AgentCommand.Response.Accepted)

      case bundleId: SubagentBundleId =>
        persistItemDetachedIfExists.ifPersisted: _ =>
          subagentKeeper.removeSubagentBundle(bundleId)
        .rightAs(AgentCommand.Response.Accepted)

      case WorkflowId.as(workflowId) =>
        persistItemDetachedIfExists.ifPersisted: persisted =>
          persisted.originalAggregate.idToWorkflow.get(workflowId).foldMap: workflow =>
            subagentKeeper
              .stopJobs(workflow.keyToJob.keys, SIGKILL /*just in case*/)
              .handleError: t =>
                logger.error(s"SubagentKeeper.stopJobs: ${t.toStringWithCauses}", t)
        .rightAs(AgentCommand.Response.Accepted)

      case _ =>
        persistItemDetachedIfExists
          .rightAs(AgentCommand.Response.Accepted)
  end detachItem

  private def changeSubagentAndClusterNodeThenProceed(event: ItemAttachedToMe): IO[Checked[Unit]] =
    // TODO Behaviour could be improved
    changeSubagentAndClusterNodeAndProceedFiber
      .update: fiber =>
        changeSubagentAndClusterNodeAndProceedFiberStop = true
        fiber.joinStd *>
          IO.defer:
            changeSubagentAndClusterNodeAndProceedFiberStop = false
            tryForeverChangeSubagentAndClusterNodeAndProceed(event)
              .startAndLogError
      .flatMap(_.joinStd.timeoutTo(10.s/*???*/, IO.right(()) /*respond the command*/))

  private def tryForeverChangeSubagentAndClusterNodeAndProceed(event: ItemAttachedToMe)
  : IO[Checked[Unit]] =
    IO.defer:
      import event.item
      val label = s"${event.getClass.simpleScalaName}(${item.key})"
      val since = now
      val delays = Iterator(1.s, 3.s, 6.s, 10.s).continueWithLast
      val sym = new BlockingSymbol
      ().tailRecM: _ =>
        changeSubagentAndClusterNode(event)
          .flatMapT: _ =>
            orderMotor.proceedWithItem(item)
          .uncancelable // Only the loop should be cancelable, but not the inner operations
          .flatMap:
            case Left(problem @ PassiveClusterNodeUrlChangeableOnlyWhenNotCoupledProblem) =>
              sym.onWarn()
              logger.warn(s"$sym $label => $problem — trying since ${since.elapsed.pretty} ...")
              IO.sleep(delays.next()).as(Left(()) /*repeat*/)

            case checked =>
              IO:
                if sym.used then checked match
                  case Left(problem) => logger.error(s"🔥 $label: $problem")
                  case Right(()) => logger.info(s"🟢 $label: Cluster setting has been changed")
                Right(checked)

  /** Emits the event, and ClusterSettingUpdated if needed, in separate transaction. */
  private def changeSubagentAndClusterNode(event: ItemAttachedToMe): IO[Checked[Unit]] =
    logger.debugIO:
      journal.aggregate.flatMap: agentState =>
        if !agentState.isDedicated then
          journal.persist(event).rightAs(())
        else
          IO.pure(agentState.applyKeyedEvent(event)).flatMapT: nextAgentState =>
            demandedClusterNodeUris(nextAgentState) match
              case None =>
                journal.persist(event).rightAs(())

              case Some(idToUri) =>
                agentState.clusterState.match
                  case ClusterState.Empty =>
                    Some(NodeId.primary)
                  case clusterState: ClusterState.HasNodes =>
                    (clusterState.setting.idToUri != idToUri) ? clusterState.activeId
                .match
                  case None =>
                    journal.persist(event).rightAs(())
                  case Some(activeNodeId) =>
                    workingClusterNode.appointNodes(idToUri, activeNodeId, extraEvent = Some(event))

  /** Returns Some when AgentRef and the Director's SubagentIds are available. */
  private def demandedClusterNodeUris(agentState: AgentState): Option[Map[NodeId, Uri]] =
    for
      agentRef <- agentState.keyToItem(AgentRef).get(agentState.meta.agentPath)
      if agentRef.directors.length == 2
      subagentItems <- agentRef.directors.traverse(agentState.keyToItem(SubagentItem).get)
      if subagentItems.length == 2
    yield
      Map(
        NodeId.primary -> subagentItems(0).uri,
        NodeId.backup -> subagentItems(1).uri)

  private def persist[E <: Event](keyedEvent: KeyedEvent[E])
  : IO[Checked[Persisted[AgentState, E]]] =
    journal.persist(keyedEvent)
      .flatTapT(onPersisted)

  private def persist[E <: Event](toEvents: AgentState => Checked[IterableOnce[KeyedEvent[E]]])
  : IO[Checked[Persisted[AgentState, E]]] =
    journal.persist(EventCalc.checked[AgentState, E, TimeCtx](toEvents(_)))
      .flatTapT(onPersisted)

  private def persist[E <: Event](eventCalc: EventCalc[AgentState, E, TimeCtx])
  : IO[Checked[Persisted[AgentState, E]]] =
    journal.persist(eventCalc)
      .flatTapT(onPersisted)

  private def onPersisted[E <: Event](persisted: Persisted[AgentState, Event]): IO[Checked[Unit]] =
    if persisted.isEmpty then
      IO.right(())
    else
      onItemEventPersisted(persisted)

  private def onItemEventPersisted(persisted: Persisted[AgentState, Event]): IO[Checked[Unit]] =
    persisted.keyedEvents.foldMap:
      case KeyedEvent(NoKey, ItemDetached(WorkflowId.as(workflowId), _)) =>
        orderMotor.jobMotorKeeper.stopJobMotors(workflowId)
          .map(Right(_))
      case KeyedEvent(NoKey, ItemAttachedToMe(item)) =>
        orderMotor.proceedWithItem(item)
      case KeyedEvent(NoKey, SignedItemAttachedToMe(Signed(workflow: Workflow, _))) =>
        orderMotor.jobMotorKeeper.startJobMotors(persisted.aggregate.idToWorkflow(workflow.id))
          .map(Right(_))
      case _ =>
        IO.right(())


object ItemCommandExecutor:
  private val logger = Logger[ItemCommandExecutor]
