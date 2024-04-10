package js7.data_for_java.controller

import io.vavr.control.Either as VEither
import java.time.Instant
import java.util.Objects.requireNonNull
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.log.{CorrelId, CorrelIdWrapped}
import js7.base.problem.Problem
import js7.base.time.JavaTimestamp
import js7.data.agent.AgentPath
import js7.data.board.{BoardPath, NoticeId}
import js7.data.controller.ControllerCommand
import js7.data.controller.ControllerCommand.{AddOrder, Batch, ClusterSwitchOver, ConfirmClusterNodeLoss, ControlWorkflow, ControlWorkflowPath, GoOrder, PostNotice, TransferOrders}
import js7.data.node.NodeId
import js7.data.order.OrderId
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.{Label, Position}
import js7.data_for_java.common.JJsonable
import js7.data_for_java.order.JFreshOrder
import js7.data_for_java.workflow.JWorkflowId
import js7.data_for_java.workflow.position.JPosition
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional

@javaApi
final case class JControllerCommand(asScala: ControllerCommand)
extends JJsonable[JControllerCommand]:

  type AsScala = ControllerCommand

  protected def companion = JControllerCommand


@javaApi
object JControllerCommand extends JJsonable.Companion[JControllerCommand]:
  type AsScala = ControllerCommand

  @Nonnull
  def addOrder(@Nonnull jFreshOrder: JFreshOrder): JControllerCommand =
    JControllerCommand(AddOrder(jFreshOrder.asScala))

  def batch(@Nonnull commands: java.util.List[JControllerCommand]): JControllerCommand =
    JControllerCommand(Batch:
      commands.asScala
        .map: cmd =>
          CorrelIdWrapped(CorrelId.current, cmd.asScala)
        .toVector)

  @Nonnull
  def goOrder(
    @Nonnull orderId: OrderId,
    @Nonnull position: JPosition)
  : JControllerCommand =
    JControllerCommand(GoOrder(orderId, position.asScala))

  @Nonnull
  def postNotice(
    @Nonnull boardPath: BoardPath,
    @Nonnull noticeId: NoticeId,
    @Nonnull endOfLife: Optional[Instant])
  : JControllerCommand =
    JControllerCommand(
      PostNotice(
        requireNonNull(boardPath),
        requireNonNull(noticeId),
        endOfLife.toScala
          .map(JavaTimestamp.ofInstant)))

  @Nonnull
  def controlWorkflowPath(
    @Nonnull workflowPath: WorkflowPath,
    @Nonnull suspend: Optional[Boolean],
    @Nonnull skip: java.util.Map[Label, java.lang.Boolean])
  : JControllerCommand =
    JControllerCommand(
      ControlWorkflowPath(
        requireNonNull(workflowPath),
        suspend = suspend.toScala,
        skip.asScala.view.mapValues(_.booleanValue).toMap))

  @Nonnull
  def controlWorkflow(
    @Nonnull workflowId: JWorkflowId,
    @Nonnull breakpoints: java.util.Map[JPosition, java.lang.Boolean])
  : JControllerCommand =
    JControllerCommand(
      ControlWorkflow(
        workflowId.asScala,
        removeBreakpoints = breakpoints.asScala.view
          .collect { case (k, java.lang.Boolean.FALSE) => k }
          .map(_.asScala)
          .toSet,
        addBreakpoints = breakpoints.asScala.view
          .collect { case (k, java.lang.Boolean.TRUE) => k }
          .map(_.asScala)
          .toSet))

  @Nonnull
  def confirmClusterNodeLoss(
    @Nonnull agentPath: AgentPath,
    @Nonnull lostNodeId: NodeId,
    @Nonnull confirmer: String)
  : JControllerCommand =
    JControllerCommand(
      ConfirmClusterNodeLoss(agentPath, lostNodeId, confirmer))

  @Nonnull
  def clusterSwitchover(@Nonnull agentPath: Optional[AgentPath]): JControllerCommand =
    JControllerCommand(
      ClusterSwitchOver(agentPath.toScala))

  @Nonnull
  def transferOrders(@Nonnull workflowId: JWorkflowId): JControllerCommand =
    JControllerCommand(
      TransferOrders(workflowId.asScala))

  @Nonnull
  override def fromJson(@Nonnull jsonString: String): VEither[Problem, JControllerCommand] =
    super.fromJson(jsonString)

  protected def jsonDecoder = ControllerCommand.jsonCodec
  protected def jsonEncoder = ControllerCommand.jsonCodec
