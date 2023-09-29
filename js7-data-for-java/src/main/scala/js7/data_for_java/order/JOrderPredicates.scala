package js7.data_for_java.order

import java.time.Instant
import java.util.Objects.requireNonNull
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.time.JavaTimeConverters.AsScalaInstant
import js7.base.time.WallClock
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.order.{Order, OrderId, OrderObstacleCalculator}
import js7.data.workflow.WorkflowPath
import js7.data_for_java.controller.JControllerState
import js7.data_for_java.workflow.JWorkflowId

@javaApi
object JOrderPredicates:
  private type Predicate = Order[Order.State] => Boolean

  val any: Predicate = _ => true

  val none: Predicate = _ => false

  @Nonnull
  def by(@Nonnull workflowId: JWorkflowId): Predicate =
    byWorkflowId(workflowId)

  @Nonnull
  def by(@Nonnull workflowPath: WorkflowPath): Predicate =
    byWorkflowPath(workflowPath)

  @Nonnull
  def byWorkflowId(@Nonnull workflowId: JWorkflowId): Predicate =
    val id = workflowId.asScala
    _.workflowId == id

  @Nonnull
  def byWorkflowPath(@Nonnull workflowPath: WorkflowPath): Predicate =
    requireNonNull(workflowPath)
    _.workflowPath == workflowPath

  @Nonnull
  def byOrderState(@Nonnull stateClass: Class[? <: Order.State]): Predicate =
    requireNonNull(stateClass)
    order => stateClass isAssignableFrom order.state.getClass

  @Nonnull
  def byOrderObstacleClass(
    state: JControllerState,
    @Nonnull obstacleClass: Class[? <: JOrderObstacle],
    now: Instant)
  : Predicate =
    val cls = JOrderObstacle.toScalaClass(obstacleClass)
    val service = new InstructionExecutorService(WallClock.fixed(now.toTimestamp))

    order => new OrderObstacleCalculator(state.asScala)
      .orderToObstacles(order.id)(service)
      .exists(_.exists(cls.isInstance(_)))

  @Nonnull
  def markedAsDeleteWhenTerminated(@Nonnull value: Boolean): Predicate =
    requireNonNull(value)
    _.deleteWhenTerminated == value

  @Nonnull
  def byOrderIdPredicate(@Nonnull predicate: java.util.function.Predicate[OrderId]): Predicate =
    requireNonNull(predicate)
    order => predicate.test(order.id)

  @Nonnull
  def and(@Nonnull a: Predicate, @Nonnull b: Predicate): Predicate =
    requireNonNull(a)
    requireNonNull(b)
    order => a(order) && b(order)

  @Nonnull
  def or(@Nonnull a: Predicate, @Nonnull b: Predicate): Predicate =
    requireNonNull(a)
    requireNonNull(b)
    order => a(order) || b(order)

  @Nonnull
  def not(@Nonnull predicate: Predicate): Predicate =
    requireNonNull(predicate)
    order => !predicate(order)
