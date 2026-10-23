package js7.data_for_java.order

import java.time.Instant
import java.util.Objects.requireNonNull
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.time.JavaTimeConverters.AsScalaInstant
import js7.data.order.{Order, OrderId, OrderObstacleCalculator}
import js7.data.workflow.WorkflowPath
import js7.data_for_java.controller.JControllerState
import js7.data_for_java.workflow.JWorkflowId

@javaApi
object JOrderPredicates:
  type OrderPredicate = java.util.function.Predicate[Order[Order.State]]

  val any: OrderPredicate =
    _ => true

  val none: OrderPredicate =
    _ => false

  @Nonnull
  def by(@Nonnull workflowId: JWorkflowId): OrderPredicate =
    byWorkflowId(workflowId)

  @Nonnull
  def by(@Nonnull workflowPath: WorkflowPath): OrderPredicate =
    byWorkflowPath(workflowPath)

  @Nonnull
  def byWorkflowId(@Nonnull workflowId: JWorkflowId): OrderPredicate =
    val id = workflowId.asScala
    _.workflowId == id

  @Nonnull
  def byWorkflowPath(@Nonnull workflowPath: WorkflowPath): OrderPredicate =
    requireNonNull(workflowPath)
    _.workflowPath == workflowPath

  @Nonnull
  def byOrderState(@Nonnull stateClass: Class[? <: Order.State]): OrderPredicate =
    requireNonNull(stateClass)
    order => stateClass.isAssignableFrom(order.state.getClass)

  @Nonnull
  def byOrderObstacleClass(
    state: JControllerState,
    @Nonnull obstacleClass: Class[? <: JOrderObstacle],
    now: Instant)
  : OrderPredicate =
    val cls = JOrderObstacle.toScalaClass(obstacleClass)
    order => new OrderObstacleCalculator(state.asScala)
      .orderToObstacles(order.id, now.toTimestamp)
      .exists(_.exists(cls.isInstance(_)))

  @Nonnull
  def markedAsDeleteWhenTerminated(@Nonnull value: Boolean): OrderPredicate =
    requireNonNull(value)
    _.deleteWhenTerminated == value

  @Nonnull
  def byOrderIdPredicate(@Nonnull predicate: java.util.function.Predicate[OrderId]): OrderPredicate =
    requireNonNull(predicate)
    order => predicate.test(order.id)

  @Nonnull
  def and(@Nonnull a: OrderPredicate, @Nonnull b: OrderPredicate): OrderPredicate =
    requireNonNull(a)
    requireNonNull(b)
    order => a.test(order) && b.test(order)

  @Nonnull
  def or(@Nonnull a: OrderPredicate, @Nonnull b: OrderPredicate): OrderPredicate =
    requireNonNull(a)
    requireNonNull(b)
    order => a.test(order) || b.test(order)

  @Nonnull
  def not(@Nonnull predicate: OrderPredicate): OrderPredicate =
    requireNonNull(predicate)
    order => !predicate.test(order)
