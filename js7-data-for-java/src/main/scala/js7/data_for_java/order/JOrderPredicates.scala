package js7.data_for_java.order

import java.util.Objects.requireNonNull
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.data.order.{Order, OrderId}
import js7.data.workflow.WorkflowPath
import js7.data_for_java.workflow.JWorkflowId

@javaApi
object JOrderPredicates
{
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
  def byWorkflowId(@Nonnull workflowId: JWorkflowId): Predicate = {
    val id = workflowId.asScala
    _.workflowId == id
  }

  @Nonnull
  def byWorkflowPath(@Nonnull workflowPath: WorkflowPath): Predicate = {
    requireNonNull(workflowPath)
    _.workflowId.path == workflowPath
  }

  @Nonnull
  def byOrderState(@Nonnull stateClass: Class[_ <: Order.State]): Predicate = {
    requireNonNull(stateClass)
    order => stateClass isAssignableFrom order.state.getClass
  }

  @Nonnull
  def markedAsDeleteWhenTerminated(@Nonnull value: Boolean): Predicate = {
    requireNonNull(value)
    _.deleteWhenTerminated == value
  }

  @Nonnull
  def byOrderIdPredicate(@Nonnull predicate: java.util.function.Predicate[OrderId]): Predicate = {
    requireNonNull(predicate)
    order => predicate.test(order.id)
  }

  @Nonnull
  def and(@Nonnull a: Predicate, @Nonnull b: Predicate): Predicate = {
    requireNonNull(a)
    requireNonNull(b)
    order => a(order) && b(order)
  }

  @Nonnull
  def or(@Nonnull a: Predicate, @Nonnull b: Predicate): Predicate = {
    requireNonNull(a)
    requireNonNull(b)
    order => a(order) || b(order)
  }

  @Nonnull
  def not(@Nonnull predicate: Predicate): Predicate = {
    requireNonNull(predicate)
    order => !predicate(order)
  }
}
