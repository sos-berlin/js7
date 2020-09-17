package js7.proxy.javaapi.data.order

import js7.base.annotation.javaApi
import js7.data.order.{Order, OrderId}
import js7.data.workflow.WorkflowPath
import js7.proxy.javaapi.data.workflow.JWorkflowId

@javaApi
object JOrderPredicates
{
  private type Predicate = Order[Order.State] => Boolean

  val any: Predicate = _ => true

  val none: Predicate = _ => false

  def by(workflowId: JWorkflowId): Predicate =
    byWorkflowId(workflowId)

  def by(workflowPath: WorkflowPath): Predicate =
    byWorkflowPath(workflowPath)

  def byWorkflowId(workflowId: JWorkflowId): Predicate = {
    val id = workflowId.asScala
    _.workflowId == id
  }

  def byWorkflowPath(workflowPath: WorkflowPath): Predicate =
    _.workflowId.path == workflowPath

  def byOrderState(stateClass: Class[_ <: Order.State]): Predicate =
    order => stateClass isAssignableFrom order.state.getClass

  def markedAsRemoveWhenTerminated(value: Boolean): Predicate =
    _.removeWhenTerminated == value

  def byOrderIdPredicate(predicate: java.util.function.Predicate[OrderId]): Predicate =
    order => predicate.test(order.id)

  def and(a: Predicate, b: Predicate): Predicate =
    order => a(order) && b(order)

  def or(a: Predicate, b: Predicate): Predicate =
    order => a(order) || b(order)

  def not(predicate: Predicate): Predicate =
    order => !predicate(order)
}
