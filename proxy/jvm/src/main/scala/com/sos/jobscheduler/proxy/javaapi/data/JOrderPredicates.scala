package js7.proxy.javaapi.data

import js7.base.annotation.javaApi
import js7.data.order.Order
import js7.data.workflow.WorkflowPath

@javaApi
object JOrderPredicates
{
  type Predicate = Order[Order.State] => Boolean

  def by(workflowId: JWorkflowId): Predicate =
    byWorkflowId(workflowId)

  def by(workflowPath: WorkflowPath): Predicate =
    byWorkflowPath(workflowPath)

  def byWorkflowId(workflowId: JWorkflowId): Predicate = {
    val id = workflowId.underlying
    _.workflowId == id
  }

  def byWorkflowPath(workflowPath: WorkflowPath): Predicate =
    _.workflowId.path == workflowPath

  def byOrderState(stateClass: Class[_ <: Order.State]): Predicate =
    order => stateClass isAssignableFrom order.state.getClass

  def and(a: Predicate, b: Predicate): Predicate =
    order => a(order) && b(order)

  def or(a: Predicate, b: Predicate): Predicate =
    order => a(order) || b(order)

  def not(predicate: Predicate): Predicate =
    order => !predicate(order)
}
