package com.sos.jobscheduler.proxy.javaapi.data

import com.sos.jobscheduler.base.annotation.javaApi
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.workflow.WorkflowPath

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
