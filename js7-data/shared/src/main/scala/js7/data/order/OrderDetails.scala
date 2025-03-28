package js7.data.order

import js7.base.time.Timestamp
import js7.data.controller.ControllerId
import js7.data.plan.PlanId
import js7.data.value.NamedValues
import js7.data.value.expression.Scope
import js7.data.value.expression.scopes.OrderScopes
import js7.data.workflow.WorkflowPath

/** Details of an `Order` which are known at start and do not change in the Order's lifetime. */
trait OrderDetails:

  def arguments: NamedValues

  def scheduledFor: Option[Timestamp]

  // Not sure about workflowPath with future callable Workflows ???
  // This must be constant. In a future nested workflow, this must be the original workflow.
  def workflowPath: WorkflowPath

  //def forceJobAdmission: Boolean
  //def innerBlock: BranchPath
  //def startPosition: Option[PositionOrLabel]
  //def stopPositions: Set[PositionOrLabel]


trait MinimumOrder extends OrderDetails:

  def id: OrderId

  def planId: PlanId

  /** A Scope that does not change in the Order's lifetime. */
  def minimumScope(controllerId: ControllerId): Scope =
    OrderScopes.minimumOrderScope(id, this, controllerId)
