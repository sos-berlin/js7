package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.Order.AttachedTo
import com.sos.jobscheduler.data.order.{Order, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.Workflow.JobNode
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.{NodeId, NodeKey, Workflow}

/**
  * @author Joacim Zschimmer
  */
object Workflows {

  private val logger = Logger(getClass)

  implicit class ExecutableWorkflow(val workflow: Workflow) extends AnyVal {

    def isTransitionableOnAgent(nodeId: NodeId, state: Order.Transitionable, agentPath: AgentPath): Boolean =
      transitionForNode(nodeId, state) exists { transition ⇒
        transition.fromNodeIds map workflow.idToNode forall {
          case node: JobNode ⇒
            node.agentPath == agentPath
          case node ⇒
            logger.warn(s"Unexpected node type in transition $transition: $node")
            false
        }
      }

    private[workflow] def transitionForNode(nodeId: NodeId, state: Order.Transitionable): Option[Transition] =
      state match {
        case _: Order.Forked ⇒ workflow.forkNodeToJoiningTransition.get(nodeId)
        case Order.Processed ⇒ workflow.nodeToOutputTransition.get(nodeId)
      }

    def order[S <: Order.State](
      id: OrderId,
      nodeId: NodeId = workflow.inputNodeId,
      state: S,
      attachedTo: Option[AttachedTo] = None,
      payload: Payload = Payload.empty,
      parent: Option[OrderId] = None)
    =
      new Order[S](id, NodeKey(workflow.path, nodeId), state, attachedTo, payload, parent)
  }
}
