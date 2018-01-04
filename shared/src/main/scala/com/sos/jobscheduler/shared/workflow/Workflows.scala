package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowGraph.JobNode
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.{NodeId, WorkflowGraph}

/**
  * @author Joacim Zschimmer
  */
object Workflows {

  private val logger = Logger(getClass)

  implicit class ExecutableWorkflowGraph(val workflowGraph: WorkflowGraph) extends AnyVal {

    def isTransitionableOnAgent(nodeId: NodeId, state: Order.Transitionable, agentPath: AgentPath): Boolean =
      transitionForNode(nodeId, state) exists { transition ⇒
        transition.fromNodeIds map workflowGraph.idToNode forall {
          case node: JobNode ⇒
            node.agentPath == agentPath
          case node ⇒
            logger.warn(s"Unexpected node type in transition $transition: $node")
            false
        }
      }

    private[workflow] def transitionForNode(nodeId: NodeId, state: Order.Transitionable): Option[Transition] =
      state match {
        case _: Order.Forked ⇒ workflowGraph.forkNodeToJoiningTransition.get(nodeId)
        case Order.Processed ⇒ workflowGraph.nodeToOutputTransition.get(nodeId)
      }
  }
}
