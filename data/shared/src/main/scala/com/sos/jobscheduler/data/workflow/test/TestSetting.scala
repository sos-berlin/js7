package com.sos.jobscheduler.data.workflow.test

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.{Order, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.WorkflowGraph.{EndNode, JobNode}
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.{JobPath, NodeId, WorkflowGraph, WorkflowPath}

/**
  * For tests only.
  */
private[jobscheduler] object TestSetting {

  val TestAgentPath = AgentPath("/AGENT")
  val AJobPath = JobPath("/A")
  val BJobPath = JobPath("/B")
  val TestJobPaths = Vector(AJobPath, BJobPath)

  val A = JobNode(NodeId("A"), TestAgentPath, AJobPath)
  val B = JobNode(NodeId("B"), TestAgentPath, BJobPath)
  val END = EndNode(NodeId("END"))

  val a = Transition(from = A.id, to = B.id)
  val b = Transition(from = B.id, to = END.id)

  val TestWorkflow = WorkflowGraph.Named(
    WorkflowPath("/WORKFLOW"),
    WorkflowGraph(start = A.id, nodes = List(A, B, END), transitions = List(a, b), sourceScript = None))

  val TestOrder = Order(OrderId("TEST"), TestWorkflow.start, Order.Ready, payload = Payload(Map("VARIABLE" → "VALUE")))
}
