package com.sos.jobscheduler.data.workflow.test

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.{Order, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.Workflow.{EndNode, JobNode}
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.{JobPath, NodeId, Workflow, WorkflowPath}

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

  val TestWorkflow = Workflow(WorkflowPath("/WORKFLOW"), start = A.id, end = END.id, List(A, B, END), List(a, b))

  val TestOrder = Order(OrderId("TEST"), TestWorkflow.startNodeKey, Order.Ready, payload = Payload(Map("VARIABLE" → "VALUE")))
}
