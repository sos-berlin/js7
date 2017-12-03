package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.transition.{SuccessFailureTransition, Transition}
import com.sos.jobscheduler.data.workflow.{JobPath, NodeId, Workflow, WorkflowPath}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowXmlParserTest extends FreeSpec {

  "WorkflowXmlParser" in {
    val xml =
      <job_chain>
        <job_chain_node state="A" agent="/AGENT" job="/JOB-A" next_state="B" error_state="FAILURE"/>
        <job_chain_node state="B" agent="/AGENT" job="/JOB-B" next_state="C" error_state="C"/>
        <job_chain_node state="C" agent="/AGENT" job="/JOB-C"/>
        <job_chain_node state="END"/>
        <job_chain_node.end state="FAILURE"/>
      </job_chain>
    val path = WorkflowPath("/FOLDER/JOBNET")
    val A = Workflow.JobNode(NodeId("A"), AgentPath("/AGENT"), JobPath("/JOB-A"))
    val B = Workflow.JobNode(NodeId("B"), AgentPath("/AGENT"), JobPath("/JOB-B"))
    val C = Workflow.JobNode(NodeId("C"), AgentPath("/AGENT"), JobPath("/JOB-C"))
    val END = Workflow.EndNode(NodeId("END"))
    val FAILURE = Workflow.EndNode(NodeId("FAILURE"))
    assert(WorkflowXmlParser.parseXml(path, xml) ==
      Workflow(path, inputNodeId = A.id, List(
        Transition(A :: Nil, B :: FAILURE :: Nil, SuccessFailureTransition),
        Transition(B, C),
        Transition(C, END))))
  }
}
