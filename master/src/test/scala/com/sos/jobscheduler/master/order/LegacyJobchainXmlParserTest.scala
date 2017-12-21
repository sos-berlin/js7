package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.transitions.SuccessErrorTransition
import com.sos.jobscheduler.data.workflow.{JobPath, NodeId, Workflow, WorkflowRoute, WorkflowScript}
import com.sos.jobscheduler.shared.workflow.script.WorkflowScriptToRoute.workflowScriptToRoute
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class LegacyJobchainXmlParserTest extends FreeSpec {

  private val xml =
    <job_chain>
      <job_chain_node state="A" agent="/AGENT" job="/JOB-A" next_state="B" error_state="FAILURE"/>
      <job_chain_node state="B" agent="/AGENT" job="JOB-B" next_state="C" error_state="C"/>
      <job_chain_node state="C" agent="AGENT" job="/JOB-C"/>
      <job_chain_node state="END"/>
      <job_chain_node.end state="FAILURE"/>
    </job_chain>
  private val A = Workflow.JobNode(NodeId("A"), AgentPath("/AGENT"), JobPath("/JOB-A"))
  private val B = Workflow.JobNode(NodeId("B"), AgentPath("/AGENT"), JobPath("/FOLDER/JOB-B"))
  private val C = Workflow.JobNode(NodeId("C"), AgentPath("/FOLDER/AGENT"), JobPath("/JOB-C"))
  private val END = Workflow.EndNode(NodeId("END"))
  private val FAILURE = Workflow.EndNode(NodeId("FAILURE"))  // This last node becomes the workflow's end node
  private val script = LegacyJobchainXmlParser.parseXml(xml, FolderPath("/FOLDER"))

  "WorkflowScript" in {
    import WorkflowScript.{End, Job, OnError}
    assert(script == WorkflowScript(List(
      Job(A.id, A.job),
      OnError(FAILURE.id),
      Job(B.id, B.job),
      Job(C.id, C.job),
      End(END.id),
      End(FAILURE.id))))
  }

  "WorkflowRoute" in {
    assert(workflowScriptToRoute(script) ==
      WorkflowRoute(start = A.id,
        List(A, B, C, END, FAILURE),
        List(
          Transition(Vector(A.id), Vector(B.id, FAILURE.id), SuccessErrorTransition),
          Transition(B.id, C.id),
          Transition(C.id, END.id))))
  }
}
