package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.data.workflow.WorkflowScript.{End, Goto, Job, OnError}
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.transitions.SuccessErrorTransition
import com.sos.jobscheduler.data.workflow.{JobPath, NodeId, WorkflowGraph, WorkflowScript}
import com.sos.jobscheduler.shared.workflow.script.WorkflowScriptToGraph.workflowScriptToGraph
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class LegacyJobchainXmlParserTest extends FreeSpec {

  private val xml =
    <job_chain>
      <job_chain_node state="A" agent="/AGENT" job="/JOB-A" next_state="B" error_state="FAILURE"/>
      <job_chain_node state="B" agent="AGENT"  job="JOB-B"  next_state="C" error_state="FAILURE"/>
      <job_chain_node state="C" agent="/AGENT" job="/JOB-C" next_state="D" error_state="D"/>
      <job_chain_node state="E" agent="/AGENT" job="/JOB-E" next_state="END" error_state="FAILURE"/>
      <job_chain_node state="D" agent="/AGENT" job="/JOB-D" next_state="E"/>
      <job_chain_node state="END"/>
      <job_chain_node.end state="FAILURE"/>
    </job_chain>
  private val A = WorkflowGraph.JobNode(NodeId("A"), AgentPath("/AGENT"), JobPath("/JOB-A"))
  private val B = WorkflowGraph.JobNode(NodeId("B"), AgentPath("/FOLDER/AGENT"), JobPath("/FOLDER/JOB-B"))
  private val C = WorkflowGraph.JobNode(NodeId("C"), AgentPath("/AGENT"), JobPath("/JOB-C"))
  private val D = WorkflowGraph.JobNode(NodeId("D"), AgentPath("/AGENT"), JobPath("/JOB-D"))
  private val E = WorkflowGraph.JobNode(NodeId("E"), AgentPath("/AGENT"), JobPath("/JOB-E"))
  private val END = WorkflowGraph.EndNode(NodeId("END"))
  private val FAILURE = WorkflowGraph.EndNode(NodeId("FAILURE"))  // This last node becomes the workflow's end node
  private val script = LegacyJobchainXmlParser.parseXml(xml, FolderPath("/FOLDER"))

  "WorkflowScript" in {
    assert(script == WorkflowScript(List(
      Job(A.id, A.job), OnError(FAILURE.id),
      Job(B.id, B.job), OnError(FAILURE.id),
      Job(C.id, C.job), Goto(D.id),
      Job(E.id, E.job), OnError(FAILURE.id), Goto(END.id),
      Job(D.id, D.job), Goto(E.id),
      End(END.id),
      End(FAILURE.id))))
  }

  "WorkflowGraph" in {
    assert(workflowScriptToGraph(script) ==
      WorkflowGraph(start = A.id,
        List(A, B, C, E, D, END, FAILURE),
        List(
          Transition(Vector(A.id), Vector(B.id, FAILURE.id), SuccessErrorTransition),
          Transition(Vector(B.id), Vector(C.id, FAILURE.id), SuccessErrorTransition),
          Transition(C.id, D.id),
          Transition(Vector(E.id), Vector(END.id, FAILURE.id), SuccessErrorTransition),
          Transition(D.id, E.id)),
        Some(script)))
  }
}
