package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.{JobPath, NodeId, Workflow, WorkflowPath}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowXmlParserTest extends FreeSpec {

  "WorkflowXmlParser" in {
    val xml =
      <job_chain>
        <job_chain_node state="INPUT" agent="/AGENT" job="/INPUT" next_state="NEXT" error_state="ERROR"/>
        <job_chain_node state="100" agent="AGENT" job="JOB"/>
        <job_chain_node state="NEXT"/>
        <job_chain_node.end state="ERROR"/>
      </job_chain>
    val path = WorkflowPath("/FOLDER/JOBNET")
    assert(WorkflowXmlParser.parseXml(path, xml) ==
      Workflow(
        path,
        inputNodeId = NodeId("INPUT"),
        Map(
          NodeId("INPUT") → Workflow.JobNode(NodeId("INPUT"), AgentPath("/AGENT"), JobPath("/INPUT"), NodeId("NEXT"), NodeId("ERROR")),
          NodeId("100") → Workflow.JobNode(NodeId("100"), AgentPath("/FOLDER/AGENT"), JobPath("/FOLDER/JOB"), NodeId("NEXT"), NodeId("NEXT")),
          NodeId("NEXT") → Workflow.EndNode(NodeId("NEXT")),
          NodeId("ERROR") → Workflow.EndNode(NodeId("ERROR")))))
  }
}
