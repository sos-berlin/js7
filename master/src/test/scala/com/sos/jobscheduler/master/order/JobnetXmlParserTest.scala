package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.jobnet.{JobPath, Jobnet, JobnetPath, NodeId}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JobnetXmlParserTest extends FreeSpec {

  "JobnetXmlParser" in {
    val xml =
      <job_chain>
        <job_chain_node state="INPUT" agent="/AGENT" job="/INPUT" next_state="NEXT" error_state="ERROR"/>
        <job_chain_node state="100" agent="AGENT" job="JOB"/>
        <job_chain_node state="NEXT"/>
        <job_chain_node.end state="ERROR"/>
      </job_chain>
    val path = JobnetPath("/FOLDER/JOBNET")
    assert(JobnetXmlParser.parseXml(path, xml) ==
      Jobnet(
        path,
        inputNodeId = NodeId("INPUT"),
        Map(
          NodeId("INPUT") → Jobnet.JobNode(NodeId("INPUT"), AgentPath("/AGENT"), JobPath("/INPUT"), NodeId("NEXT"), NodeId("ERROR")),
          NodeId("100") → Jobnet.JobNode(NodeId("100"), AgentPath("/FOLDER/AGENT"), JobPath("/FOLDER/JOB"), NodeId("NEXT"), NodeId("NEXT")),
          NodeId("NEXT") → Jobnet.EndNode(NodeId("NEXT")),
          NodeId("ERROR") → Jobnet.EndNode(NodeId("ERROR")))))
  }
}
