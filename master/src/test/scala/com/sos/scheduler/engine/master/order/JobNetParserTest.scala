package com.sos.scheduler.engine.master.order

import com.sos.scheduler.engine.common.scalautil.xmls.XmlSources._
import com.sos.scheduler.engine.data.engine2.agent.AgentPath
import com.sos.scheduler.engine.data.engine2.order.{JobChainPath, JobNet, JobPath, NodeId}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JobNetParserTest extends FreeSpec {

  "JobNetParser" in {
    val xml =
      <job_chain>
        <job_chain_node state="INPUT" agent="/AGENT" job="/INPUT" next_state="NEXT" error_state="ERROR"/>
        <job_chain_node state="100" agent="AGENT" job="JOB"/>
        <job_chain_node state="NEXT"/>
        <job_chain_node.end state="ERROR"/>
      </job_chain>
    val path = JobChainPath("/FOLDER/JOBNET")
    assert(JobNetParser.parseXml(path, xml) ==
      JobNet(
        path,
        inputNodeId = NodeId("INPUT"),
        Map(
          NodeId("INPUT") → JobNet.JobNode(NodeId("INPUT"), AgentPath("/AGENT"), JobPath("/INPUT"), NodeId("NEXT"), NodeId("ERROR")),
          NodeId("100") → JobNet.JobNode(NodeId("100"), AgentPath("/FOLDER/AGENT"), JobPath("/FOLDER/JOB"), NodeId("NEXT"), NodeId("NEXT")),
          NodeId("NEXT") → JobNet.EndNode(NodeId("NEXT")),
          NodeId("ERROR") → JobNet.EndNode(NodeId("ERROR")))))
  }
}
