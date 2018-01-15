package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.data.workflow.Instruction.{ExplicitEnd, Goto, IfError, Job}
import com.sos.jobscheduler.data.workflow.{AgentJobPath, JobPath, Label, Workflow}
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

  private val workflow = LegacyJobchainXmlParser.parseXml(xml, FolderPath("/FOLDER"))

  "Workflow" in {
    assert(workflow == Workflow(Vector(
      "A"       @: Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/JOB-A"))),
                   IfError(Label("FAILURE")),
      "B"       @: Job(AgentJobPath(AgentPath("/FOLDER/AGENT"), JobPath("/FOLDER/JOB-B"))),
                   IfError(Label("FAILURE")),
      "C"       @: Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/JOB-C"))),
                   Goto(Label("D")),
      "E"       @: Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/JOB-E"))),
                   IfError(Label("FAILURE")),
                   Goto(Label("END")),
      "D"       @: Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/JOB-D"))),
                   Goto(Label("E")),
      "END"     @: ExplicitEnd,
      "FAILURE" @: ExplicitEnd)))
  }
}
