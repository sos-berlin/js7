package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.data.workflow.Instruction.simplify._
import com.sos.jobscheduler.data.workflow.instructions.{ExplicitEnd, Goto, IfFailedGoto, Job, ReturnCodeMeaning}
import com.sos.jobscheduler.data.workflow.{JobPath, Label, Workflow}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class LegacyJobchainXmlParserTest extends FreeSpec {

  private val xml =
    <job_chain>
      <job_chain_node state="A" agent="/AGENT" job="/JOB-A"                error_state="ERROR"/>
      <job_chain_node state="B" agent="AGENT"  job="JOB-B"  next_state="C" error_state="ERROR"/>
      <job_chain_node state="C" agent="/AGENT" job="/JOB-C" next_state="D" error_state="D"/>
      <job_chain_node state="E" agent="/AGENT" job="/JOB-E" next_state="END" error_state="ERROR"/>
      <job_chain_node state="D" agent="/AGENT" job="/JOB-D" next_state="E"/>
      <job_chain_node state="END"/>
      <job_chain_node.end state="ERROR"/>
    </job_chain>

  private val workflow = LegacyJobchainXmlParser.parseXml(FolderPath("/FOLDER"), xml)

  "Workflow" in {
    assert(workflow == Workflow(Vector(
      "A" @: Job(JobPath("/JOB-A"), AgentPath("/AGENT"), ReturnCodeMeaning.NoFailure),
             IfFailedGoto(Label("ERROR")),
      "B" @: Job(JobPath("/FOLDER/JOB-B"), AgentPath("/FOLDER/AGENT"), ReturnCodeMeaning.NoFailure),
             IfFailedGoto(Label("ERROR")),
      "C" @: Job(JobPath("/JOB-C"), AgentPath("/AGENT"), ReturnCodeMeaning.NoFailure),
             Goto(Label("D")),
      "E" @: Job(JobPath("/JOB-E"), AgentPath("/AGENT"), ReturnCodeMeaning.NoFailure),
             IfFailedGoto(Label("ERROR")),
             Goto(Label("END")),
      "D" @: Job(JobPath("/JOB-D"), AgentPath("/AGENT"), ReturnCodeMeaning.NoFailure),
             Goto(Label("E")),
      "END" @: ExplicitEnd,
      "ERROR" @: ExplicitEnd)))
  }
}
