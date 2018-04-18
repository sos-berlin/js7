package com.sos.jobscheduler.master.workflow

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.workflow.instructions.{ExplicitEnd, Goto, IfNonZeroReturnCodeGoto, Job, ReturnCodeMeaning}
import com.sos.jobscheduler.data.workflow.{Label, Workflow}
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


  "Workflow" in {
    val workflow = LegacyJobchainXmlParser.parseXml(FolderPath("/FOLDER"), xml).orThrow
    assert(workflow == Workflow.of(
      "A" @: Job(JobPath("/JOB-A"), AgentPath("/AGENT"), ReturnCodeMeaning.NoFailure),
             IfNonZeroReturnCodeGoto(Label("ERROR")),
      "B" @: Job(JobPath("/FOLDER/JOB-B"), AgentPath("/FOLDER/AGENT"), ReturnCodeMeaning.NoFailure),
             IfNonZeroReturnCodeGoto(Label("ERROR")),
      "C" @: Job(JobPath("/JOB-C"), AgentPath("/AGENT"), ReturnCodeMeaning.NoFailure),
             Goto(Label("D")),
      "E" @: Job(JobPath("/JOB-E"), AgentPath("/AGENT"), ReturnCodeMeaning.NoFailure),
             IfNonZeroReturnCodeGoto(Label("ERROR")),
             Goto(Label("END")),
      "D" @: Job(JobPath("/JOB-D"), AgentPath("/AGENT"), ReturnCodeMeaning.NoFailure),
             Goto(Label("E")),
      "END" @: ExplicitEnd,
      "ERROR" @: ExplicitEnd))
  }
}
