package com.sos.jobscheduler.master

import com.sos.jobscheduler.core.filebased.RepoReader
import com.sos.jobscheduler.master.agent.AgentReader
import com.sos.jobscheduler.master.workflow.WorkflowReader
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
final class MasterRepoReader(val fileBasedDirectory: Path) extends RepoReader
{
  protected final val readers = Set(WorkflowReader, AgentReader)
}
