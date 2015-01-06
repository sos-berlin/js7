package com.sos.scheduler.engine.data.xmlcommands

import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.xmlcommands.ModifyJobCommand._

/**
 * @author Joacim Zschimmer
 */
final case class ModifyJobCommand(
  jobPath: JobPath,
  cmd: Option[Cmd] = None)
extends XmlCommand {

  def xmlElem = <modify_job
    job={jobPath.string}
    cmd={(cmd map { _.name }).orNull}
    />
}

object ModifyJobCommand {
  sealed class Cmd(val name: String)

  object Cmd {
    case object Stop extends Cmd("stop")
  }
}
