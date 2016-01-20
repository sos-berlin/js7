package com.sos.scheduler.engine.data.xmlcommands

import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.xmlcommands.StartJobCommand._

final case class StartJobCommand(jobPath: JobPath, variables: Iterable[(String, String)] = Nil, at: Option[At] = None) extends XmlCommand {
  def xmlElem =
    <start_job job={jobPath.string} at={(at map { _.toXmlValue }).orNull}><params>{
      variables map { case (k, v) ⇒ <param name={k} value={v}/> }
    }</params></start_job>
}

object StartJobCommand {
  sealed trait At {
    def toXmlValue: String
  }
  object At {
    object Period extends At {
      def toXmlValue = "period"
    }
  }
}
