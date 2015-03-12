package com.sos.scheduler.engine.data.xmlcommands

import com.sos.scheduler.engine.data.job.JobPath

final case class StartJobCommand(jobPath: JobPath, variables: Iterable[(String, String)] = Nil) extends XmlCommand {
  def xmlElem =
    <start_job job={jobPath.string}><params>{
      variables map { case (k, v) ⇒ <param name={k} value={v}/> }
    }</params></start_job>
}
