package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.data.filebased.TypedPath

final case class WorkflowPath(string: String)
extends TypedPath {

  validate()

  def companion = WorkflowPath
}


object WorkflowPath extends TypedPath.Companion[WorkflowPath] {

  override lazy val xmlFilenameExtension = s".job_chain.xml"
}
