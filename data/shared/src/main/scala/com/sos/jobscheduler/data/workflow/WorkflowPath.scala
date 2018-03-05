package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.data.filebased.{SourceType, TypedPath}

final case class WorkflowPath(string: String)
extends TypedPath
{
  def companion = WorkflowPath
}

object WorkflowPath extends TypedPath.Companion[WorkflowPath]
{
  val sourceTypeToFilenameExtension = Map(
    SourceType.Json → ".workflow.json",
    SourceType.Txt → ".workflow.txt",
    SourceType.Xml → ".job_chain.xml")
}
