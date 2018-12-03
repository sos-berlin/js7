package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.data.filebased.{SourceType, TypedPath}

final case class WorkflowPath private(string: String)
extends TypedPath
{
  def companion = WorkflowPath
}

object WorkflowPath extends TypedPath.Companion[WorkflowPath]
{
  val sourceTypeToFilenameExtension = Map(
    SourceType.Json → ".workflow.json",
    SourceType.Yaml → ".workflow.yaml",
    SourceType.Txt → ".workflow.txt",
    SourceType.Xml → ".job_chain.xml")

  protected def unchecked(string: String) = new WorkflowPath(string)
}
