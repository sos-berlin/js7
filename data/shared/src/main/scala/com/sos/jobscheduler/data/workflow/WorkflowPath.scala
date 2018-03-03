package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.data.filebased.{SourceType, TypedPath}
import com.sos.jobscheduler.data.folder.FolderPath

final case class WorkflowPath(string: String)
extends TypedPath {

  validate()

  def companion = WorkflowPath

  def requireNonAnonymous(): Unit =
    if (this == WorkflowPath.Anonymous) throw new IllegalArgumentException("Order in WorkflowPath.Anonymous?")
}


object WorkflowPath extends TypedPath.Companion[WorkflowPath]
{
  val sourceTypeToFilenameExtension = Map(
    SourceType.Json → ".workflow.json",
    SourceType.Txt → ".workflow.txt",
    SourceType.Xml → ".job_chain.xml")

  val Anonymous: WorkflowPath = FolderPath.anonymous[WorkflowPath]
}
