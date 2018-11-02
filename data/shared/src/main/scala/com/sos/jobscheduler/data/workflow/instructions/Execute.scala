package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.workflow.Instruction
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob

/**
  * @author Joacim Zschimmer
  */
sealed trait Execute extends Instruction

object Execute
{
  def apply(name: WorkflowJob.Name) = Named(name)

  def apply(workflowJob: WorkflowJob) = Anonymous(workflowJob)

  final case class Named(name: WorkflowJob.Name) extends Execute {
    override def toString = s"execute $name"
  }

  final case class Anonymous(job: WorkflowJob) extends Execute {
    override def toString = s"execute '${job.executablePath}'"
  }

  implicit val jsonCodec = TypedJsonCodec[Execute](
    Subtype.named(deriveCodec[Named], "Execute.Named"),
    Subtype.named(deriveCodec[Anonymous], "Execute.Anonymous"))
}
