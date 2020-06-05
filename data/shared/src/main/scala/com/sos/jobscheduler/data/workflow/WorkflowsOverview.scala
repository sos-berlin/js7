package js7.data.workflow

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.data.filebased.FileBasedsOverview

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowsOverview(count: Int) extends FileBasedsOverview

object WorkflowsOverview extends FileBasedsOverview.Companion[Workflow] {
  type Overview = WorkflowsOverview

  implicit val jsonCodec = deriveCodec[WorkflowsOverview]

  def fileBasedsToOverview(fileBaseds: Seq[Workflow]) = WorkflowsOverview(fileBaseds.size)
}
