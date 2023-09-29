package js7.data.workflow

import js7.base.utils.ScalaUtils.syntax.*

/**
  * @author Joacim Zschimmer
  */
package object position
{
  object syntax {
    implicit final class RichWorkflowId(private val underlying: WorkflowId) extends AnyVal {
      def /(position: Position) = WorkflowPosition(underlying, position)
    }
  }
}
