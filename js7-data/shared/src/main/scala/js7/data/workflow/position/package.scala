package js7.data.workflow

/**
  * @author Joacim Zschimmer
  */
package object position:
  object syntax:
    implicit final class RichWorkflowId(private val underlying: WorkflowId) extends AnyVal:
      def /(position: Position): WorkflowPosition =
        WorkflowPosition(underlying, position)
