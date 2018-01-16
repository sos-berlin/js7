package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.Instruction.{@:, End, ForkJoin, Gap, Job, Labeled}
import com.sos.jobscheduler.data.workflow.Workflow

/**
  * @author Joacim Zschimmer
  */
object Workflows {

  implicit class ExecutableWorkflowScript(val underlying: Workflow) extends AnyVal {
    import underlying._

    def reduceForAgent(agentPath: AgentPath): Workflow =
      Workflow(labeledInstructions map {
        case labels @: (fj @ ForkJoin(branches)) if fj isPartiallyExecutableOnAgent agentPath ⇒
          labels @: ForkJoin(
            for (b ← branches) yield
              b.copy(workflow = b.workflow.reduceForAgent(agentPath)))

        case o @ (_ @: (job: Job)) if job isExecutableOnAgent agentPath ⇒
          o

        case o @ (_ @: (_: End))  ⇒
          o

        case Labeled(labels, _) ⇒
          Labeled(labels, Gap)
      })
  }
}
