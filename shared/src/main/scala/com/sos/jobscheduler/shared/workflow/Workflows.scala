package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.Instruction.{@:, End, ForkJoin, Gap, Job, Labeled}
import com.sos.jobscheduler.data.workflow.Workflow
import scala.collection.immutable.ListMap

/**
  * @author Joacim Zschimmer
  */
object Workflows {

  implicit class ExecutableWorkflowScript(val underlying: Workflow) extends AnyVal {
    import underlying._

    def reduceForAgent(agentPath: AgentPath): Workflow =
      Workflow(labeledInstructions map {
        case labels @: (fj @ ForkJoin(idToWorkflow)) if fj isPartiallyExecutableOnAgent agentPath ⇒
          labels @: ForkJoin(ListMap() ++ idToWorkflow.mapValues(_.reduceForAgent(agentPath)))

        case o @ (_ @: (job: Job)) if job isExecutableOnAgent agentPath ⇒
          o

        case o @ (_ @: (_: End))  ⇒
          o

        case Labeled(labels, _) ⇒
          Labeled(labels, Gap)
      })
  }
}
