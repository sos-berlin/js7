package com.sos.jobscheduler.master.gui.components.workfloworders

import com.sos.jobscheduler.data.workflow.Instruction
import com.sos.jobscheduler.data.workflow.Instruction.@:
import com.sos.jobscheduler.data.workflow.instructions.{ForkJoin, If, ImplicitEnd, Job}
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.html_<^._

/**
  * @author Joacim Zschimmer
  */
private[workfloworders] object InstructionComponent
{
  def apply(labeled: Instruction.Labeled) = component(labeled)

  private val component = ScalaComponent.builder[Instruction.Labeled]("Instruction")
    .render_P { case labels @: instruction ⇒
      <.div(^.cls := "orders-Instruction-head",
        labels.map(_ + ": ").mkString,
        instruction match {
          case _: ForkJoin ⇒
            "fork"

          case instr: If ⇒
            s"if (${instr.predicate})"

          case job: Job ⇒
            VdomArray(
              "job ", <.span(^.cls := "orders-Instruction-Job", job.jobPath.string),
              <.div("on ", <.span(^.cls := "orders-Instruction-Agent", job.agentPath.string)))

          case ImplicitEnd ⇒
            "end"

          case stmt: Instruction ⇒
            stmt.toString
        })
    }
    .configure {
      implicit val orderEntryReuse = Reusability.byRef[Instruction.Labeled]
      Reusability.shouldComponentUpdate
    }
    .build
}
