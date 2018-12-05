package com.sos.jobscheduler.master.gui.browser.components.workfloworders

import com.sos.jobscheduler.data.workflow.Instruction
import com.sos.jobscheduler.data.workflow.Instruction.@:
import com.sos.jobscheduler.data.workflow.instructions.{Execute, Fork, If, ImplicitEnd}
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
          case _: Fork ⇒
            "fork"

          case instr: If ⇒
            s"if (${instr.predicate})"

          case Execute.Anonymous(job) ⇒
            VdomArray(
              "execute ", <.span(^.cls := "orders-Instruction-Executable", job.executablePath.string), ", ",
              <.div("agent=", <.span(^.cls := "orders-Instruction-Agent", job.agentPath.string)))

          case named: Execute.Named ⇒
            VdomArray("job ", named.name.string)

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
