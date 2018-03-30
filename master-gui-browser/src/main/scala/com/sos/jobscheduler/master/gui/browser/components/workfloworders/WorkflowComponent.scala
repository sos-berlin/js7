package com.sos.jobscheduler.master.gui.browser.components.workfloworders

import com.sos.jobscheduler.data.workflow.Instruction
import com.sos.jobscheduler.master.gui.browser.components.state.PreparedWorkflow
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.html_<^
import japgolly.scalajs.react.vdom.html_<^._

/**
  * @author Joacim Zschimmer
  */
object WorkflowComponent
{
  def apply(preparedWorkflow: PreparedWorkflow) = component(preparedWorkflow)

  private val component = ScalaComponent.builder[PreparedWorkflow]("PreparedWorkflow")
    .render_P { preparedWorkflow ⇒
      val b = new VdomBuilder
      b.build(preparedWorkflow.workflow)
      b.vdomArray
    }
    .configure {
      implicit val reuse = Reusability.byRef[PreparedWorkflow]
      Reusability.shouldComponentUpdate
    }
    .build

  def moveElement(x: Int, y: Int): TagMod =
    ^.transform := s"translate(${x}px,${y}px)"

  private class VdomBuilder extends PreparedWorkflow.ScaffoldBuilder {
    val vdomArray = VdomArray()
    def addVdom(vdomNode: ⇒ html_<^.VdomNode) = vdomArray += vdomNode
    def instructionToVdom(labeled: Instruction.Labeled) = InstructionComponent(labeled)
  }
}
