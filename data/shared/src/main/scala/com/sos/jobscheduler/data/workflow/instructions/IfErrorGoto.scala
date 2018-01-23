package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.data.workflow.{JumpInstruction, Label}
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class IfErrorGoto(to: Label) extends JumpInstruction {
  def nodes = Nil

  override def toString = s"ifError $to"
}
