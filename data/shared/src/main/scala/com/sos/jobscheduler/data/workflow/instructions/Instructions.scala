package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.workflow.Instruction

/**
  * @author Joacim Zschimmer
  */
object Instructions
{
  implicit val jsonCodec: TypedJsonCodec[Instruction] = TypedJsonCodec[Instruction](
    Subtype[AwaitOrder],
    Subtype[Execute],
    Subtype.named[ExplicitEnd]("End"),
    Subtype[ImplicitEnd],  // Serialized for easier external use of Workflow
    Subtype[Finish],
    Subtype[Fail],
    Subtype[Fork],
    Subtype[Offer],
    Subtype[If],
    Subtype[IfNonZeroReturnCodeGoto],
    Subtype[Retry],
    Subtype.named[TryInstruction]("Try"),
    Subtype[Goto],
    Subtype[Gap])
}
