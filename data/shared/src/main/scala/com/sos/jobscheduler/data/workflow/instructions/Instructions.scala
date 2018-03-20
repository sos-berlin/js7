package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils.singletonCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.workflow.Instruction

/**
  * @author Joacim Zschimmer
  */
object Instructions
{
  private[workflow] implicit val jsonCodec: TypedJsonCodec[Instruction] = TypedJsonCodec[Instruction](
    Subtype[AwaitOrder],
    Subtype[Job],
    Subtype.named(singletonCodec(ExplicitEnd), "End"),
    Subtype(singletonCodec(ImplicitEnd)),  // Serialized for easier external use of Workflow
    Subtype(ForkJoin.jsonCodec),
    Subtype[Offer],
    Subtype[IfReturnCode],
    Subtype[IfNonZeroReturnCodeGoto],
    Subtype[Goto],
    Subtype(singletonCodec(Gap)))
}
