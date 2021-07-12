package js7.data.workflow.instructions

import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.workflow.Instruction

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
    Subtype[IfFailedGoto],
    Subtype[Prompt],
    Subtype[PostNotice],
    Subtype[ReadNotice],
    Subtype[Retry],
    Subtype.named[TryInstruction]("Try"),
    Subtype.named[LockInstruction]("Lock"),
    Subtype[Goto],
    Subtype[Gap])
}
