package js7.data.workflow.instructions

import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.workflow.Instruction

/**
  * @author Joacim Zschimmer
  */
object Instructions
{
  implicit val jsonCodec: TypedJsonCodec[Instruction] = TypedJsonCodec[Instruction](
    Subtype[Execute],
    Subtype.named[ExplicitEnd]("End"),
    Subtype[ImplicitEnd],  // Serialized for easier external use of Workflow
    Subtype[Finish],
    Subtype[Fail],
    Subtype[Fork],
    Subtype[ForkList],
    Subtype[If],
    Subtype[Prompt],
    PostNotice.compatibleSubtype,
    Subtype[PostNotices],
    ExpectNotice.compatibleSubtype,
    Subtype[ExpectNotices],
    Subtype[Retry],
    Subtype.named[TryInstruction]("Try"),
    Subtype.named[LockInstruction]("Lock"),
    Subtype[Cycle],
    Subtype[AddOrder],
    Subtype[Gap])
}
