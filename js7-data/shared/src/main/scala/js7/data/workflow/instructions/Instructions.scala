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
    Subtype.named1[ExplicitEnd]("End"),
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
    Subtype[ConsumeNotices],
    Subtype[Retry],
    Subtype.named1[TryInstruction]("Try"),
    Subtype.named1[LockInstruction]("Lock"),
    Subtype[Cycle],
    Subtype[StickySubagent],
    Subtype[AddOrder],
    Subtype[Options],
    Subtype[Stop],
    Subtype[EmptyInstruction],
    Subtype[Gap])
}
