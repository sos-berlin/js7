package js7.data.workflow.position

import cats.syntax.traverse.*
import js7.base.problem.{Checked, Problem}
import js7.data.workflow.instructions.ForkBranchId
import js7.data.workflow.position.BlockNesting.*

opaque type BlockNesting = List[Segment]

object BlockNesting:

  extension (reverseList: BlockNesting)
    def toLegacy: BranchPath =
      reverseList.view.map(_.toLegacy).reverse.toList


  val empty: BlockNesting = Nil

  inline def fromReverseList(reverseList: List[Segment]): BlockNesting =
    reverseList

  def fromLegacy(branchPath: BranchPath): Checked[BlockNesting] =
    branchPath.traverse: segment =>
      val BranchPath.Segment(nr, branchId) = segment
      BlockId.fromLegacy(branchId).map(Segment(nr, _))
    .map: segments =>
      segments.reverse


  final case class Segment(nr: InstructionNr, blockId: BlockId):
    def toLegacy: BranchPath.Segment =
      BranchPath.Segment(nr, blockId.toLegacyBranchId)


  enum BlockId private(val toLegacyBranchId: BranchId):
    case Options extends BlockId(BranchId.Options)
    case Then extends BlockId(BranchId.Then)
    case Else extends BlockId(BranchId.Else)
    case Try extends BlockId(BranchId.Try_)
    case Catch extends BlockId(BranchId.Catch_)
    case Cycle extends BlockId(BranchId.Cycle)
    case ConsumeNotices extends BlockId(BranchId.ConsumeNotices)
    case StickySubagent extends BlockId(BranchId.StickySubagent)
    case Lock extends BlockId(BranchId.Lock)
    case Fork(branchId: ForkBranchId) extends BlockId(BranchId.fork(branchId.string))
    case ForkList extends BlockId(BranchId.ForkList)

  object BlockId:
    def fromLegacy(branchId: BranchId): Checked[BlockId] =
      branchId match
        case BranchId.Options => Right(Options)
        case BranchId.Then => Right(Then)
        case BranchId.Else => Right(Else)
        case BranchId.Try_ => Right(Try)
        case BranchId.Catch_ => Right(Catch)
        case BranchId.Cycle => Right(Cycle)
        case BranchId.ConsumeNotices => Right(ConsumeNotices)
        case BranchId.StickySubagent => Right(StickySubagent)
        case BranchId.Lock => Right(Lock)
        case BranchId.ForkList => Right(ForkList)
        case _ =>
          if branchId.string.startsWith(BranchId.ForkPrefix) then
            ForkBranchId.checked(branchId.string.drop(BranchId.ForkPrefix.length))
              .map(BlockId.Fork(_))
          else
            Left(Problem(s"Unknown BranchId $branchId"))
