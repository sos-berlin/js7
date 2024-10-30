package js7.data.workflow.position

import js7.base.problem.Checked.catchNonFatal
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.CycleState
import js7.data.workflow.instructions.ForkBranchId

trait NewBranchId(val toStatic: BlockNesting.BlockId):
  def toLegacy: BranchId


case object NewBranchId:

  case object Options extends NewBranchId(BlockNesting.BlockId.Options):
    def toLegacy: BranchId.Named = BranchId.Options

  case object Then extends NewBranchId(BlockNesting.BlockId.Then):
    def toLegacy = BranchId.Then

  case object Else extends NewBranchId(BlockNesting.BlockId.Else):
    def toLegacy = BranchId.Else

  final case class Try(index: Int) extends NewBranchId(BlockNesting.BlockId.Try):
    def toLegacy = BranchId.try_(index)

  final case class Catch(index: Int) extends NewBranchId(BlockNesting.BlockId.Catch):
    def toLegacy = BranchId.catch_(index)

  final case class Cycle(cycleState: CycleState) extends NewBranchId(BlockNesting.BlockId.Cycle):
    def toLegacy = BranchId.cycle(cycleState)

  case object ConsumeNotices extends NewBranchId(BlockNesting.BlockId.ConsumeNotices):
    def toLegacy = BranchId.ConsumeNotices

  case object StickySubagent extends NewBranchId(BlockNesting.BlockId.StickySubagent):
    def toLegacy = BranchId.StickySubagent

  case object Lock extends NewBranchId(BlockNesting.BlockId.Lock):
    def toLegacy = BranchId.Lock

  final case class Fork(branchId: ForkBranchId)
    extends NewBranchId(BlockNesting.BlockId.Fork(branchId)):
    def toLegacy = BranchId.fork(branchId.string)

  case object ForkList extends NewBranchId(BlockNesting.BlockId.ForkList):
    def toLegacy = BranchId.ForkList

  def fromLegacy(branchId: BranchId): Checked[NewBranchId] =
    branchId match
      case BranchId.Options => Right(Options)
      case BranchId.Then => Right(Then)
      case BranchId.Else => Right(Else)
      case BranchId.ConsumeNotices => Right(ConsumeNotices)
      case BranchId.StickySubagent => Right(StickySubagent)
      case BranchId.Lock => Right(Lock)
      case BranchId.ForkList => Right(ForkList)
      case _ =>
        val string = branchId.string
        if string.startsWith(BranchId.TryPrefix) then
          catchNonFatal:
            Try(string.drop(BranchId.TryPrefix.length).toInt)
        else if string.startsWith(BranchId.CatchPrefix) then
          catchNonFatal:
            Catch(string.drop(BranchId.CatchPrefix.length).toInt)
        else if branchId.isCycle then
          branchId.toCycleState
            .map(NewBranchId.Cycle(_))
        else if string.startsWith(BranchId.ForkPrefix) then
          ForkBranchId.checked(string.drop(BranchId.ForkPrefix.length))
            .map(NewBranchId.Fork(_))
        else
          Left(Problem(s"Invalid NewBranchId: $branchId"))
