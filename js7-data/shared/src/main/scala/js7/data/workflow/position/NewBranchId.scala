package js7.data.workflow.position

import js7.base.problem.Checked.catchNonFatal
import js7.base.problem.{Checked, Problem}
import js7.data.order.CycleState
import js7.data.workflow.instructions.ForkBranchId

/** An executing instruction block nested in an instruction is named NewBranchId.
  * <p>
  * For example, the NewBranchIds of a ForList instruction are named after ForkList index value.
  *
  * @param toStatic The static (non-execution) representation
  * @param isMoveBoundary Whether an order can be moved through a move boundary.
  */
enum NewBranchId(
  val toStatic: BlockNesting.BlockId,
  val isMoveBoundary: Boolean,
  val toLegacy: BranchId):

  case Options extends NewBranchId(
    toStatic = BlockNesting.BlockId.Options,
    isMoveBoundary = false,
    toLegacy = BranchId.Options)

  case Then(index: Int = 1) extends NewBranchId(
    toStatic = BlockNesting.BlockId.Then,
    isMoveBoundary = false,
    toLegacy = BranchId.then_(index))

  case Else extends NewBranchId(
    toStatic = BlockNesting.BlockId.Else,
    isMoveBoundary = false,
    toLegacy = BranchId.Else)

  case Try(index: Int) extends NewBranchId(
    toStatic = BlockNesting.BlockId.Try,
    isMoveBoundary = false,
    toLegacy = BranchId.try_(index))

  case Catch(index: Int) extends NewBranchId(
    toStatic = BlockNesting.BlockId.Catch,
    isMoveBoundary = false,
    toLegacy = BranchId.catch_(index))

  case Cycle(cycleState: CycleState) extends NewBranchId(
    toStatic = BlockNesting.BlockId.Cycle,
    isMoveBoundary = false,
    toLegacy = BranchId.cycle(cycleState))

  case ConsumeNotices extends NewBranchId(
    toStatic = BlockNesting.BlockId.ConsumeNotices,
    isMoveBoundary = true,
    toLegacy = BranchId.ConsumeNotices)

  case StickySubagent extends NewBranchId(
    toStatic = BlockNesting.BlockId.StickySubagent,
    isMoveBoundary = true,
    toLegacy = BranchId.StickySubagent)

  case Lock extends NewBranchId(
    toStatic = BlockNesting.BlockId.Lock,
    isMoveBoundary = true,
    toLegacy = BranchId.Lock)

  case Fork(branchId: ForkBranchId) extends NewBranchId(
    toStatic = BlockNesting.BlockId.Fork(branchId),
    isMoveBoundary = true,
    toLegacy = BranchId.fork(branchId.string))

  case ForkList(branchId: ForkBranchId) extends NewBranchId(
    toStatic = BlockNesting.BlockId.ForkList,
    isMoveBoundary = true,
    toLegacy = BranchId.ForkList)


case object NewBranchId:

  def fromLegacy(branchId: BranchId): Checked[NewBranchId] =
    branchId match
      case BranchId.Options => Right(Options)
      case BranchId.Then => Right(Then())
      case BranchId.Else => Right(Else)
      case BranchId.ConsumeNotices => Right(ConsumeNotices)
      case BranchId.StickySubagent => Right(StickySubagent)
      case BranchId.Lock => Right(Lock)
      case _ =>
        val string = branchId.string
        if string.startsWith(BranchId.ThenPrefix) then
          catchNonFatal:
            Then(string.drop(BranchId.ThenPrefix.length).toInt)
        else if string.startsWith(BranchId.TryPrefix) then
          catchNonFatal:
            Try(string.drop(BranchId.TryPrefix.length).toInt)
        else if string.startsWith(BranchId.CatchPrefix) then
          catchNonFatal:
            Catch(string.drop(BranchId.CatchPrefix.length).toInt)
        else if branchId.isCycle then
          branchId.toCycleState
            .map(NewBranchId.Cycle(_))
        else if string.startsWith(BranchId.ForkPrefix) then
          //ForkBranchId.checked(string.drop(BranchId.ForkPrefix.length))
          //  .map(NewBranchId.Fork(_))
          // TODO BranchId.fork is not decidable
          Left(Problem:
            s"The legacy BranchId:${branchId.string} can be both a NewBranchId.Fork and a NewBranchId.ForkList")
        else
          Left(Problem(s"Invalid NewBranchId: $branchId"))
