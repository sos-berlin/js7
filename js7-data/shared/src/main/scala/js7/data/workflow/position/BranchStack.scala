package js7.data.workflow.position

import cats.syntax.traverse.*
import js7.base.problem.Checked
import js7.data.workflow.position.BranchStack.*


opaque type BranchStack = List[Segment]


object BranchStack:

  extension (branchStack: BranchStack)
    def toStatic: BlockNesting =
      BlockNesting.fromReverseList:
        branchStack.map(_.toStatic)

    def toLegacy: BranchPath =
      branchStack.view.map(_.toLegacy).reverse.toList


  val empty: BranchStack = Nil

  def fromReverseList(reverseList: List[Segment]): BranchStack = reverseList

  def fromLegacy(branchPath: BranchPath): Checked[BranchStack] =
    branchPath.traverse: segment =>
      val BranchPath.Segment(nr, branchId) = segment
      NewBranchId.fromLegacy(branchId).map(BranchStack.Segment(nr, _))
    .map: segments =>
      segments.reverse


  final case class Segment(nr: InstructionNr, branchId: NewBranchId):
    def toStatic: BlockNesting.Segment =
      BlockNesting.Segment(nr, branchId.toStatic)

    def toLegacy: BranchPath.Segment =
      BranchPath.Segment(nr, branchId.toLegacy)
