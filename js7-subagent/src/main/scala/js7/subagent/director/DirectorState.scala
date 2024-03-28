package js7.subagent.director

import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Allocated
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.subagent.{SubagentId, SubagentItem, SubagentSelection, SubagentSelectionId}
import js7.subagent.director.DirectorState.*
import monix.eval.Task
import scala.collection.MapView

private final case class DirectorState(
  subagentToEntry: Map[SubagentId, Entry],
  selectionToPrioritized: Map[Option[SubagentSelectionId], Prioritized[SubagentId]])
{
  val idToDriver: MapView[SubagentId, SubagentDriver] =
    subagentToEntry.view.mapValues(_.driver)

  def idToAllocatedDriver: MapView[SubagentId, Allocated[Task, SubagentDriver]] =
    subagentToEntry.view.mapValues(_.allocatedDriver)

  def insertSubagentDriver(
    allocatedDriver: Allocated[Task, SubagentDriver],
    disabled: Boolean = false)
  : Checked[DirectorState] = {
    logger.trace("insertSubagentDriver", s"$driver, disabled=$disabled")
    val subagentId = allocatedDriver.allocatedThing.subagentId
    subagentToEntry
      .insert(subagentId -> Entry(allocatedDriver, disabled))
      .map(idToE => update(
        subagentToEntry = idToE,
        subagentId,
        disabled))
  }

  def replaceSubagentDriver(
    allocatedDriver: Allocated[Task, SubagentDriver],
    subagentItem: SubagentItem)
  : Checked[DirectorState] = {
    logger.trace("replaceSubagentDriver", s"$driver, $subagentItem")
    val subagentId = allocatedDriver.allocatedThing.subagentId
    if (!subagentToEntry.contains(subagentId))
      Left(Problem(s"Replacing unknown $subagentId SubagentDriver"))
    else
      Right(update(
        subagentToEntry = subagentToEntry.updated(
          subagentId,
          Entry(allocatedDriver, subagentItem.disabled)),
        subagentId,
        subagentItem.disabled))
  }

  def removeSubagent(subagentId: SubagentId): DirectorState = {
    logger.trace("removeSubagent", subagentId)
    update(
      subagentToEntry = subagentToEntry.removed(subagentId),
      // Remove SubagentId from default SubagentSelection
      subagentId,
      disabled = true/*remove from selectionToPrioritized*/)
  }

  def setDisabled(id: SubagentId, disabled: Boolean): Checked[DirectorState] = {
    logger.trace("setDisabled", s"$id, disabled=$disabled")
    Right(
      subagentToEntry
        .get(id)
        .filter(_.disabled != disabled)
        .fold(this)(entry =>
          update(
            subagentToEntry = subagentToEntry.updated(id, entry.copy(disabled = disabled)),
            id,
            disabled)))
  }

  private def update(
    subagentToEntry: Map[SubagentId, Entry],
    subagentId: SubagentId,
    disabled: Boolean)
  : DirectorState =
    copy(
      subagentToEntry = subagentToEntry,
      selectionToPrioritized = updateSelectionToPrioritized(subagentId, disabled))

  private def updateSelectionToPrioritized(subagentId: SubagentId, disabled: Boolean)
  : Map[Option[SubagentSelectionId], Prioritized[SubagentId]] =
    selectionToPrioritized.updated(None,
      if (disabled)
        selectionToPrioritized(None).remove(subagentId)
      else if (allocatedDriver.allocatedThing.isInstanceOf[LocalSubagentDriver])
        selectionToPrioritized(None).insertFirst(subagentId)
      else
        selectionToPrioritized(None).add(subagentId))

  def insertOrReplaceSelection(selection: SubagentSelection): Checked[DirectorState] = {
    logger.trace("insertOrReplaceSelection", selection)
    Right(copy(
      selectionToPrioritized = selectionToPrioritized.updated(
        Some(selection.id),
        Prioritized[SubagentId](
          selection.subagentToPriority.keys,
          id => selection.subagentToPriority.getOrElse(id, {
            logger.error(s"${selection.id} uses unknown $id. Assuming priority=$DefaultPriority")
            DefaultPriority
          })))))
  }

  def removeSelection(selectionId: SubagentSelectionId): DirectorState = {
    logger.trace("removeSelection", selectionId)
    copy(selectionToPrioritized = selectionToPrioritized - Some(selectionId))
  }

  def clear: DirectorState = {
    logger.trace("clear")
    copy(
      subagentToEntry = Map.empty,
      selectionToPrioritized = Map.empty)
  }

  def selectNext(maybeSelectionId: Option[SubagentSelectionId]): Checked[Option[SubagentDriver]] =
    maybeSelectionId match {
      case Some(selectionId) if !selectionToPrioritized.contains(maybeSelectionId) =>
        // A SubagentSelectionId, if not defined, may denote a Subagent
        subagentToEntry
          .checked(selectionId.toSubagentId) // May be non-existent when stopping ???
          .map(o => Some(o.driver))

      case _ =>
        Right(selectionToPrioritized
          .get(maybeSelectionId) // May be non-existent when stopping
          .flatMap(_
            .selectNext(subagentId =>
              subagentToEntry.get(subagentId).fold(false)(_.isAvailable)))
          .flatMap(subagentId =>
            subagentToEntry.get(subagentId).map(_.driver)))
    }
}

private object DirectorState
{
  private val logger = Logger[this.type]
  private val DefaultPriority = 0

  final case class Entry(
    allocatedDriver: Allocated[Task, SubagentDriver],
    disabled: Boolean = false)
  {
    val driver: SubagentDriver =
      allocatedDriver.allocatedThing

    def isAvailable: Boolean =
      !disabled && driver.isCoupled
  }
}
