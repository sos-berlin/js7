package js7.subagent.director

import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.subagent.{SubagentId, SubagentItem, SubagentSelection, SubagentSelectionId}
import js7.subagent.SubagentDriver
import js7.subagent.director.DirectorState.*

private final case class DirectorState(
  subagentToEntry: Map[SubagentId, Entry],
  selectionToPrioritized: Map[Option[SubagentSelectionId], Prioritized[SubagentId]])
{
  def idToDriver = subagentToEntry.view.mapValues(_.driver)

  def insertSubagentDriver(driver: SubagentDriver, subagentItem: SubagentItem)
  : Checked[DirectorState] =
    insertSubagentDriver(driver, subagentItem.disabled)

  def insertSubagentDriver(driver: SubagentDriver, disabled: Boolean = false)
  : Checked[DirectorState] = {
    logger.trace("insertSubagentDriver", s"$driver, disabled=$disabled")
    subagentToEntry.insert(driver.subagentId -> Entry(driver, disabled))
      .map(idToE => update(
        subagentToEntry = idToE,
        driver.subagentId,
        disabled))
  }

  def replaceSubagentDriver(driver: SubagentDriver, subagentItem: SubagentItem)
  : Checked[DirectorState] = {
    logger.trace("replaceSubagentDriver", s"$driver, $subagentItem")
    if (!subagentToEntry.contains(driver.subagentId))
      Left(Problem(s"Replacing unknown ${driver.subagentId} SubagentDriver"))
    else
      Right(update(
        subagentToEntry = subagentToEntry.updated(
          driver.subagentId,
          Entry(driver, subagentItem.disabled)),
        driver.subagentId,
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
      else
        // Add SubagentId to default SubagentSelection
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

  final case class Entry(driver: SubagentDriver, disabled: Boolean = false) {
    def isAvailable = !disabled && driver.isCoupled
  }
}
