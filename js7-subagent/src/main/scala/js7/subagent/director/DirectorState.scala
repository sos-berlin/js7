package js7.subagent.director

import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Allocated
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.subagent.{SubagentId, SubagentItem, SubagentSelection, SubagentSelectionId}
import js7.subagent.director.DirectorState.*
import cats.effect.IO
import scala.collection.MapView

private final case class DirectorState(
  subagentToEntry: Map[SubagentId, Entry],
  selectionToPrioritized: Map[Option[SubagentSelectionId], Prioritized[SubagentId]]):

  val idToDriver: MapView[SubagentId, SubagentDriver] =
    subagentToEntry.view.mapValues(_.driver)

  def idToAllocatedDriver: MapView[SubagentId, Allocated[IO, SubagentDriver]] =
    subagentToEntry.view.mapValues(_.allocatedDriver)

  def insertSubagentDriver(
    allocatedDriver: Allocated[IO, SubagentDriver],
    disabled: Boolean = false)
  : Checked[DirectorState] =
    logger.trace("insertSubagentDriver", s"$allocatedDriver, disabled=$disabled")
    val driver = allocatedDriver.allocatedThing
    val subagentId = driver.subagentId
    subagentToEntry
      .insert(subagentId -> Entry(allocatedDriver, disabled))
      .map: idToE =>
        update(idToE, driver, disabled)

  def replaceSubagentDriver(
    allocatedDriver: Allocated[IO, SubagentDriver],
    subagentItem: SubagentItem)
  : Checked[DirectorState] =
    logger.trace("replaceSubagentDriver", s"$allocatedDriver, $subagentItem")
    val driver = allocatedDriver.allocatedThing
    val subagentId = driver.subagentId
    if !subagentToEntry.contains(subagentId) then
      Left(Problem(s"Replacing unknown $subagentId SubagentDriver"))
    else
      Right(update(
        subagentToEntry = subagentToEntry.updated(
          subagentId,
          Entry(allocatedDriver, subagentItem.disabled)),
        driver,
        subagentItem.disabled))

  def removeSubagent(subagentId: SubagentId): DirectorState = {
    logger.trace("removeSubagent", subagentId)
    copy(
      subagentToEntry = subagentToEntry.removed(subagentId),
      selectionToPrioritized = selectionToPrioritized
        .updated(None, selectionToPrioritized(None).remove(subagentId)))
  }

  def setDisabled(id: SubagentId, disabled: Boolean): Checked[DirectorState] =
    logger.trace("setDisabled", s"$id, disabled=$disabled")
    Right(
      subagentToEntry
        .get(id)
        .filter(_.disabled != disabled)
        .fold(this): entry =>
          update(
            subagentToEntry = subagentToEntry.updated(id, entry.copy(disabled = disabled)),
            entry.driver,
            disabled))

  private def update(
    subagentToEntry: Map[SubagentId, Entry],
    driver: SubagentDriver,
    disabled: Boolean)
  : DirectorState =
    copy(
      subagentToEntry = subagentToEntry,
      selectionToPrioritized = updatePriorization(driver, disabled))

  private def updatePriorization(driver: SubagentDriver, disabled: Boolean)
  : Map[Option[SubagentSelectionId], Prioritized[SubagentId]] =
    val subagentId = driver.subagentId
    selectionToPrioritized.updated(None,
      if disabled then
        selectionToPrioritized(None).remove(subagentId)
      else if driver.isInstanceOf[LocalSubagentDriver] then
        selectionToPrioritized(None).insertFirst(subagentId)
      else
        selectionToPrioritized(None).add(subagentId))

  def insertOrReplaceSelection(selection: SubagentSelection): Checked[DirectorState] =
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

  def removeSelection(selectionId: SubagentSelectionId): DirectorState =
    logger.trace("removeSelection", selectionId)
    copy(selectionToPrioritized = selectionToPrioritized - Some(selectionId))

  def clear: DirectorState =
    logger.trace("clear")
    copy(
      subagentToEntry = Map.empty,
      selectionToPrioritized = Map.empty)

  def selectNext(maybeSelectionId: Option[SubagentSelectionId]): Checked[Option[SubagentDriver]] =
    maybeSelectionId match
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


private object DirectorState:
  private val logger = Logger[this.type]
  private val DefaultPriority = 0

  final case class Entry(
    allocatedDriver: Allocated[IO, SubagentDriver],
    disabled: Boolean = false):
    val driver: SubagentDriver =
      allocatedDriver.allocatedThing

    def isAvailable: Boolean =
      !disabled && driver.isCoupled
