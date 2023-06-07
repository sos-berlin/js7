package js7.subagent.director

import js7.base.io.process.ProcessSignal
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
    val subagentId = allocatedDriver.allocatedThing.subagentId
    subagentToEntry
      .insert(subagentId -> Entry(allocatedDriver, disabled))
      .map(idToE => copy(
        subagentToEntry = idToE,
        selectionToPrioritized =
          if (disabled)
            selectionToPrioritized
          else
            // Add SubagentId to default SubagentSelection
            selectionToPrioritized
              .updated(
                None,
                selectionToPrioritized(None).add(subagentId))))
  }

  def replaceSubagentDriver(
    allocatedDriver: Allocated[Task, SubagentDriver],
    subagentItem: SubagentItem)
  : Checked[DirectorState] = {
    val subagentId = allocatedDriver.allocatedThing.subagentId
    if (!subagentToEntry.contains(subagentId))
      Left(Problem(s"Replacing unknown $subagentId SubagentDriver"))
    else
      Right(copy(
        subagentToEntry = subagentToEntry.updated(
          subagentId,
          Entry(allocatedDriver, subagentItem.disabled))))
  }

  def removeSubagent(subagentId: SubagentId): DirectorState =
    copy(
      subagentToEntry = subagentToEntry.removed(subagentId),
      // Remove SubagentId from default SubagentSelection
      selectionToPrioritized =
        selectionToPrioritized.updated(None, selectionToPrioritized(None).remove(subagentId)))

  def insertOrReplaceSelection(selection: SubagentSelection): Checked[DirectorState] =
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
    copy(selectionToPrioritized = selectionToPrioritized - Some(selectionId))

  def clear: DirectorState =
    copy(
      subagentToEntry = Map.empty,
      selectionToPrioritized = Map.empty)

  def setDisabled(id: SubagentId, disabled: Boolean): Checked[DirectorState] =
    Right(
      subagentToEntry
        .get(id)
        .filter(_.disabled != disabled)
        .fold(this)(entry =>
          copy(
            subagentToEntry = subagentToEntry.updated(id, entry.copy(disabled = disabled)))))

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

    def terminate(signal: Option[ProcessSignal]): Task[Unit] =
      driver.terminate(signal)
        .*>(allocatedDriver.release)
  }
}
