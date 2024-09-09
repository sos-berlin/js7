package js7.subagent.director

import cats.effect.IO
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Allocated
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.subagent.{SubagentId, SubagentItem, SubagentSelection, SubagentSelectionId}
import js7.data.value.NumberValue
import js7.data.value.expression.{Expression, Scope}
import js7.subagent.configuration.DirectorConf
import js7.subagent.director.DirectorState.*
import js7.subagent.director.priority.Prioritized
import scala.collection.MapView

private final case class DirectorState private(
  subagentToEntry: Map[SubagentId, SubagentEntry],
  private val selectionToEntry: Map[SubagentSelectionId, SelectionEntry],
  conf: DirectorConf):

  /** Without a SubagentSelection, we go round-robin through all Subagents. */
  private lazy val selectionlessRoundRobin = Prioritized.roundRobin(subagentToEntry.keys.toVector)

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
      .insert(subagentId -> SubagentEntry(allocatedDriver, disabled))
      .map: idToE =>
        copy(
          subagentToEntry = idToE)

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
      Right(copy(
        subagentToEntry = subagentToEntry.updated(
          subagentId,
          SubagentEntry(allocatedDriver, subagentItem.disabled))))

  def removeSubagent(subagentId: SubagentId): DirectorState =
    logger.trace("removeSubagent", subagentId)
    copy(
      subagentToEntry = subagentToEntry.removed(subagentId))

  def setDisabled(id: SubagentId, disabled: Boolean): Checked[DirectorState] =
    logger.trace(s"setDisabled $id, disabled=$disabled")
    Right:
      subagentToEntry
        .get(id)
        .filter(_.disabled != disabled)
        .fold(this): entry =>
          copy(
            subagentToEntry = subagentToEntry.updated(id, entry.copy(
              disabled = disabled)))

  def insertOrReplaceSelection(selection: SubagentSelection): Checked[DirectorState] =
    logger.trace("insertOrReplaceSelection", selection)
    Right(copy(
      selectionToEntry = selectionToEntry.updated(
        selection.id,
        SelectionEntry(
          selection,
          selection.subagentToPriority.map: (subagentId, expr) =>
            subagentId -> expr))))

  def removeSelection(selectionId: SubagentSelectionId): DirectorState =
    logger.trace("removeSelection", selectionId)
    copy(
      selectionToEntry = selectionToEntry - selectionId)

  def clear: DirectorState =
    logger.trace("clear")
    copy(
      subagentToEntry = Map.empty,
      selectionToEntry = Map.empty)

  def selectNext(maybeSelectionId: Option[SubagentSelectionId]): Checked[Option[SubagentDriver]] =
    maybeSelectionId match
      case Some(selectionId) if !selectionToEntry.contains(selectionId) =>
        // A SubagentSelectionId, if not defined, may denote a Subagent
        subagentToEntry
          .checked(selectionId.toSubagentId) // May be non-existent when stopping ???
          .map(o => Some(o.driver))

      case _ =>
        Right:
          maybeSelectionId.match
            case None =>
              Some(selectionlessRoundRobin)
            case Some(selectionId) =>
              selectionToEntry
                .get(selectionId) // May be non-existent when stopping
                .map(cachedPrioritized)
          .flatMap: prioritized =>
            prioritized.selectNext(isAvailable).flatMap: subagentId =>
              subagentToEntry.get(subagentId).map(_.driver)

  private def cachedPrioritized(entry: SelectionEntry): Prioritized[SubagentId] =
    if entry.subagentSelection.allPrioritiesArePure then
      // Evaluate Prioritized only once
      if entry.cachedPrioritized == null then
        val prioritized = toPrioritized(entry)
        entry.cachedPrioritized = prioritized
        prioritized
      else
        entry.cachedPrioritized
    else
      val prioritized = toPrioritized(entry)
      entry.cachedPrioritized match
        case cached: Prioritized[SubagentId] if cached.isEquivalentTo(prioritized) =>
          // Keep MutableRoundRobin index in Prioritized
          cached
        case _ =>
          entry.cachedPrioritized = prioritized
          prioritized

  private def toPrioritized(entry: SelectionEntry): Prioritized[SubagentId] =
    Prioritized:
      entry.subagentIds.flatMap: subagentId =>
        subagentToEntry.get(subagentId)
          .map(_.driver)
          .flatMap: driver =>
            val subagentId = driver.subagentId
            entry.subagentToExpr.get(subagentId).flatMap: expr =>
              expr.eval(Scope.empty).flatMap(_.toNumberValue) match
                case Left(problem) =>
                  val id = entry.subagentSelection.id
                  logger.error(s"$id: $subagentId priority expression failed with $problem")
                  None // Subagent is not selected
                case Right(prio) =>
                  Some(subagentId -> prio)

  private def isAvailable(subagentId: SubagentId) =
    subagentToEntry.get(subagentId).fold(false)(_.isAvailable)

  override def toString =
    s"DirectorState(${subagentToEntry.values.toSeq}, $selectionToEntry)"


private object DirectorState:
  private val logger = Logger[this.type]

  def initial(conf: DirectorConf): DirectorState =
    DirectorState(Map.empty, Map.empty, conf)


  final case class SubagentEntry private[DirectorState](
    allocatedDriver: Allocated[IO, SubagentDriver],
    disabled: Boolean = false):

    val driver: SubagentDriver =
      allocatedDriver.allocatedThing

    def isAvailable: Boolean =
      !disabled && driver.isCoupled

    override def toString = s"DirectorEntry${allocatedDriver.allocatedThing.subagentId
      }${disabled ?? s" disabled"} isAvailable=$isAvailable)"


  private final case class SelectionEntry(
    subagentSelection: SubagentSelection,
    subagentToExpr: Map[SubagentId, Expression]):

    var cachedPrioritized: Prioritized[SubagentId] | Null = null

    def subagentIds =
      subagentSelection.subagentIds.toVector
