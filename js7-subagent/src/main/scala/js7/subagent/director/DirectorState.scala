package js7.subagent.director

import cats.effect.IO
import cats.syntax.monoid.*
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.RichMap
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, Atomic}
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentId, SubagentItem}
import js7.data.value.{MissingValue, NumberValue}
import js7.data.value.expression.{Expression, Scope}
import js7.subagent.configuration.DirectorConf
import js7.subagent.director.DirectorState.*
import js7.subagent.director.priority.Prioritized
import scala.annotation.tailrec
import scala.collection.MapView

private final case class DirectorState private(
  subagentToEntry: Map[SubagentId, SubagentEntry],
  private val bundleToEntry: Map[SubagentBundleId, BundleEntry],
  conf: DirectorConf):

  /** Without a SubagentBundle, we go round-robin through all Subagents. */
  private lazy val bundlelessRoundRobin = Prioritized.roundRobin(subagentToEntry.keys.toVector)

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

  def insertOrReplaceBundle(bundle: SubagentBundle): Checked[DirectorState] =
    logger.trace(s"insertOrReplaceBundle $bundle")
    Right(copy(
      bundleToEntry = bundleToEntry.updated(
        bundle.id,
        BundleEntry(
          bundle,
          bundle.subagentToPriority.map: (subagentId, expr) =>
            subagentId -> expr))))

  def removeBundle(bundleId: SubagentBundleId): DirectorState =
    logger.trace("removeBundle", bundleId)
    copy(
      bundleToEntry = bundleToEntry - bundleId)

  def clear: DirectorState =
    logger.trace("clear")
    copy(
      subagentToEntry = Map.empty,
      bundleToEntry = Map.empty)

  def selectNext(maybeBundleId: Option[SubagentBundleId], scope: Scope)
  : Checked[Option[SubagentDriver]] =
    maybeBundleId match
      case Some(bundleId) if !bundleToEntry.contains(bundleId) =>
        // A SubagentBundleId, if not defined, may denote a Subagent
        subagentToEntry
          .checked(bundleId.toSubagentId) // May be non-existent when stopping ???
          .map(o => Some(o.driver))

      case _ =>
        Right:
          maybeBundleId.match
            case None =>
              Some(bundlelessRoundRobin)
            case Some(bundleId) =>
              bundleToEntry.get(bundleId).map: entry => // May be non-existent when stopping
                if entry.subagentBundle.allPrioritiesArePure then
                  entry.cachedStaticPrioritized(scope)
                else
                  entry.cachedDynamicPrioritized: subagentId =>
                    idToDriver.get(subagentId)
                      .flatMap: driver =>
                        driver.serverMeteringScope()
                          .map(_ |+| driver.subagentProcessCountScope |+| scope)
          .flatMap: prioritized =>
            prioritized.selectNext(isAvailable).flatMap: subagentId =>
              subagentToEntry.get(subagentId).map(_.driver)

  private def isAvailable(subagentId: SubagentId) =
    subagentToEntry.get(subagentId).fold(false)(_.isAvailable)

  override def toString =
    s"DirectorState(${subagentToEntry.values.toSeq}, $bundleToEntry)"


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


  private final case class BundleEntry(
    subagentBundle: SubagentBundle,
    subagentToExpr: Map[SubagentId, Expression]):
    entry =>

    private val _cachedPrioritized = Atomic[Prioritized[SubagentId] | Null](null)

    // Cache may be updated in parallel
    def cachedStaticPrioritized(scope: Scope): Prioritized[SubagentId] =
      // Because priorities does not depend on a Scope, we evaluate Prioritized only once
      _cachedPrioritized.get() match
        case null =>
          val prioritized = mkPrioritized(_ => Some(scope))
          _cachedPrioritized.compareAndExchange(null, prioritized) match
            case null =>
              logger.trace(s"cachedPrioritized: $bundleId new $prioritized")
              prioritized
            case cached: Prioritized[SubagentId] =>
              logger.trace(s"cachedPrioritized: $bundleId reuse $cached")
              cached
        case cached: Prioritized[SubagentId] =>
          logger.trace(s"cachedPrioritized: $bundleId reuse $cached")
          cached

    // Cache may be updated in parallel
    def cachedDynamicPrioritized(toScope: SubagentId => Option[Scope]): Prioritized[SubagentId] =
      val prioritized = mkPrioritized(toScope)
      @tailrec def cache(): Prioritized[SubagentId] =
        _cachedPrioritized.get() match
          // subagentBundle.allPrioritiesArePure implies isEquivalentTo
          case cached: Prioritized[SubagentId] if cached.isEquivalentTo(prioritized) =>
            // Keep MutableRoundRobin index in Prioritized
            logger.trace(s"cachedPrioritized: $bundleId reuse $cached")
            cached
          case cached =>
            if _cachedPrioritized.compareAndSet(cached, prioritized) then
              logger.trace(s"cachedPrioritized: $bundleId new $prioritized")
              prioritized
            else
              cache()
      cache()

    private def mkPrioritized(toScope: SubagentId => Option[Scope]): Prioritized[SubagentId] =
      Prioritized:
        subagentToExpr.toVector.flatMap: (subagentId, expr) =>
          toScope(subagentId).flatMap: scope =>
            expr.eval(scope).flatMap(_.asMissingOr[NumberValue]) match
              case Left(problem) =>
                logger.error(s"$bundleId: $subagentId priority expression failed with $problem")
                None // Subagent is not selected
              case Right(MissingValue) =>
                logger.trace(s"$bundleId $subagentId priority expression returns MissingValue")
                None
              case Right(o: NumberValue) =>
                Some(subagentId -> o)

    private def bundleId =
      subagentBundle.id
