package js7.data.controller

import cats.syntax.semigroup.*
import js7.base.test.OurTestSuite
import js7.base.time.TimestampForTests.ts
import js7.base.time.Timezone
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.board.{BoardPath, PlannableBoard}
import js7.data.controller.ControllerEvent.ControllerInitialized
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventCalc, EventCalcCtx, EventCollCtx, NoKeyEvent, TimeCtx}
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemAdded
import js7.data.item.{InventoryItemEvent, ItemRevision}
import js7.data.plan.{PlanSchema, PlanSchemaId}
import js7.data.state.EngineStateStatistics

/** Test EventCalc[ControllerState, Event]. */
final class ControllerEventCalcTest extends OurTestSuite:

  "Combine two EventCalcs and calculate events and aggregate" in:
    val initiallyStartedAt = ts"2025-03-11T12:00:00Z"
    val planSchema = PlanSchema(PlanSchemaId("DailyPlan"), PlanSchema.EachUnknownPlanIsClosed,
      itemRevision = Some(ItemRevision(0)))
    val board = PlannableBoard(BoardPath("BOARD"), itemRevision = Some(ItemRevision(0)))

    // First EventCalc //
    val controllerInitialized: EventCalcCtx[ControllerState, ControllerInitialized, TimeCtx] =
      EventCalc.single: _ =>
        NoKey <-: ControllerInitialized(ControllerId("Controller"), EventCalc.now)

    // Second EventCalc //
    val itemAdded: EventCalcCtx[ControllerState, InventoryItemEvent, Any] =
      EventCalcCtx.pure(
        NoKey <-: UnsignedSimpleItemAdded(planSchema),
        NoKey <-: UnsignedSimpleItemAdded(board))

    // Combined EventCalc //
    val eventCalc: EventCalcCtx[ControllerState, NoKeyEvent, TimeCtx] =
      controllerInitialized.widen |+| itemAdded.widen

    // Calculate EventColl with events and aggregate //
    val context = TimeCtx(initiallyStartedAt)
    val eventColl: EventCollCtx[ControllerState, NoKeyEvent, TimeCtx] =
      eventCalc.calculate(ControllerState.empty, context).orThrow

    assert(eventColl.keyedEvents.toVector == Vector(
      NoKey <-: ControllerInitialized(ControllerId("Controller"), initiallyStartedAt),
      NoKey <-: UnsignedSimpleItemAdded(planSchema),
      NoKey <-: UnsignedSimpleItemAdded(board)))

    assert(eventColl.aggregate == ControllerState.empty.copy(
      controllerMetaState = ControllerMetaState(
        ControllerId("Controller"),
        initiallyStartedAt,
        Timezone.utc),
      keyToUnsignedItemState_ = Map(
        PlanSchema.Global.path -> PlanSchema.Global.toInitialItemState,
        planSchema.path -> planSchema.toInitialItemState,
        board.path -> board.toInitialItemState),
      statistics = EngineStateStatistics.empty/*(
        eventCounter = EventCounter(Map(
          "ControllerInitialized" -> 1,
          "UnsignedSimpleItemAdded" -> 2)))*/))

    locally:
      val eventColl2 = (controllerInitialized.widen |+| itemAdded.widen)
        .calculate(ControllerState.empty, context).orThrow

      assert(eventColl2 == eventColl)
