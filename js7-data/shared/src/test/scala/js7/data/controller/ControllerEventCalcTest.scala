package js7.data.controller

import cats.syntax.semigroup.*
import js7.base.test.OurTestSuite
import js7.base.time.TimestampForTests.ts
import js7.base.time.Timezone
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.board.{BoardPath, PlannableBoard}
import js7.data.controller.ControllerEvent.ControllerInitialized
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventCalc, EventColl, EventId, NoKeyEvent, TimeCtx}
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemAdded
import js7.data.item.{InventoryItemEvent, ItemRevision}
import js7.data.plan.{PlanSchema, PlanSchemaId}

/** Test EventCalc[ControllerState, Event, TimeCtx]. */
final class ControllerEventCalcTest extends OurTestSuite:

  "Combine two EventCalcs and calculate events and aggregate" in:
    val initiallyStartedAt = ts"2025-03-11T12:00:00Z"
    val planSchema = PlanSchema(PlanSchemaId("DailyPlan"), PlanSchema.EachUnknownPlanIsClosed,
      itemRevision = Some(ItemRevision(0)))
    val board = PlannableBoard(BoardPath("BOARD"), itemRevision = Some(ItemRevision(0)))

    // First EventCalc //
    val controllerInitialized: EventCalc[ControllerState, ControllerInitialized, TimeCtx] =
      EventCalc: coll =>
        coll.add:
          NoKey <-: ControllerInitialized(ControllerId("Controller"), coll.context.now)

    // Second EventCalc //
    val itemAdded: EventCalc[ControllerState, InventoryItemEvent, Any] =
      EventCalc.add(
        NoKey <-: UnsignedSimpleItemAdded(planSchema),
        NoKey <-: UnsignedSimpleItemAdded(board))

    // Combined EventCalc //
    val eventCalc: EventCalc[ControllerState, NoKeyEvent, TimeCtx] =
      EventCalc.combine(controllerInitialized.widen, itemAdded.widen)

    // Calculate EventColl with events and aggregate //
    val context = TimeCtx(initiallyStartedAt)
    val eventColl: EventColl[ControllerState, NoKeyEvent, TimeCtx] =
      eventCalc.calculate(ControllerState.empty, context).orThrow

    assert(eventColl.keyedEvents == Vector(
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
        board.path -> board.toInitialItemState)))

    locally:
      // Test combine and |+| //
      val eventColl1 = controllerInitialized.combine(itemAdded.widen)
        .calculate(ControllerState.empty, context).orThrow

      val eventColl2 = (controllerInitialized.widen |+| itemAdded.widen)
        .calculate(ControllerState.empty, context).orThrow

      assert(eventColl1 == eventColl)
      assert(eventColl2 == eventColl)
