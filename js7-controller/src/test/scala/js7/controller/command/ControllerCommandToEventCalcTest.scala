package js7.controller.command

import com.typesafe.config.ConfigFactory
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.test.OurTestSuite
import js7.base.time.TimestampForTests.ts
import js7.controller.command.ControllerCommandToEventCalcTest.*
import js7.data.board.BoardPath
import js7.data.controller.ControllerCommand.DeleteNotice
import js7.data.controller.ControllerState
import js7.data.event.TimeCtx
import js7.data.plan.PlanId

final class ControllerCommandToEventCalcTest extends OurTestSuite:

  "DeleteNoticeConverter" in:
    val eventCalc = ControllerCommandToEventCalc(ConfigFactory.empty).commandToEventCalc:
      DeleteNotice(PlanId.Global / BoardPath("BOARD") / "NOTICE")
    val checked = eventCalc.calculate(ControllerState.empty, ctx)
    assert(checked == Left(UnknownKeyProblem("BoardPath", BoardPath("BOARD"))))


object ControllerCommandToEventCalcTest:
  private val ctx = TimeCtx(ts"2025-03-12T00:00:00Z")
