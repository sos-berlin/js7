package com.sos.jobscheduler.core.problems

import com.sos.jobscheduler.base.problem.{Problem, TestCodeProblem}
import com.sos.jobscheduler.core.message.ProblemCodeMessages
import com.sos.jobscheduler.data.order.OrderId
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TestProblemTest extends FreeSpec
{
  ProblemCodeMessages.initialize()

  final case class UnknownOrderProblem(orderId: OrderId) extends Problem.Coded
  {
    def arguments = Map("orderId" → orderId.string)
  }
}
