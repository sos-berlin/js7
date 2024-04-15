package js7.tests.jobs

import cats.effect.IO
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.OrderOutcome
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.tests.jobs.EmptyJob.logger

class EmptyJob(outcome: OrderOutcome.Completed)
extends InternalJob:
  // We need an empty constructor for reflection
  def this() = this(OrderOutcome.succeeded)

  final def toOrderProcess(step: Step): OrderProcess =
    OrderProcess(IO {
      logger.debug(s"${getClass.simpleScalaName} ${step.order.id}")
      outcome
    })


object EmptyJob extends InternalJob.Companion[EmptyJob]:
  private val logger = Logger[this.type]
