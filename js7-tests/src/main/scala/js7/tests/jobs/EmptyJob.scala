package js7.tests.jobs

import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax._
import js7.data.order.Outcome
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.tests.jobs.EmptyJob.logger
import monix.eval.Task

class EmptyJob(outcome: Outcome.Completed)
extends InternalJob:
  // We need an empty constructor for reflection
  def this() = this(Outcome.succeeded)

  final def toOrderProcess(step: Step) =
    OrderProcess(Task {
      logger.debug(s"${getClass.simpleScalaName} ${step.order.id}")
      outcome
    })


object EmptyJob extends InternalJob.Companion[EmptyJob]:
  private val logger = Logger[this.type]
