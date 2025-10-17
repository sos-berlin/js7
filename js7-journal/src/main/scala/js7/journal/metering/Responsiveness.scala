package js7.journal.metering

import cats.effect.IO
import js7.base.log.Logger
import js7.base.metering.Responsivenessmeter
import js7.base.problem.Checked.RichCheckedF
import js7.journal.Journal
import js7.journal.metering.ResponsivenessEvent.InternalResponseTime

object Responsiveness:
  private val logger = Logger[this.type]

  def onMetered(journal: Journal[?]): Responsivenessmeter.MeterCallback =
    (delay, tooLong, conf) =>
      import conf.emitEvents
      IO.whenA(emitEvents):
        // TODO Wenn persist dauert, soll es nicht die Responsiveness-Schleife blockieren
        journal.persist:
          InternalResponseTime(delay, tooLong = tooLong)
        .handleProblem: problem =>
          logger.warn(problem.toString)
        .void
