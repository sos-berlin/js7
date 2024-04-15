package js7.provider.scheduledorder.oldruntime

import java.time.Instant

trait Schedule {
  //def firstInstant(o: Instant): Option[Instant]

  //def nextInterval(from: Instant): Option[InstantInterval]
}


object Schedule:
  val eternalInterval: InstantInterval = InstantInterval(Instant.ofEpochSecond(0), Instant.ofEpochSecond(Long.MaxValue))

  object Default extends Schedule:
    def nextInstant(o: Instant): None.type =
      None

    def nextInterval(from: Instant): Some[InstantInterval] =
      Some(eternalInterval)
