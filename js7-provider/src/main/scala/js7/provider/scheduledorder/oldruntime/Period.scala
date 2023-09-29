package js7.provider.scheduledorder.oldruntime

import java.time.{Duration, LocalTime}

/**
  * @author Joacim Zschimmer
  */
trait Period extends Ordered[Period]:

  def begin: LocalTime

  def end: ExtendedLocalTime

  def absoluteRepeatOption: Option[Duration]

  def nextLocalTime(t: LocalTime): Option[LocalTime]

  final def compare(o: Period) = begin compareTo o.begin
