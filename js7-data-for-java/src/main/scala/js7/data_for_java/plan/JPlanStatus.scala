package js7.data_for_java.plan

import js7.base.time.JavaTimestamp.specific.RichJavaTimestampCompanion
import js7.base.time.Timestamp
import js7.data.plan.PlanStatus

object JPlanStatus:

  val Open: PlanStatus = PlanStatus.Open

  val Closed: PlanStatus = PlanStatus.Closed

  def Finished(at: java.util.Date): PlanStatus =
    PlanStatus.Finished(Timestamp.fromJavaUtilDate(at))

  val Deleted: PlanStatus = PlanStatus.Deleted
