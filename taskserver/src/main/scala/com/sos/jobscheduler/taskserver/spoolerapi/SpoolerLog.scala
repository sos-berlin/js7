package com.sos.jobscheduler.taskserver.spoolerapi

import com.sos.jobscheduler.data.log.SchedulerLogger
import com.sos.jobscheduler.minicom.idispatch.IDispatch

/**
 * @author Joacim Zschimmer
 */
trait SpoolerLog extends IDispatch with SchedulerLogger
