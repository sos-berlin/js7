package com.sos.jobscheduler.taskserver.spoolerapi

import com.sos.jobscheduler.minicom.idispatch.IDispatch
import com.sos.jobscheduler.taskserver.data.SchedulerLogger

/**
 * @author Joacim Zschimmer
 */
trait SpoolerLog extends IDispatch with SchedulerLogger
