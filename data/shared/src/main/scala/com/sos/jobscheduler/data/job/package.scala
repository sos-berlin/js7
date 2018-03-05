package com.sos.jobscheduler.data

import com.sos.jobscheduler.data.filebased.FileBasedId

package object job {
  type JobId = FileBasedId[JobPath]
  val JobId = new FileBasedId.Companion[JobPath] {}
}
