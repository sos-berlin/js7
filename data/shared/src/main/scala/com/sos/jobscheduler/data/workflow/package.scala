package com.sos.jobscheduler.data

import com.sos.jobscheduler.data.filebased.FileBasedId

/**
  * @author Joacim Zschimmer
  */
package object workflow
{
  type WorkflowId = FileBasedId[WorkflowPath]
  val WorkflowId = new FileBasedId.Companion[WorkflowPath] {}
}
