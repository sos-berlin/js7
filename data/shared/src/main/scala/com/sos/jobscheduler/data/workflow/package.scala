package js7.data

import js7.data.filebased.FileBasedId

/**
  * @author Joacim Zschimmer
  */
package object workflow
{
  type WorkflowId = FileBasedId[WorkflowPath]
  val WorkflowId = new FileBasedId.Companion[WorkflowPath] {}
}
