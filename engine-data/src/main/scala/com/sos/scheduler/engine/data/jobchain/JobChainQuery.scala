package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.base.generic.IsString
import com.sos.scheduler.engine.data.folder.FolderPath

/**
  * @author Joacim Zschimmer
  */
final case class JobChainQuery(string: String) extends IsString {
  require((string startsWith "/") && !(string contains "/.."), "Invalid jobChain pattern")

  def matches(jobChainPath: JobChainPath): Boolean =
    matchesAll || (
      if (string endsWith "/") jobChainPath.string startsWith string
      else jobChainPath.string == string)

  val matchesAll = string == "/"

  /**
    * For query optimization, returns a reduces type.
    * @return `All`, `JobChainPath`, `FolderPath` or some other type.
    */
  private[engine] def reduce: Any =
    if (matchesAll) JobChainQuery.All
    else if (string endsWith "/") FolderPath(string stripSuffix "/")
    else JobChainPath(string)
}

object JobChainQuery extends IsString.Companion[JobChainQuery] {
  val All = JobChainQuery("/")

  def apply(folder: FolderPath) = new JobChainQuery((folder.string stripSuffix "/") + "/")

  def apply(jobChainPath: JobChainPath) = new JobChainQuery(jobChainPath.string)
}
