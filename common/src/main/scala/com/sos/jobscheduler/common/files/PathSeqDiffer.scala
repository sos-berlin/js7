package com.sos.jobscheduler.common.files

import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.common.files.DirectoryReader.Entry
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
object PathSeqDiffer
{
  def diff(newEntries: Seq[Entry], oldEntries: Seq[Entry]): PathSeqDiff = {
    val newMap = newEntries toKeyedMap (_.path)
    val newSet = newMap.keySet
    val oldMap = oldEntries toKeyedMap (_.path)
    val oldSet = oldMap.keySet

    PathSeqDiff(
      added = (newSet -- oldSet).toVector,
      deleted = (oldSet -- newSet).toVector,
      changed = (newSet intersect oldSet).toVector filter (p ⇒ newMap(p) isTouched oldMap(p).attributes))
  }
}
