package js7.common.files

import js7.base.utils.Collections.implicits.*
import js7.common.files.DirectoryReader.Entry

/**
  * @author Joacim Zschimmer
  */
object PathSeqDiffer:
  def diff(newEntries: Seq[Entry], oldEntries: Seq[Entry]): PathSeqDiff =
    val newMap = newEntries toKeyedMap (_.file)
    val newSet = newMap.keySet
    val oldMap = oldEntries toKeyedMap (_.file)
    val oldSet = oldMap.keySet

    PathSeqDiff(
      added = (newSet -- oldSet).toVector,
      deleted = (oldSet -- newSet).toVector,
      changed = (newSet intersect oldSet).toVector.filter(p => newMap(p) isTouched oldMap(p).attributes))
