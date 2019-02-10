package com.sos.jobscheduler.provider

import com.sos.jobscheduler.data.filebased.VersionId
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class VersionIdGenerator
{
  private val usedVersions = mutable.Set[VersionId]()  // TODO Grows endless. We need only the values of the last second (see VersionId.generate)

  def apply(): VersionId = {
    val v = VersionId.generate(isKnown = usedVersions)
    usedVersions += v
    v
  }
}
