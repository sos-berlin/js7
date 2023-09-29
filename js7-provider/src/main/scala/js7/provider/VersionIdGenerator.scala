package js7.provider

import js7.data.item.VersionId
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class VersionIdGenerator:
  private val usedVersions = mutable.Set[VersionId]()  // TODO Grows endless. We need only the values of the last second (see VersionId.generate)

  def apply(): VersionId =
    val v = VersionId.generate(isKnown = usedVersions)
    usedVersions += v
    v
