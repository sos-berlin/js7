package js7.base.utils

import java.time.Instant

object JavaExtensions:

  extension (instant: Instant)
    def toEpochNanos: Long =
      instant.getEpochSecond * 1_000_000_000L + instant.getNano
