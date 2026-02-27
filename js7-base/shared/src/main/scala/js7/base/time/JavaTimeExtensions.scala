package js7.base.time

import java.time.{Instant, ZonedDateTime}

object JavaTimeExtensions:

  extension (instant: Instant)
    def toEpochNano: EpochNano =
      EpochNano.from(instant).asInstanceOf[EpochNano]


  extension (zoneDateTime: ZonedDateTime)
    def toEpochNano: EpochNano =
      zoneDateTime.toInstant.toEpochNano
