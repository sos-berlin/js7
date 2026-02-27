package js7.base.time

import java.time.{Instant, ZonedDateTime}

object JavaTimeExtensions:

  private val MinNanoInstant = Instant.ofEpochMilli(Long.MinValue)
  private val MaxNanoInstant = Instant.ofEpochMilli(Long.MaxValue)

  extension (instant: Instant)
    def toEpochNano: EpochNano =
      if instant.isBefore(MinNanoInstant) || instant.isAfter(MaxNanoInstant) then
        throw new IllegalArgumentException(s"Instant $instant is out of range")
      EpochNano.from(instant).asInstanceOf[EpochNano]


  extension (zoneDateTime: ZonedDateTime)
    def toEpochNano: EpochNano =
      zoneDateTime.toInstant.toEpochNano
