package js7.data.state

import js7.base.fs2utils.Fs2Utils.StreamPure
import js7.data.event.{AnyKeyedEvent, EventCounter}

final case class EngineStateStatistics(eventCounter: EventCounter):

  def estimatedSnapshotSize: Int =
    eventCounter.estimatedSnapshotSize

  def toSnapshotStream: StreamPure[EventCounter.EventCount] =
    eventCounter.toSnapshotStream

  def toStringStream: StreamPure[String] =
    eventCounter.toStringStream

  def applyKeyedEvent(keyedEvent: AnyKeyedEvent): EngineStateStatistics =
    copy(eventCounter = eventCounter.applyKeyedEvent(keyedEvent))


object EngineStateStatistics:
  type SnapshotObjectType = EventCounter.EventCount

  val empty = EngineStateStatistics(EventCounter.empty)

  final class Builder:
    private val eventCounterBuilder = EventCounter.Builder()

    def put(eventCount: EventCounter.EventCount) =
      eventCounterBuilder.put(eventCount.eventName, eventCount.count)

    def result(): EngineStateStatistics =
      EngineStateStatistics(eventCounterBuilder.result())

