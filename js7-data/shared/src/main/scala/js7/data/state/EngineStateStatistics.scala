package js7.data.state

import js7.base.fs2utils.Fs2Utils.StreamPure
import js7.data.event.{AnyKeyedEvent, EventCounter}

final case class EngineStateStatistics(eventCounter: EventCounter):

  def estimatedSnapshotSize: Int =
    0 //eventCounter.estimatedSnapshotSize

  def toSnapshotStream: StreamPure[EventCounter.EventCount] =
    fs2.Stream.empty //eventCounter.toSnapshotStream

  def toStringStream: StreamPure[String] =
    fs2.Stream.empty //eventCounter.toStringStream

  def applyKeyedEvent(keyedEvent: AnyKeyedEvent): EngineStateStatistics =
    this //copy(eventCounter = eventCounter.applyKeyedEvent(keyedEvent))


object EngineStateStatistics:
  type SnapshotObjectType = EventCounter.EventCount

  val empty: EngineStateStatistics =
    EngineStateStatistics(EventCounter.empty)

  final class Builder:
    //private val eventCounterBuilder = EventCounter.Builder()

    def put(eventCount: EventCounter.EventCount) =
      () //eventCounterBuilder.put(eventCount.eventName, eventCount.count)

    def result(): EngineStateStatistics =
      EngineStateStatistics.empty //(eventCounterBuilder.result())
