package js7.data

/**
  * @author Joacim Zschimmer
  */
package object event:
  /**
    *  Identifies [[js7.data.event.Stamped]]s taken at different times.
    *  <p>
    *    The ID encodes the timestamp as the value of milliseconds since 1970-01-01 UTC multiplied by 1000.
    *    The accuracy is one millisecond in most cases (below 1000 events/ms).
    *  <p>
    *    The factor of 1000 is chosen to have room for a counter, starting every millisecond at 0,
    *    to discriminate multiple events of the same millisecond.
    *    The counter may overflow into the millisecond part (at >= 1000 events/s),
    *    in which case the accuracy is no longer a millisecond.
    *    For the algorithm, see EventIdGenerator.
    *
    */
  type EventId = Long
