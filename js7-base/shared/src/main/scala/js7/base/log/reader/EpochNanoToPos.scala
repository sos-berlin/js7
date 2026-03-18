package js7.base.log.reader

import java.util.Arrays.binarySearch
import js7.base.time.EpochNano
import org.jetbrains.annotations.TestOnly

/** Like a reduced Java ConcurrentSkipListMap, but with small arrays and binary search.
  *
  * Requires 16 bytes per entry.
  * Or not more then twice as much before calling [[shrink]].
  *
  * EpochNanoToPos needs only a fraction of memory compared with TreeMap or ConcurrentSkipListMap.
  */
private final class EpochNanoToPos(initialSize: Int = 1024):

  private var _length = 1
  private var epochNanos: Array[Long] = Array.fill(initialSize)(0L)
  private var positions: Array[Long] = Array.fill(initialSize)(0L)

  epochNanos(0) = Long.MinValue

  inline def isEmpty: Boolean =
    length == 0

  inline def length: Int =
    _length - 1

  @TestOnly
  private[reader] inline def internalSize: Int =
    epochNanos.length

  /** The last entry's [[EpochNano]], or `EpochNano(Long.MinValue)` if empty.*/
  def lastEpochNano: EpochNano =
    EpochNano(epochNanos(_length - 1))

  /** The last entry, or (`EpochNano(Long.MinValue), 0)` if empty.*/
  def lastEntry: (EpochNano, Long) =
    val i = _length - 1
    EpochNano(epochNanos(i)) -> positions(i)

  /** Return the position corresponding to the greatest [[EpochNano]]
    * less than or equal to the given [[EpochNano]], or 0 if there is no such [[EpochNano]].
    */
  def toPos(epochNano: EpochNano): Long =
    // No synchronization needed
    val i = binarySearch(epochNanos, 0, _length, epochNano.toLong)
    if i < 0 then
      positions(-i - 2)
    else
      positions(i)

  /** `epochNano` must be greater than the last added [[EpochNano]].*/
  def add(epochNano: EpochNano, pos: Long) =
    // Update in a way that toPos works without synchronization
    synchronized:
      if epochNano.toLong <= epochNanos(_length - 1) then throw new IllegalArgumentException(
        s"EpochNanoToPos: Added values must be strictly monotonic increasing: add(${
          epochNano.show}) <= ${lastEpochNano.show}")
      if _length == epochNanos.length then
        resize(2 * _length max 16)
      positions(_length) = pos
      epochNanos(_length) = epochNano.toLong
      _length += 1

  def shrink(): Unit =
    if _length < epochNanos.length then
      synchronized:
        if _length < epochNanos.length then
          resize(_length)

  private def resize(newSize: Int): Unit =
    val newPositions = new Array[Long](newSize)
    val newEpochNanos = new Array[Long](newSize)
    System.arraycopy(positions, 0, newPositions, 0, _length)
    System.arraycopy(epochNanos, 0, newEpochNanos, 0, _length)
    positions = newPositions
    epochNanos = newEpochNanos

  override def toString = s"EpochNanoToPos($length entries})"
