package js7.base.log.reader

import java.util.Arrays.binarySearch
import js7.base.io.OpaquePos
import js7.base.log.Logger
import js7.base.log.reader.EpochNanoToPos.*
import js7.base.time.EpochNano
import js7.base.utils.Tests.isStrict
import org.jetbrains.annotations.TestOnly

/** Like a reduced Java ConcurrentSkipListMap, but with small arrays and binary search.
  *
  * For a given EpochNano returns a byte position and a
  * OpaquePos (position of a chunk in a compressed file).
  *
  * Same for a given byte position.
  *
  * Requires 24 bytes per entry, after [[shrink]] has been called.
  *
  * EpochNanoToPos needs only a fraction of memory compared with TreeMap or ConcurrentSkipListMap.
  */
private final class EpochNanoToPos(initialSize: Int = 32):

  private var _length = 1
  private var _byteCount = 0L
  private var epochNanos: Array[Long] = Array.fill(initialSize)(0L)
  private var bytePositions: Array[Long] = Array.fill(initialSize)(0L)
  // Will be allocated only if contents differs from bytePositions (opaquePos != position)
  // That means less memorry for uncompressed logs
  private var _opaquePositions: Array[Long] | Null = null

  epochNanos(0) = Long.MinValue

  inline def isEmpty: Boolean =
    length == 0

  inline def length: Int =
    _length - 1

  /** Number of index log file bytes (the size of the log file). */
  inline def byteCount: Long =
    _byteCount

  private[reader] inline def byteCount_=(inline byteCount: Long): Unit =
    _byteCount = byteCount

  @TestOnly
  private[reader] inline def internalSize: Int =
    epochNanos.length

  /** The last entry's [[EpochNano]], or `MinValue` if empty.*/
  def lastEpochNano: EpochNano =
    EpochNano(epochNanos(_length - 1))

  /** The last entry, or `(0, 0)` if empty. */
  def lastChunkPosAndOpaquePos: (Long, OpaquePos) =
    val i = _length - 1
    bytePositions(i) -> OpaquePos(opaquePosition(i))

  def posToChunkPosAndOpaquePos(position: Long): (Long, OpaquePos) =
    // No synchronization needed
    var i = binarySearch(bytePositions, 0, _length, position)
    if i < 0 then
      i = -i - 2 // not exact? then return next position
    bytePositions(i) -> OpaquePos(opaquePosition(i))

  def posToBackwardChunkPosAndOpaquePos(position: Long, skipBackwards: Int): (Long, OpaquePos) =
    // No synchronization needed
    assert(skipBackwards >= 1)
    var i = binarySearch(bytePositions, 0, _length, position)
    if i >= 0 then
      i -= skipBackwards
      if i < 0 then i = 0
    else
      i = -i - 2 // not exact? then return the previous entry
      i -= skipBackwards - 1
      if i < 0 then i = 0
    bytePositions(i) -> OpaquePos(opaquePosition(i))

  /** Return the position corresponding to the greatest [[EpochNano]]
    * less than or equal to the given [[EpochNano]], or 0 if there is no such [[EpochNano]].
    * @return (uncompressed position, opaque (compressed) position)
    */
  def epochNanoToChunkPosAndOpaquePos(epochNano: EpochNano): (Long, OpaquePos) =
    // No synchronization needed
    var i = binarySearch(epochNanos, 0, _length, epochNano.toLong)
    if i < 0 then i = -i - 2 // not exact? then return the previous position
    bytePositions(i) -> OpaquePos(opaquePosition(i))

  /** @param epochNano Timestamp of the line, must be greater than the last added epochNano
    * @param opaquePos Position in the (maybe compressed) file
    * @param position position in the uncompressed byte stream
    */
  def add(epochNano: EpochNano, opaquePos: OpaquePos, position: Long): Unit =
    // Update in a way that toOpaquePos works without synchronization
    synchronized:
      if epochNano.toLong <= epochNanos(_length - 1) then
        val msg = s"EpochNanoToPos: Added values must be strictly monotonic increasing: add(${
          epochNano.show}) <= ${lastEpochNano.show}"
        logger.warn(msg)
        if isStrict then throw IllegalArgumentException(msg)

      if _length == epochNanos.length then
        resize(2 * _length max 16)

      _opaquePositions match
        case null => if opaquePos.toLong != position then
          val a = bytePositions.clone()
          a(_length) = opaquePos.toLong
          _opaquePositions = a
        case a => a(_length) = opaquePos.toLong

      bytePositions(_length) = position
      epochNanos(_length) = epochNano.toLong
      _length += 1 // Last operation to allow concurrent access

  private def opaquePosition(i: Int): Long =
    _opaquePositions match
      case null => bytePositions(i)
      case a => a(i)

  def shrink(): Unit =
    synchronized:
      if _length < epochNanos.length then
        resize(_length)

  private def resize(newSize: Int): Unit =
    bytePositions = resizeArray(bytePositions, newSize)
    _opaquePositions match
      case a: Array[Long] => _opaquePositions = resizeArray(a, newSize)
      case null =>
    epochNanos = resizeArray(epochNanos, newSize)

  private def resizeArray(array: Array[Long], newSize: Int): Array[Long] =
    val a = new Array[Long](newSize)
    System.arraycopy(array, 0, a, 0, _length)
    a

  @TestOnly
  private[reader] inline def isUsingNoMemoryForOpaquePos =
    _opaquePositions eq null

  def memorySize: Long =
    _length * (if _opaquePositions eq null then 2 * 8 else EntrySize)

  override def toString = s"EpochNanoToPos($length entries})"


object EpochNanoToPos:
  private val logger = Logger[this.type]
  val EntrySize: Int = 3 * 8 // EpochNanoToPos has three Array[Long]
