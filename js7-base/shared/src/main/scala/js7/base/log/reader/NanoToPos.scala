package js7.base.log.reader

import java.util.Arrays.binarySearch
import js7.base.time.EpochNano
import org.jetbrains.annotations.TestOnly

private final class NanoToPos(initialSize: Int = 1024):
  private var _length = 1
  private var epochNanos: Array[Long] = Array.fill(initialSize)(0L)
  private var positions: Array[Long] = Array.fill(initialSize)(0L)

  epochNanos(0) = EpochNano.Nix.toLong

  inline def isEmpty: Boolean =
    length == 0

  inline def length: Int =
    _length - 1

  @TestOnly
  private[reader] inline def internalSize: Int =
    epochNanos.length

  def lastEpochNano: EpochNano =
    if _length == 0 then EpochNano.Nix else EpochNano(epochNanos(_length - 1))

  /** Return the position corresponding to the greatest [[EpochNano]]
    * less than or equal to the given [[EpochNano]] or 0 if there is no such [[EpochNano]].
    */
  def toPos(epochNano: EpochNano): Long =
    var i = binarySearch(epochNanos, 0, _length, epochNano.toLong)
    if i < 0 then
      i = -i - 1
      positions(i - 1)
    else
      positions(i)

  /** `epochNano` must be greater than the last added [[EpochNano]].*/
  def add(epochNano: EpochNano, pos: Long) =
    synchronized:
      if _length == epochNanos.length then
        val newSize = 2 * _length max 16

        val newEpochNanos = new Array[Long](newSize)
        System.arraycopy(epochNanos, 0, newEpochNanos, 0, _length)
        epochNanos = newEpochNanos

        val newPositions = new Array[Long](newSize)
        System.arraycopy(positions, 0, newPositions, 0, _length)
        positions = newPositions
      end if

      // First add value, then add key. Then we don't need to synchronize the toPos method.
      positions(_length) = pos
      epochNanos(_length) = epochNano.toLong
      _length += 1

  def shrink(): Unit =
    if _length < epochNanos.length then
      synchronized:
        if _length < epochNanos.length then
          val newEpochNanos = new Array[Long](_length)
          System.arraycopy(epochNanos, 0, newEpochNanos, 0, _length)
          epochNanos = newEpochNanos

          val newPositions = new Array[Long](_length)
          System.arraycopy(positions, 0, newPositions, 0, _length)
          positions = newPositions

  override def toString = s"NanoToPos($length entries})"
