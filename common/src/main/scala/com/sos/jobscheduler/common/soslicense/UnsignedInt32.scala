package com.sos.scheduler.engine.common.soslicense

import com.sos.scheduler.engine.common.soslicense.UnsignedInt32._

/**
 * @author Joacim Zschimmer
 */
private[soslicense] final case class UnsignedInt32(toInt: Int) {
  def *(o: Int): UnsignedInt32 = this * UnsignedInt32(o)
  def %(o: Int): UnsignedInt32 = this % UnsignedInt32(o)
  def <<(o: Int) = truncate(toLong << o)
  def >>(o: Int) = truncate(toLong >> o)
  def &(o: Int) = truncate(toLong & o)
  def ^(o: Int) = truncate(toLong ^ o)
  def +(o: UnsignedInt32) = truncate(toLong + o.toLong)
  def -(o: UnsignedInt32) = truncate(toLong - o.toLong)
  def *(o: UnsignedInt32) = truncate(toLong * o.toLong)
  def %(o: UnsignedInt32) = truncate(toLong % o.toLong)
  def |(o: UnsignedInt32) = truncate(toLong | o.toLong)
  def toLong = toInt.toLong & 0xffffffffL
}

private[soslicense] object UnsignedInt32 {
  private def truncate(i: Long) = UnsignedInt32((i & 0xffffffffL).toInt)

  implicit class AsUnsignedInt(val delegate: Int) extends AnyVal {
    def unsigned = UnsignedInt32(delegate)
  }
}
