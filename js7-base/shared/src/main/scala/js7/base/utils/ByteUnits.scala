package js7.base.utils

import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
object ByteUnits:

  /** Convert to bytes, kB, MB or GB. */
  def toKBGB(size: Long): String =
    size.abs match
      case a if a < 1000 => s"${size}bytes"
      case a if a < 1000_000 => formatNumber(size, 1000, "kB")
      case a if a < 1000_000_000 => formatNumber(size, 1000_000, "MB")
      case _ => formatNumber(size, 1000_000_000, "GB")

  def toMB(size: Long): String =
    size.abs match
      case a if a < 0 => toKBGB(size)
      case a if a == 0 => "0MB"
      case a if a < 1000 * 1000 => if size < 0 then ">-1MB" else "<1MB"
      case _ => toKBGB(size)

  /** Convert to bytes, kiB, MiB or GiB. */
  def toKiBGiB(size: Long): String =
    size.abs match
      case a if a < 1024 => s"${size}bytes"
      case a if a < 2*1024-1 => formatNumber(size, 1024, "KiB")
      case a if a < 1024*1024 => formatNumber(size, 1024, "KiB")
      case a if a < 1024*1024*1024 => formatNumber(size, 1024*1024, "MiB")
      case _ => formatNumber(size, 1024*1024*1024, "GiB")

  private[utils] def formatNumber(number: Long, divisor: Int, suffix: String): String =
    val sb = new mutable.StringBuilder(16)
    if number < 0 then sb.append('-')
    val a = number.abs
    val n = a / divisor
    if n >= 10 then
      sb.append(n)
    else
      10 * a / divisor % 10 match
        case 0 => sb.append(n)
        case m =>
          sb.append(n)
          sb.append('.')
          sb.append(m)
    sb.append(suffix)
    sb.toString
