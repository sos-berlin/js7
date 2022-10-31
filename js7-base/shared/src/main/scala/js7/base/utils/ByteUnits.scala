package js7.base.utils

/**
  * @author Joacim Zschimmer
  */
object ByteUnits
{
  /** Converts to bytes, kB, MB or GB */
  def toKBGB(size: Long): String =
    size match {
      case _ if size < 1000 => s"${size}bytes"
      case _ if size < 1000_000 => formatNumber(size, 1000) + "kB"
      case _ if size < 1000_000_000 => formatNumber(size, 1000_000) + "MB"
      case _ => formatNumber(size, 1000_000_000) + "GB"
    }

  def toMB(size: Long): String =
    size match {
      case _ if size < 0 => toKBGB(size)
      case _ if size == 0 => "0MB"
      case _ if size < 1000 * 1000 => "<1MB"
      case _ => toKBGB(size)
    }

  /** Converts to bytes, kB, MB or GB */
  def toKiBGiB(size: Long): String =
    size match {
      case _ if size < 1024 => s"${size}bytes"
      case _ if size < 2*1024-1 => formatNumber(size, 1024) + "KiB"
      case _ if size < 1024*1024 => formatNumber(size, 1024) + "KiB"
      case _ if size < 1024*1024*1024 => formatNumber(size, 1024*1024) + "MiB"
      case _ => formatNumber(size, 1024*1024*1024) + "GiB"
    }

  private[utils] def formatNumber(number: Long, divisor: Int): String = {
    val n = number / divisor
    if (n >= 10)
      n.toString
    else {
      val m = 10 * number / divisor % 10
      if (m == 0) n.toString
      else s"$n.$m"
    }
  }
}
