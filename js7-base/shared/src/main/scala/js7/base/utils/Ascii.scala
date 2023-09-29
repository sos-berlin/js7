package js7.base.utils

/**
  * @author Joacim Zschimmer
  */
object Ascii:
  val RS: Byte = 0x1e
  val LF: Byte = 0x0a

  def isAsciiDigit(o: Char) = o >= '0' && o <= '9'
