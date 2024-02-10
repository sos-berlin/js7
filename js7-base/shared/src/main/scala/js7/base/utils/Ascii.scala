package js7.base.utils

/**
  * @author Joacim Zschimmer
  */
object Ascii:
  val RS: Byte = 0x1e
  val LF: Byte = 0x0a
  private val SubstituteChar = '�'

  def isAsciiDigit(o: Char): Boolean =
    o >= '0' && o <= '9'

  def byteToPrintableChar(byte: Byte): Char =
    if (byte & 0x80) != 0 then
      SubstituteChar
    else
      toPrintableChar((byte & 0xff).toChar)

  def toPrintableChar(c: Char): Char =
    if c.isControl then
      if c < '\u0020' then
        ('\u2400' + c).toChar // Control character representation
      else if c == '\u007f' then
        '\u2421' // DEL
      else
        SubstituteChar
    else
      c
