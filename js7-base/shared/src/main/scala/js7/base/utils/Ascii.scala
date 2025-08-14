package js7.base.utils

/**
  * @author Joacim Zschimmer
  */
object Ascii:
  val RS: Byte = 0x1e
  val LF: Byte = 0x0a
  val DEL: Byte = 0x7f
  private val SubstituteChar = '�'

  def isAsciiDigit(o: Char): Boolean =
    o >= '0' && o <= '9'

  def byteToPrintableChar(byte: Byte): Char =
    if byte <= 0 then
      SubstituteChar
    else
      toPrintableChar((byte & 0xff).toChar)

  private val charToSubst =
    val a = (0 to 0xff).map(_.toChar).toArray[Char]
    for i <- 0 to 0x1f do a(i) = ('\u2400' + i).toChar // Control character representation
    a('\t') = '⟶'
    a('\n') = '⏎' // ⏎↲↩︎ (⮐ ist zu breit)
    a(DEL) = '\u2421'
    for i <- 0x80 to 0x9f do a(i) = SubstituteChar
    a

  def toPrintableChar(c: Char): Char =
    if c <= 0xff then
      charToSubst(c)
    else
      c
