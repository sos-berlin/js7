package js7.base.log

import io.circe.Decoder
import java.security.SecureRandom
import js7.base.circeutils.CirceUtils._
import js7.base.generic.GenericString
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.Tests.isTest

/** Correlation ID. */
sealed trait CorrelId extends GenericString
{
  def fixedWidthString: String

  def or(correlId: CorrelId): CorrelId =
    if (isEmpty) correlId else this

  def toOption: Option[CorrelId] =
    if (isEmpty) None else Some(this)

  override def toString =
    if (isEmpty) "" else string

  /** ASCII encoding is required for HTTP header values. */
  def toAscii: String
}

object CorrelId extends GenericString.Checked_[CorrelId]
{
  private[log] val longByteCount = 6
  private[log] val width = (longByteCount + 2) / 3 * 4  // Base64 length
  private[log] val bitMask = (1L << (longByteCount * 8)) - 1
  private[log] val emptyString = " " * width

  private val random = new SecureRandom
  private var generateCount = 0L
  private var asStringCount = 0L

  val empty: CorrelId = EmptyCorrelId

  private val maybeEnabled0 =
    sys.props.get("js7.log.correlId") match {
      case Some("" | "true") => Some(true)
      case Some("false") => Some(false)
      case _ => isTest ? true
    }

  private val isEnabled0 = couldBeEnabled //maybeEnabled0 getOrElse false

  // Maybe some percent performance gain when not being called?
  @inline private[log] def onCorrelIdLogged(): Unit =
    {} //  isEnabled0 = true

  def couldBeEnabled = maybeEnabled0 getOrElse true

  def isEnabled = isEnabled0

  def apply(number: Long): CorrelId =
    LongCorrelId(number)

  def generate(): CorrelId =
    if (!isEnabled)
      CorrelId.empty
    else {
      generateCount += 1
      LongCorrelId(random.nextLong & bitMask)
    }

  def logStatistics(): Unit =
    scribe.debug(statistics)

  def statistics: String =
    s"$generateCount CorrelIds generated, $asStringCount× string"

  protected def unchecked(string: String): CorrelId =
    throw new NotImplementedError("CorrelId.unchecked is not implemented")

  override def checked(string: String): Checked[CorrelId] =
    if (string.isEmpty)
      Right(EmptyCorrelId)
    else
      LongCorrelId.checked(string)

  private[log] sealed case class LongCorrelId(long: Long) extends CorrelId {
    import LongCorrelId._

    assert((long & ~bitMask) == 0)

    override def isEmpty = false

    def fixedWidthString = string

    lazy val string = {
      asStringCount += 1
      LongCorrelId.toBase64(long)
    }

    def toAscii: String =
      string.replace(code62Replacement, code62Original)
  }
  private[log] object LongCorrelId {
    /** Replacement for Base64 '-' character.
     * For usage in a HTTP header, Non-ASCII must be replaced by ASCII code .
     */
    val code62Replacement = 'ñ'
    val code62Original = '-'

    // Similar to the RFC 4648 "URL and Filename safe" Base 64 Alphabet" but replaces "-".
    // Only word characters are used to make a CorrelId selectable with a single mouse click.
    // The result can easily be used with the grep command, no quotes are required.
    private val toBase64_ = Array[Char](xs =
      'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
      'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
      'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
      'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', code62Replacement, '_')

    private val fromBase64_ = toBase64_.view.zipWithIndex.map(_.swap)
      .scanLeft(Vector.fill(256)(-1))((v, iToChar) => v.updated(iToChar._2, iToChar._1))
      .last.toArray
    fromBase64_(code62Original) = 62

    private[log] def toBase64(long: Long): String = {
      val array = new Array[Char](width)
      var i = width - 1
      var a = long
      while (i >= 0) {
        array(i) = toBase64_((a & 0x3f).toInt)
        a >>>= 6
        i -= 1
      }
      new String(array)
    }

    private[log] def fromBase64(string: String): Checked[Long] =
      checked(string).map(_.long)

    private val invalidCorrelId = Left(Problem.pure("Invalid CorrelId"))

    private[CorrelId] def checked(string: String): Checked[LongCorrelId] = {
      var long = 0L
      var i =0
      val n = string.length
      while (i < n) {
        val c = string(i)
        if (c < 0 || c >= fromBase64_.length) return invalidCorrelId
        val v = fromBase64_(c)
        if (v == -1) return invalidCorrelId
        long = (long << 6) | v
        i += 1
      }
      if ((long & ~bitMask) != 0)
        invalidCorrelId
      else
        Right(LongCorrelId(long))
    }
  }

  //private sealed case class StringCorrelId(string: String) extends CorrelId {
  //  def fixedWidthString = string
  //  override def isEmpty = false
  //}

  private object EmptyCorrelId extends CorrelId {
    val string = ""
    val fixedWidthString = " " * width
    val toAscii = ""
  }

  override implicit val jsonDecoder: Decoder[CorrelId] =
    c => c.as[String].flatMap(o => checked(o).toDecoderResult(c.history))
}
