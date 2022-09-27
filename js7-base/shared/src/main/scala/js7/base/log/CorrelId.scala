package js7.base.log

import io.circe.Decoder
import java.security.SecureRandom
import js7.base.circeutils.CirceUtils.*
import js7.base.generic.GenericString
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.{isIntelliJIdea, isTest}
import monix.execution.Scheduler
import monix.execution.atomic.Atomic
import monix.execution.misc.Local
import monix.execution.schedulers.TracingScheduler

/** Correlation ID. */
sealed trait CorrelId extends GenericString
{
  def fixedWidthString: String

  def orNew: CorrelId =
    or(CorrelId.generate())

  def or(correlId: CorrelId): CorrelId =
    if (isEmpty) correlId else this

  def toOption: Option[CorrelId] =
    if (isEmpty) None else Some(this)

  override def toString =
    if (isEmpty) "" else string

  /** ASCII encoding is required for HTTP header values. */
  def toAscii: String

  def bind[R](body: => R)(implicit R: CanBindCorrelId[R]): R =
    R.bind(this)(body)

  /** For a synchronous non-Unit executable body only, uses `CanBindLocals.synchronous`. */
  def bindNow[R](body: => R): R =
    bind(body)(CanBindCorrelId.synchronous)

  def fold[A](whenEmpty: => A, whenNonEmpty: CorrelId => A): A =
    if (isEmpty) whenEmpty
    else whenNonEmpty(this)
}

object CorrelId extends GenericString.Checked_[CorrelId]
{
  private[log] val longByteCount = 6
  private[log] val width = (longByteCount + 2) / 3 * 4  // Base64 length
  private[log] val bitMask = (1L << (longByteCount * 8)) - 1
  val empty: CorrelId = EmptyCorrelId
  private[log] val local = Local(CorrelId.empty)

  private val nextCorrelId: NextCorrelId =
    if (isTest && isIntelliJIdea)
      new NextCorrelId {
        private val next = Atomic(LongCorrelId.checked("CorAAAAA").orThrow.long)
        def apply() = LongCorrelId(next.incrementAndGet())
      }
    else
      new NextCorrelId {
        private val random = new SecureRandom()
        def apply() = LongCorrelId(random.nextLong())
      }

  private trait NextCorrelId {
    def apply(): CorrelId
  }

  private var currentCorrelIdCount = 0L
  private var generateCount = 0L
  private var asStringCount = 0L

  private val maybeEnabled0 =
    sys.props.get("js7.log.correlId") match {
      case Some("" | "true") => Some(true)
      case Some("false") => Some(false)
      case _ => isTest ? true
    }

  def couldBeEnabled = maybeEnabled0 getOrElse true

  private val isEnabled0 = couldBeEnabled //maybeEnabled0 getOrElse false

  // Maybe some percent performance gain when not being called?
  @inline private[log] def onCorrelIdLogged(): Unit =
    {} //  isEnabled0 = true

  @inline def isEnabled = isEnabled0

  def apply(number: Long): CorrelId =
    LongCorrelId(number)

  protected def unchecked(string: String): CorrelId =
    throw new NotImplementedError("CorrelId.unchecked is not implemented")

  override def checked(string: String): Checked[CorrelId] =
    if (string.isEmpty)
      Right(EmptyCorrelId)
    else
      LongCorrelId.checked(string)

  override implicit val jsonDecoder: Decoder[CorrelId] =
    c => c.as[String].flatMap(o => checked(o).toDecoderResult(c.history))

  def generate(): CorrelId =
    if (!isEnabled)
      CorrelId.empty
    else {
      generateCount += 1
      nextCorrelId()
    }

  def current: CorrelId =
    if (!CorrelId.isEnabled)
      CorrelId.empty
    else {
      currentCorrelIdCount += 1
      local()
    }

  def isolate[R](body: LogCorrelId => R): R =
    if (!CorrelId.isEnabled)
      body(EmptyLogCorrelId)
    else {
      val previous = current
      try body(new ActiveLogCorrelId(previous))
      finally local.update(previous)
    }

  sealed trait LogCorrelId {
    def :=(correlId: CorrelId): Unit
  }

  private final class ActiveLogCorrelId(private var currCorrelId: CorrelId)
  extends LogCorrelId
  {
    def :=(correlId: CorrelId): Unit =
      if (correlId != currCorrelId) {
        currCorrelId = correlId
        local.update(correlId)
      }
  }

  private object EmptyLogCorrelId extends LogCorrelId {
    def :=(correlId: CorrelId) = {}
  }

  /** Generate a CorrelId for the body if `current` isEmpty. */
  def bind[R](body: => R)(implicit R: CanBindCorrelId[R]): R =
    R.bindNewIfNoCurrent(body)

  /** Generate a CorrelId for the synchronously executable body if `current` isEmpty. */
  def bindNow[R](body: => R): R =
    CanBindCorrelId.synchronous.bindNewIfNoCurrent(body)

  def bindNew[R](body: => R)(implicit R: CanBindCorrelId[R]): R =
    R.bind(CorrelId.generate())(body)

  def bindNewNow[R](body: => R): R =
    CanBindCorrelId.synchronous.bind(CorrelId.generate())(body)

  def enableScheduler(scheduler: Scheduler): Scheduler =
    if (!CorrelId.couldBeEnabled)
      scheduler
    else
      TracingScheduler(scheduler)

  private[log] sealed case class LongCorrelId private(long: Long) extends CorrelId {
    import LongCorrelId.*

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

    def apply(long: Long): LongCorrelId =
      new LongCorrelId(long & bitMask)

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

  def logStatisticsIfEnabled(): Unit =
    if (CorrelId.isEnabled) logStatistics()

  def logStatistics(): Unit =
    scribe.debug(statistics)

  def statistics: String =
    s"$generateCount CorrelIds generated, $asStringCount× string, " +
    s"${CanBindCorrelId.bindCorrelIdCount}× bindCorrelId, " +
      s"$currentCorrelIdCount× CorrelId.current"
}
