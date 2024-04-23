package js7.base.log

import java.util.Locale
import js7.base.convert.As
import js7.base.utils.Ordinal

/**
  * @author Joacim Zschimmer
  */
enum LogLevel(private val name: String):
  case Trace extends LogLevel("trace")
  case Debug extends LogLevel("debug")
  case Info extends LogLevel("info")
  case Warn extends LogLevel("warn")
  case Error extends LogLevel("error")
  case None extends LogLevel("none")


object LogLevel:
  given Ordering[LogLevel] = Ordering.by(_.ordinal)

  /** The usual Ordinal invariants are not valid here:
   * <ul>
   * <li>pred(succ(level)) == level, not for LogLevel.Error
   * <li>succ(pred(level)) == level, not for LogLevel.None
   * </ul>
   */
  given NoneIsLowestOrdinal: Ordinal[LogLevel] = new Ordinal[LogLevel]:
    def succ(level: LogLevel) =
      level match
        case None => Trace
        case Error => Error
        case _ => LogLevel.fromOrdinal(level.ordinal + 1)

    def pred(level: LogLevel) =
      level match
        case None => None
        case Trace => None
        case _ => LogLevel.fromOrdinal(level.ordinal - 1)

    override def isSuccessorOf(a: LogLevel, b: LogLevel) =
      a.ordinal == b.ordinal + 1


  /** The usual Ordinal invariants are not valid here:
   * <ul>
   * <li>pred(succ(level)) == level, not for LogLevel.None
   * <li>succ(pred(level)) == level, not for LogLevel.Trace
   * </ul>
   */
  given NoneIsHighestOrdinal: Ordinal[LogLevel] = new Ordinal[LogLevel]:
    def succ(level: LogLevel) =
      level match
        case None => None
        case _ => LogLevel.fromOrdinal(level.ordinal + 1)

    def pred(level: LogLevel) =
      level match
        case Trace => Trace
        case _ => LogLevel.fromOrdinal(level.ordinal - 1)

    override def isSuccessorOf(a: LogLevel, b: LogLevel) =
      a.ordinal == b.ordinal + 1


  def apply(name: String): LogLevel =
    try LogLevel.valueOf(name.toLowerCase(Locale.ROOT).capitalize)
    catch case _: IllegalArgumentException =>
      throw new IllegalArgumentException(s"Invalid LogLevel: $name")

  implicit val StringAsLogLevel: As[String, LogLevel] =
    As[String, LogLevel](LogLevel.apply)
