package js7.base.time

import java.time.LocalTime
import java.time.format.DateTimeParseException
import org.typelevel.literally.Literally

object JavaTimeLiterals:

  extension (inline ctx: StringContext)
    /** Compile-time checked LocalTime. */
    inline def localTime(inline args: Any*): LocalTime =
      ${ LocalTimeLiteral('ctx, 'args) }

  object LocalTimeLiteral extends Literally[LocalTime]:
    def validate(string: String)(using Quotes) =
      try
        LocalTime.parse(string) // compile-time check
        Right:
          '{ LocalTime.parse(${Expr(string)}) }
      catch case t: DateTimeParseException => 
        Left(t.toString)
