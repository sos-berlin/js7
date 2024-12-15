package js7.base.time

import org.typelevel.literally.Literally

object TimestampForTests:

  extension (inline ctx: StringContext)
    /** Compile-time checked Timestamp.
      */
    inline def ts(inline args: Any*): Timestamp =
      ${TimestampLiteral('ctx, 'args)}

  object TimestampLiteral extends Literally[Timestamp]:
    def validate(string: String)(using Quotes) =
      Timestamp.checked(string) match
        case Left(problem) => Left(problem.toString)
        case Right(_) => Right('{Timestamp(${Expr(string)})})
