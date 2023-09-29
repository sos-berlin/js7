package js7.base.parser

import cats.data.NonEmptyList
import cats.parse.Numbers.digits
import cats.parse.Parser.{anyChar, char, charIn, charWhere, charsWhile, charsWhile0, end, failWith, peek, pure, string}
import cats.parse.{Parser, Parser0}
import java.lang.Character.isUnicodeIdentifierPart
import js7.base.parser.BasicPrinter.{isIdentifierPart, isIdentifierStart}
import js7.base.parser.Parsers.syntax.*
import js7.base.problem.Checked
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import scala.reflect.ClassTag

object BasicParsers
{
  private val inlineComment: Parser[Unit] = {
    val untilStar = charsWhile0(_ != '*').with1 ~ char('*')
    (string("/*") ~
      untilStar ~
      ((!char('/')).with1 ~ untilStar).rep0 ~
      char('/')
    ).void
  }

  private val lineEndComment: Parser[Unit] =
    string("//") <* charsWhile0(_ != '\n')

  private val comment: Parser[Unit] =
    inlineComment | lineEndComment

  val isWhiteChar = " \t\r\n".toSet[Char]

  /** Optional whitespace including line ends */
  val w: Parser0[Unit] =
    (charIn(isWhiteChar).rep.void | comment).rep0.void

  /** Optional horizontal whitespace */
  val h: Parser0[Unit] =
    (charIn(" \t").rep.void | comment).rep0.void

  //val specialChar: Parser[Char] = {
  //  val set = BitSet.fromSpecific("!\"#$%&'()*+,-./:;<=>?[\\]^_`{|}~".map(_.toInt))
  //  charWhere(c => set(c.toInt))
  //}

  val int: Parser[Int] =
    (char('-').string.?.with1 ~ digits).string
      .flatMap(o => catchingParser { o.toInt })

  val bigDecimal: Parser[BigDecimal] =
    (char('-').string.?.with1 ~ digits ~ (char('.') ~ digits).?)
      .string
      .flatMap(o => catchingParser { BigDecimal(o) })

  val simpleIdentifier: Parser[String] =
    (charWhere(isIdentifierStart) ~ charsWhile0(isIdentifierPart))
      .string

  private val rawBacktickIdentifierPart: Parser[String] =
    charsWhile0(c => c != '`' && c != '\n').string
      .with1.surroundedBy(char('`'))

  private val backtickIdentifier: Parser[String] =
    rawBacktickIdentifierPart.rep
      .map(_.toList.mkString("`")/* `` -> ` */)
      .flatMap {
        case "" => failWith("Identifier in backticks (`) must not be empty")
        case o => pure(o)
      }

  val identifier: Parser[String] =
    backtickIdentifier | simpleIdentifier

  val identifierEnd: Parser0[Unit] =
    end | peek(charWhere(c => !isUnicodeIdentifierPart(c)))

  val keyword =
    (charWhere(isIdentifierStart) ~ charsWhile0(isUnicodeIdentifierPart))
      .string.orElse(failWith("Expected a keyword"))

  def keyword(name: String): Parser[Unit] =
    string(name) *> identifierEnd

  private val singleQuotedContent: Parser[String] =
    charsWhile(_ != '\'')

  private val singleQuoted1: Parser[String] =
    singleQuotedContent.string
      .with1.surroundedBy(char('\''))
      .orElse(failWith("Expected properly terminated '…'-quoted string without " +
        "non-printable characters (except \\r and \\n)"))

  private val singleQuote = char('\'')
  private val noSingleQuote = !singleQuote

  private def singleQuotedRemainder(n: Int): Parser[String] =
    ((singleQuotedContent ~
      (singleQuote.rep(min = 1, max = n - 1) ~ noSingleQuote).backtrack.?)
    ).rep.string
      .<*(string("'" * n))
      .orElse(failWith(
        s"Expected properly terminated ${"'" * n}…${"'" * n}-quoted string " +
          "without non-printable characters (except \\t, \\r and \\n)" +
          s" — or use ${"\"\""} (but not '') for the empty string"))

  private def singleQuotedN(n: Int): Parser[String] =
    (string("'" * n) ~ noSingleQuote).backtrack *>
      singleQuotedRemainder(n)

  private val singleQuotedN: Parser[String] =
    singleQuote.rep(min = 2)
      .flatMap { quotes => singleQuotedRemainder(quotes.length) }

  val singleQuoted: Parser[String] =
    (singleQuoted1.backtrack |
      singleQuotedN(2) |
      singleQuotedN(3) |
      singleQuotedN/*slow*/
    ).map(_.replace("\r\n", "\n"))

  val escapedCharInString: Parser[Char] = (
    char('\\') *>
      anyChar.flatMap {
        case '\\' => pure('\\')
        case '"' => pure('"')
        case 't' => pure('\t')
        case 'r' => pure('\r')
        case 'n' => pure('\n')
        case '$' => pure('$')
        case _ => failWith("""Expected blackslash (\) and one of the following characters: [\"trn$]""")
      }.backtrack/*correct error position*/)

  private val doubleQuotedContentPart: Parser0[String] =
    charsWhile0(ch => ch != '"' && ch != '\\' && ch != '$'/*reserved for interpolation*/ /*&& ch >= ' '*/)
      .string

  private val doubleQuotedContent: Parser0[String] =
    (doubleQuotedContentPart ~ (escapedCharInString ~ doubleQuotedContentPart).rep0)
      .map { case (a, pairs) => a + pairs.view.map(o => o._1.toString + o._2).mkString }

  private val doubleQuoted: Parser[String] =
    doubleQuotedContent.with1.surroundedBy(char('"'))
      .orElse(failWith("""Expected properly terminated "…"-quoted string"""))

  val quotedString: Parser[String] =
    doubleQuoted | singleQuoted

  def keyValues[A](namedValueParser: Parser[(String, A)]): Parser0[KeyToValue[A]] =
    commaSequence(namedValueParser)
      .flatMap(namedValues =>
        namedValues.duplicateKeys(_._1) match {
          case Some(dups) =>
            failWith("Expected unique keywords (duplicates: " + dups.keys.mkString(", ") + ")")
          case None =>
            pure(KeyToValue(namedValues.toMap))
        })

  def keyValue[A](name: String, parser: Parser[A]): Parser[(String, A)] =
    keyValueConvert(name, parser)(Checked.apply[A])

  def keyValueConvert[A, B](name: String, parser: Parser[A])(toValue: A => Checked[B])
  : Parser[(String, B)] =
    specificKeyValue(name, parser)
      .flatMap(o => checkedToParser(toValue(o)))
      .map(name -> _)

  final case class KeyToValue[A](nameToValue: Map[String, A]) {
    def getOrElse[A1 <: A](key: String, default: => A1): Parser0[A1] =
      pure(nameToValue.get(key).fold(default)(_.asInstanceOf[A1]))

    def apply[A1 <: A: ClassTag](key: String): Parser0[A1] =
      nameToValue.get(key) match {
        case None => failWith(s"Expected keyword $key=")
        case Some(o) =>
          val A1 = implicitClass[A1]
          if !A1.isAssignableFrom(o.getClass) then
            if A1.eq(classOf[Int]) && o.getClass.eq(classOf[Integer]) then
              pure(o.asInstanceOf[Integer].toInt.asInstanceOf[A1])
            else
              failWith(s"Expected keyword $key=<${implicitClass[A1].simpleScalaName}>, " +
                s"not $key=<${o.getClass.simpleScalaName}>")
          else
            pure(o.asInstanceOf[A1])
      }

    def get[A1 <: A](key: String): Parser0[Option[A1]] =
      pure(nameToValue.get(key).map(_.asInstanceOf[A1]))

    def noneOrOneOf[A1 <: A](keys: String*): Parser0[Option[(String, A1)]] =
      noneOrOneOf[A1](keys.toSet)

    def noneOrOneOf[A1 <: A](keys: Set[String]): Parser0[Option[(String, A1)]] = {
      val intersection = nameToValue.keySet & keys
      intersection.size match {
        case 0 => pure(None)
        case 1 => pure(Some(intersection.head -> nameToValue(intersection.head).asInstanceOf[A1]))
        case _ => failWith(s"Expected non-contradicting keywords: ${intersection.mkString("; ")}")
      }
    }

    def oneOf[A1 <: A](keys: String*): Parser0[(String, A1)] =
      oneOfSet[A1](keys.toSet)

    def oneOfSet[A1 <: A](keys: Set[String]): Parser0[(String, A1)] = {
      val intersection = nameToValue.keySet & keys
      intersection.size match {
        // TODO Better messages for empty key (no keyword, positional argument)
        case 0 => failWith("Expected one of keywords " + keys.map(_ + "=").mkString(", "))
        case 1 => pure(intersection.head -> nameToValue(intersection.head).asInstanceOf[A1])
        case _ => failWith(s"Expected non-contradicting keywords: ${intersection.mkString("; ")}")
      }
    }

    def oneOfOr[A1 <: A](keys: Set[String], default: A1): Parser0[A1] = {
      val intersection = nameToValue.keySet & keys
      intersection.size match {
        case 0 => pure(default)
        case 1 => pure(nameToValue(intersection.head).asInstanceOf[A1])
        case _ => failWith(s"Expected non-contradicting keywords: ${intersection.mkString("; ")}")
      }
    }
  }
  object KeyToValue {
    val empty = KeyToValue[Any](Map.empty)
  }

  def specificKeyValue[V](name: String, valueParser: Parser[V]): Parser[V] = {
    val keywordPart: Parser0[Unit] =
      if name.isEmpty then
        pure(())
      else
        keyword(name) <* (w ~ char('=') ~ w)
    keywordPart.with1 *> valueParser
  }

  def curly[A](parser: Parser0[A]): Parser[A] =
    between(char('{'), char('}'))(parser)

  def inParentheses[A](parser: Parser0[A]): Parser[A] =
    between(char('('), char(')'))(parser)

  def bracketCommaSequence[A](parser: Parser[A]): Parser[List[A]] =
    between(char('['), char(']'))(commaSequence(parser))

  def between[A](left: Parser[Any], right: Parser[Any])(parser: Parser0[A]): Parser[A] =
    ((left ~ w) ~ parser ~ (w ~ right))
      .map { case ((_, a), _) => a }

  def commaSequence[A](parser: Parser[A]): Parser0[List[A]] =
    nonEmptyCommaSequence(parser).?
      .map(_.fold(List.empty[A])(_.toList))

  def nonEmptyCommaSequence[A](parser: Parser[A]): Parser[NonEmptyList[A]] =
    (parser ~ (((w ~ char(',')).backtrack ~~*> parser).rep0))
      .map { case (head, tail) => NonEmptyList(head, tail) }

  def leftRecurse[A, O](initial: Parser[A], operator: Parser[O], operand: Parser[A])
    (operation: (A, (O, A)) => A)
  : Parser[A] =
    (initial ~ w ~ (operator ~ w ~ operand ~ w).map { case (((o, _), a), _) => (o, a) }.rep0)
      .map { case ((initial, _), more) =>
        more.view.scanLeft(initial)(operation).last
      }

  def leftRecurseParsers[A, O](initial: Parser[A], operator: Parser[O], operand: Parser[A])
    (operation: (A, (O, A)) => Parser0[A])
  : Parser[A] =
    initial.flatMap(initial =>
      (operator ~~ operand)
        .rep0
        .flatMap { tail =>
          def loop(left: A, tail: List[(O, A)]): Parser0[A] =
            tail match {
              case Nil =>
                pure(left)
              case (op, right) :: tl =>
                operation(left, (op, right)).flatMap(loop(_, tl))
            }
          loop(initial, tail)
        })

  def catchingParser[A](f: => A): Parser0[A] =
    checkedToParser(Checked.catchNonFatal(f))

  def checkedToParser[A](checked: Checked[A]): Parser0[A] =
    checked match {
      case Right(a) => Parser.pure(a)
      case Left(problem) => failWith(problem.toString)
    }
}
