package js7.base.auth

import js7.base.data.ByteArray
import js7.base.problem.{Checked, Problem}

final class Pem private(val typeName: String):

  private val begin = s"-----BEGIN $typeName-----"
  private val end = s"-----END $typeName-----"

  def fromPem(pem: String): Checked[ByteArray] =
    if !pem.startsWith("-----BEGIN ") then
      Left(Problem.pure("PEM format expected"))
    else
      val trimmed = pem.trim
      if !pem.startsWith(begin) || !trimmed.endsWith(end) then
        Left(Problem(s"PEM '$typeName' format expected"))
      else
        ByteArray.fromMimeBase64(trimmed
          .stripPrefix(begin)
          .stripSuffix(end))

  private val beginLine = s"$begin\r\n"
  private val endLine = s"\r\n\r\n$end\r\n"

  def toPem(byteArray: ByteArray): String =
    beginLine + byteArray.toMimeBase64 + endLine

object Pem:
  def apply(typ: String) = new Pem(typ)

  def pemTypeOf(string: String): Checked[String] =
    val head = string.takeWhile(o => o != '\r' && o != '\n')
    if !head.startsWith("-----BEGIN ") || !head.endsWith("-----") then
      Left(Problem.pure("PEM format expected"))
    else
      Right(head.stripPrefix("-----BEGIN ").stripSuffix("-----"))
