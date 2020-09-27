package js7.core.crypt.x509

import js7.base.data.ByteArray
import js7.base.problem.{Checked, Problem}

final class Pem private(typ: String)
{
  private val begin = s"-----BEGIN $typ-----"
  private val end = s"-----END $typ-----"

  def fromPem(pem: String): Checked[ByteArray] =
    if (!pem.startsWith("-----BEGIN "))
      Left(Problem.pure("PEM format expected"))
    else if (!pem.startsWith(begin))
      Left(Problem(s"PEM '$typ' format expected"))
    else
      ByteArray.fromMimeBase64(pem
        .trim
        .stripPrefix(begin)
        .stripSuffix(end))

  private val beginLine = s"$begin\r\n"
  private val endLine = s"\r\n\r\n$end\r\n"

  def toPem(byteArray: ByteArray): String =
    beginLine + byteArray.toMimeBase64 + endLine
}

object Pem {
  def apply(typ: String) = new Pem(typ)
}
