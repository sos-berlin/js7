package js7.common.crypt.generic

import js7.base.crypt.DocumentSigner
import js7.base.crypt.silly.SillySigner
import js7.base.crypt.x509.X509Signer
import js7.base.problem.Checked
import js7.base.utils.Collections.*
import js7.base.utils.Collections.implicits.*
import js7.common.crypt.pgp.PgpSigner
import js7.data.Problems.UnknownSignatureTypeProblem

/**
  * @author Joacim Zschimmer
  */
object MessageSigners
{
  private val signers: Seq[DocumentSigner.Companion] = Vector(
    PgpSigner,
    X509Signer,
    SillySigner)

  val typeToMessageSignersCompanion: Map[String, Checked[DocumentSigner.Companion]] =
    signers.toKeyedMap(_.typeName)
      .toChecked(UnknownSignatureTypeProblem.apply)
}
