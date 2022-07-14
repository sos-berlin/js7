package js7.common.crypt.generic

import js7.base.crypt.SignatureVerifier
import js7.base.crypt.silly.SillySignatureVerifier
import js7.base.crypt.x509.X509SignatureVerifier
import js7.base.problem.Checked
import js7.base.utils.Collections.*
import js7.base.utils.Collections.implicits.*
import js7.common.crypt.pgp.PgpSignatureVerifier
import js7.data.Problems.UnknownSignatureTypeProblem

/**
  * @author Joacim Zschimmer
  */
object SignatureVerifiers
{
  private val verifiers: Seq[SignatureVerifier.Companion] = Vector(
    PgpSignatureVerifier,
    X509SignatureVerifier,
    SillySignatureVerifier)

  val typeToSignatureVerifierCompanion: Map[String, Checked[SignatureVerifier.Companion]] =
    verifiers.toKeyedMap(_.typeName)
      .toChecked(UnknownSignatureTypeProblem.apply)
}
