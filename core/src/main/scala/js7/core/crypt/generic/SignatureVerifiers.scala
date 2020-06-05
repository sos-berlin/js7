package js7.core.crypt.generic

import js7.base.crypt.SignatureVerifier
import js7.base.crypt.silly.SillySignatureVerifier
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections._
import js7.base.utils.Collections.implicits._
import js7.core.crypt.pgp.PgpSignatureVerifier

/**
  * @author Joacim Zschimmer
  */
object SignatureVerifiers
{
  private val signatureVerifiers: Seq[SignatureVerifier.Companion] = Vector(
    PgpSignatureVerifier,
    SillySignatureVerifier)

  val typeToSignatureVerifierCompanion: Map[String, Checked[SignatureVerifier.Companion]] =
    signatureVerifiers toKeyedMap (_.typeName) toChecked (typeName => Problem(s"Unknown signature provider: $typeName"))
}
