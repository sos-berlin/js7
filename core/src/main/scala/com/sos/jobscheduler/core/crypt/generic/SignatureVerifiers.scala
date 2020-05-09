package com.sos.jobscheduler.core.crypt.generic

import com.sos.jobscheduler.base.crypt.SignatureVerifier
import com.sos.jobscheduler.base.crypt.silly.SillySignatureVerifier
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Collections._
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.core.crypt.pgp.PgpSignatureVerifier

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
