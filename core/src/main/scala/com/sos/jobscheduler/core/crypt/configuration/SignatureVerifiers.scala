package com.sos.jobscheduler.core.crypt.configuration

import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.core.crypt.SignatureVerifier
import com.sos.jobscheduler.core.crypt.pgp.PgpSignatureVerifier
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
object SignatureVerifiers
{
  private val signatureVerifiers: Seq[SignatureVerifier.Companion] = PgpSignatureVerifier :: Nil

  val typeToSignatureVerifierCompanion: Map[String, SignatureVerifier.Companion] =
    signatureVerifiers toKeyedMap (_.typeName)
}
