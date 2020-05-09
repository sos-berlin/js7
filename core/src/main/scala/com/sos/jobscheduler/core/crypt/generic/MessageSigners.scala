package com.sos.jobscheduler.core.crypt.generic

import com.sos.jobscheduler.base.crypt.MessageSigner
import com.sos.jobscheduler.base.crypt.silly.SillySigner
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Collections._
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.core.crypt.pgp.PgpSigner

/**
  * @author Joacim Zschimmer
  */
object MessageSigners
{
  private val signatureVerifiers: Seq[MessageSigner.Companion] = Vector(
    PgpSigner,
    SillySigner)

  val typeToMessageSignersCompanion: Map[String, Checked[MessageSigner.Companion]] =
    signatureVerifiers toKeyedMap (_.typeName) toChecked (typeName => Problem(s"Unknown signature provider: $typeName"))
}
