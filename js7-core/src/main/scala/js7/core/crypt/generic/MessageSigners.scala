package js7.core.crypt.generic

import js7.base.crypt.MessageSigner
import js7.base.crypt.silly.SillySigner
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections._
import js7.base.utils.Collections.implicits._
import js7.core.crypt.pgp.PgpSigner

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
