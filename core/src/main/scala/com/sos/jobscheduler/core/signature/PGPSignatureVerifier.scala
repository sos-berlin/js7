package com.sos.jobscheduler.core.signature

import cats.data.Validated.{Invalid, Valid}
import cats.effect.{Resource, SyncIO}
import cats.syntax.show._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.base.utils.SyncResource.ops._
import com.sos.jobscheduler.common.scalautil.GuavaUtils.stringToInputStream
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.problems.{PGPMessageSignedByUnknownProblem, PGPTamperedWithMessageProblem}
import com.sos.jobscheduler.core.signature.PGPCommons.{readMessage, _}
import com.sos.jobscheduler.core.signature.PGPSignatureVerifier._
import java.io.InputStream
import org.bouncycastle.openpgp.jcajce.JcaPGPObjectFactory
import org.bouncycastle.openpgp.operator.jcajce.JcaPGPContentVerifierBuilderProvider
import org.bouncycastle.openpgp.{PGPPublicKeyRingCollection, PGPSignature, PGPSignatureList, PGPUtil}
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class PGPSignatureVerifier(publicKeyRingCollection: PGPPublicKeyRingCollection)
{
  registerBouncyCastle()

  private val contentVerifierBuilderProvider = new JcaPGPContentVerifierBuilderProvider().setProvider("BC")

  //logger.debug(rawToString)

  def userIds: Seq[PGPUserId] =
    publicKeyRingCollection.asScala
      .flatMap(_.getPublicKeys.asScala)
      .filter(_.isMasterKey)
      .flatMap(_.getUserIDs.asScala)
      .map(PGPUserId.apply)
      .toVector

  /** Returns `Valid(message)` iff signature matches the message. */
  def verifyString(message: String, signature: Resource[SyncIO, InputStream]): Checked[(String, Seq[PGPUserId])] =
    verify(Resource.fromAutoCloseable(SyncIO { stringToInputStream(message) }), signature) map message.→

  /** Returns `Valid(userIds)` iff signature matches the message. */
  def verify(message: Resource[SyncIO, InputStream], signature: Resource[SyncIO, InputStream]): Checked[Seq[PGPUserId]] =
    readSignature(signature) flatMap { sig ⇒
      readMessage(message, sig.update(_, 0, _))
      if (sig.verify) Valid(signatureToUserIds(sig))
      else Invalid(PGPTamperedWithMessageProblem)
    }

  private def signatureToUserIds(signature: PGPSignature) =
    Option(publicKeyRingCollection.getPublicKey(signature.getKeyID)).toList
      .flatMap(_.getUserIDs.asScala)
      .map(PGPUserId.apply).toVector

  private def readSignature(in: Resource[SyncIO, InputStream]): Checked[PGPSignature] =
    in.useSync(in ⇒
      Checked.catchNonFatal(new JcaPGPObjectFactory(PGPUtil.getDecoderStream(in)).nextObject)
        .flatMap {
          case o: PGPSignatureList ⇒
            if (o.size != 1)
              Invalid(Problem(s"Unsupported PGP signature type: expected exactly one PGPSignature, not ${o.size}"))
            else {
              val signature = o.get(0)
              publicKeyRingCollection.getPublicKey(signature.getKeyID) match {
                case null ⇒ Invalid(PGPMessageSignedByUnknownProblem)
                case publicKey ⇒
                  signature.init(
                    contentVerifierBuilderProvider,
                    publicKey)
                  Valid(signature)
              }
            }

          case o ⇒
            logger.warn(s"Unsupported PGP signature type: ${o.getClass.getName} $o")
            Invalid(Problem("Unsupported PGP signature type"))
        })

  override def toString = s"PGPSignatureVerifier($rawToString)"

  private def rawToString = "userIds=" + userIds.mkString("'", "', '", "'") + " " + publicKeyRingCollection.show
}

object PGPSignatureVerifier
{
  val logger = Logger(getClass)

  intelliJuseImport(PGPPublicKeyShow)
}
