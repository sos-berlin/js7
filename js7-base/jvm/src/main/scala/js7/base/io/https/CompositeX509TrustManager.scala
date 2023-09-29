package js7.base.io.https

import java.security.cert.{CertificateException, X509Certificate}
import javax.net.ssl.X509TrustManager
import js7.base.io.https.CompositeX509TrustManager.*
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax.*
import scala.util.{Failure, Success}

/**
  * Represents an ordered list of `X509TrustManager`s with additive trust. If any one of the
  * composed managers trusts a certificate chain, then it is trusted by the composite manager.
  *
  * This is necessary because of the fine-print on `SSLContext#init`:
  * Only the first instance of a particular key and/or trust manager implementation type in the
  * array is used. (For example, only the first javax.net.ssl.X509KeyManager in the array will be used.)
  *
  * Ported to Scala - Joacim Zschimmer
  *
  * @author codyaray (original author)
  * @since 4/22/2013
  * @see https://stackoverflow.com/questions/1793979/registering-multiple-keystores-in-jvm
  */
final class CompositeX509TrustManager private(val trustManagers: Seq[X509TrustManager])
extends X509TrustManager:
  def checkClientTrusted(chain: Array[X509Certificate], authType: String) =
    tryTrustManagers(
      _.checkClientTrusted(chain, authType))

  def checkServerTrusted(chain: Array[X509Certificate], authType: String) =
    tryTrustManagers(
      _.checkServerTrusted(chain, authType))

  private def tryTrustManagers[A](op: X509TrustManager => A): A =
    val tries = trustManagers
      .to(LazyList)
      .map(trustManager =>
        try Success(op(trustManager))
        catch {
          case e: CertificateException => Failure(e)
        })

    tries.collectFirst { case Success(a) => a }
      .getOrElse:
        for t <- tries.map(_.failed.get) do logger.debug(t.toStringWithCauses)
        throw new CertificateException("None of the TrustManagers trust this certificate chain")

  def getAcceptedIssuers: Array[X509Certificate] =
    trustManagers.view.flatMap(_.getAcceptedIssuers).toArray

object CompositeX509TrustManager:
  private val logger = Logger[this.type]

  def apply(trustManagers: Seq[X509TrustManager]): X509TrustManager =
    trustManagers match
      case Seq(single) => single
      case o => new CompositeX509TrustManager(o)
