package js7.common.akkahttp.web.data

import akka.http.scaladsl.model.Uri as AkkaUri
import cats.syntax.either.*
import cats.syntax.show.*
import java.net.{InetAddress, InetSocketAddress}
import java.nio.file.Path
import js7.base.io.https.{KeyStoreRef, StoreRef, TrustStoreRef}
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.CatsUtils.*
import js7.base.web.Uri
import js7.common.http.AkkaHttpUtils.RichAkkaAsUri
import js7.common.internet.IP.inetSocketAddressShow

/**
  * @author Joacim Zschimmer
  */
sealed trait WebServerBinding:
  def address: InetSocketAddress
  def scheme: WebServerBinding.Scheme

  /** Files containing key and trust certificates (for HTTPS). */
  def requiredFiles: Seq[Path]

  def toWebServerPort: WebServerPort

  override def toString = s"$scheme://${address.show}"

object WebServerBinding:
  def http(port: Int): Http =
    WebServerBinding.Http(new InetSocketAddress("0.0.0.0", port))

  sealed trait Scheme:
    def name: String
    // Workaround for Scala 3, which does not allow to override toString in subclasses (?)
    override def toString = name

  final case class Http(address: InetSocketAddress)
  extends WebServerBinding:
    def scheme = Http

    def toWebServerPort = WebServerPort.Http(address)

    def requiredFiles = Nil
  object Http extends Scheme:
    val name = "http"
    // Scala 3: "Double definition"??? override def toString = "http"

  final case class Https(
    address: InetSocketAddress,
    keyStoreRef: KeyStoreRef,
    trustStoreRefs: Seq[TrustStoreRef] = Nil)
  extends WebServerBinding:
    def scheme = Https

    def toWebServerPort = WebServerPort.Https(address)

    def requiredFiles = storeRefs.flatMap(_.toFile)

    def storeRefs: Seq[StoreRef] =
      keyStoreRef +: trustStoreRefs

    //override def toString = super.toString +
    //  s" ($keyStoreRef, " + (trustStoreRefs.map(_.toString).mkString(", ")) + ")"
  object Https extends Scheme:
    val name = "https"
    // Scala 3: "Double definition"??? override def toString = "https"

  trait HasLocalUris:
    protected def webServerPorts: Seq[WebServerPort]

    final lazy val localHttpUri: Checked[Uri] = locallyUsableUri(WebServerBinding.Http)
    final lazy val localHttpsUri: Checked[Uri] = locallyUsableUri(WebServerBinding.Https)
    final def localUri: Uri = (localHttpUri.toValidated findValid localHttpsUri.toValidated).orThrow

    final def locallyUsableUri(scheme: WebServerBinding.Scheme): Checked[Uri] =
      webServerPorts.collectFirst { case o if o.scheme == scheme => toLocallyUsableUri(scheme, o.address) }
        .toChecked(Problem(s"No locally usable '$scheme' address: $webServerPorts"))

    private def toLocallyUsableUri(scheme: WebServerBinding.Scheme, address: InetSocketAddress): Uri =
      val localhost = scheme match
        case WebServerBinding.Http =>
          if Set("0.0.0.0", "127.0.0.1") contains address.getAddress.getHostAddress then
            "127.0.0.1"
          else
            address.getAddress.getHostAddress

        case WebServerBinding.Https =>
          assertThat(InetAddress.getByName("localhost").getHostAddress == "127.0.0.1")  // Check file /etc/host
          "localhost"  // To match TLS host name verification
      val host = address.getAddress.getHostAddress match
        case "0.0.0.0" | "127.0.0.1" => localhost
        case o => o
      val port = address.getPort
      AkkaUri(scheme.name, AkkaUri.Authority(AkkaUri.Host(host), port)).asUri
