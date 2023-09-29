package js7.base.io.https

import java.net.Socket
import java.security.Principal
import javax.net.ssl.X509KeyManager

trait OneAliasX509KeyManager
extends X509KeyManager:
  protected val keyManager: X509KeyManager
  protected val alias: String

  def getCertificateChain(alias: String) =
    if alias != this.alias then
      null
    else
      keyManager.getCertificateChain(alias)

  def getPrivateKey(alias: String) =
    if alias != this.alias then
      null
    else
      keyManager.getPrivateKey(alias)

  def getClientAliases(keyType: String, issuers: Array[Principal]) =
    Array(alias)

  def chooseClientAlias(keyType: Array[String], issuers: Array[Principal], socket: Socket) =
    alias

  def getServerAliases(keyType: String, issuers: Array[Principal]) =
    Array(alias)

  def chooseServerAlias(keyType: String, issuers: Array[Principal], socket: Socket) =
    alias
