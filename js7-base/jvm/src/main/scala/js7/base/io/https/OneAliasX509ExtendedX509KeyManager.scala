package js7.base.io.https

import java.security.Principal
import javax.net.ssl.{SSLEngine, X509ExtendedKeyManager}

final class OneAliasX509ExtendedX509KeyManager(
  protected val keyManager: X509ExtendedKeyManager,
  protected val alias: String)
extends X509ExtendedKeyManager with OneAliasX509KeyManager:
  override def chooseEngineClientAlias(keyType: Array[String], issuers: Array[Principal],
    engine: SSLEngine) =
    alias

  override def chooseEngineServerAlias(keyType: String, issuers: Array[Principal],
    engine: SSLEngine) =
    alias
