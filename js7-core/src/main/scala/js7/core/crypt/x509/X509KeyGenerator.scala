package js7.core.crypt.x509

import java.security.{KeyPair, KeyPairGenerator, SecureRandom}

object X509KeyGenerator
{
  def generateKeyPair(keySize: Int = 4096): KeyPair = {
    val keyPairGenerator = KeyPairGenerator.getInstance("RSA")
    keyPairGenerator.initialize(keySize, SecureRandom.getInstance("SHA1PRNG"))
    keyPairGenerator.generateKeyPair()
  }
}
