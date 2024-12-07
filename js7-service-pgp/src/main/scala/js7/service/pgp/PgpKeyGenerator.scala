package js7.service.pgp

import java.math.BigInteger
import java.security.SecureRandom
import js7.base.crypt.SignerId
import js7.base.generic.SecretString
import js7.base.log.Logger
import org.bouncycastle.bcpg.sig.{Features, KeyFlags}
import org.bouncycastle.bcpg.{HashAlgorithmTags, PublicKeyAlgorithmTags, PublicKeyPacket, SymmetricKeyAlgorithmTags}
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.generators.RSAKeyPairGenerator
import org.bouncycastle.crypto.params.RSAKeyGenerationParameters
import org.bouncycastle.openpgp.operator.bc.{BcPBESecretKeyEncryptorBuilder, BcPGPContentSignerBuilder, BcPGPDigestCalculatorProvider, BcPGPKeyPair}
import org.bouncycastle.openpgp.{PGPKeyRingGenerator, PGPSecretKey, PGPSignature, PGPSignatureSubpacketGenerator, PGPSignatureSubpacketVector}
import org.jetbrains.annotations.TestOnly

/**
  * @author Joacim Zschimmer
  */
object PgpKeyGenerator:
  private val logger = Logger[this.type]

  @TestOnly
  def generateSecretKey(id: SignerId, password: SecretString, keySize: Int = 4096): PGPSecretKey =
    // See https://stackoverflow.com/questions/3087049/bouncy-castle-rsa-keypair-generation-using-lightweight-api
    val publicExponent = 0x10001  // Should be a Fermat number
    val certainty = 80
    val controllerSigningKeyPair = newKeyPair:
      new RSAKeyGenerationParameters(
        BigInteger.valueOf(publicExponent),
        new SecureRandom, keySize, certainty)
    val shaCalculator = new BcPGPDigestCalculatorProvider()
      .get(HashAlgorithmTags.SHA1) // "only SHA1 supported for key checksum calculations"
    new PGPKeyRingGenerator(
      PGPSignature.POSITIVE_CERTIFICATION,
      controllerSigningKeyPair,
      id.string,
      shaCalculator,
      signatureSubpackets,
      null,
      new BcPGPContentSignerBuilder(
        controllerSigningKeyPair.getPublicKey.getAlgorithm,
        HashAlgorithmTags.SHA512),
      new BcPBESecretKeyEncryptorBuilder(SymmetricKeyAlgorithmTags.AES_256, shaCalculator)
        .build(password.string.toArray)
    ).generateSecretKeyRing.getSecretKey

  private def newKeyPair(parameters: RSAKeyGenerationParameters): BcPGPKeyPair =
    new BcPGPKeyPair(
      PublicKeyPacket.VERSION_4,
      PublicKeyAlgorithmTags.RSA_GENERAL,
      newAsymmetricCipherKeyPair(parameters),
      new java.util.Date)

  private def newAsymmetricCipherKeyPair(parameters: RSAKeyGenerationParameters): AsymmetricCipherKeyPair =
    val generator = new RSAKeyPairGenerator
    generator.init(parameters)
    logger.debug(s"Generating PGP key, ${parameters.getStrength} bits ...")
    generator.generateKeyPair()

  private def signatureSubpackets: PGPSignatureSubpacketVector =
    val generator = new PGPSignatureSubpacketGenerator
    // Declare its purpose
    generator.setKeyFlags(true, KeyFlags.CERTIFY_OTHER | KeyFlags.SIGN_DATA)
    generator.setPreferredHashAlgorithms(true, Array[Int](HashAlgorithmTags.SHA512))
    generator.setPreferredSymmetricAlgorithms(true, Array[Int](SymmetricKeyAlgorithmTags.AES_256))
    // Request senders add additional checksums to the message (useful when verifying unsigned messages.)
    generator.setFeature(true, Features.FEATURE_MODIFICATION_DETECTION)
    generator.generate()
