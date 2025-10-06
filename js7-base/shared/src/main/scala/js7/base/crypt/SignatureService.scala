package js7.base.crypt

trait SignatureService:
  def verifierProvider: SignatureVerifier.Provider

  def maybeSignerCompanion: Option[DocumentSigner.Companion]
