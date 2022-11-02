package js7.base.crypt

trait SignatureService
{
  def verifierCompanion: SignatureVerifier.Companion

  def maybeSignerCompanion: Option[DocumentSigner.Companion]
}
