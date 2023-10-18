package js7.base.crypt.x509

private[x509] final case class X509Algorithm(string: String)


object X509Algorithm:
  lazy val SHA512withRSA = X509Algorithm("SHA512withRSA")
