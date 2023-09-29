package js7.tests.https

final class SelectAliasHttpsTest extends ControllerHttpsStandardTests:
  override protected def useCluster = false
  override protected def agentHttpsMutual = true
  override protected def provideAgentClientCertificate = true
  override protected def controllerHttpsMutual = true
  override protected def provideControllerClientCertificate = true

  override protected def clientKeyAlias = Some("client")

  addTestsForCredentials()
