package js7.tests.testenv

import js7.agent.RunningAgent
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.common.configutils.Configs._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.controller.RunningController
import js7.controller.data.ControllerCommand.UpdateRepo
import js7.data.filebased.{FileBased, TypedPath, VersionId}
import monix.execution.Scheduler.Implicits.global
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
trait ControllerAgentForScalaTest extends DirectoryProviderForScalaTest {
  this: org.scalatest.Suite =>

  protected final lazy val agents: Seq[RunningAgent] = directoryProvider.startAgents() await 99.s
  protected final lazy val agent: RunningAgent = agents.head
  protected final lazy val controller: RunningController = directoryProvider.startController(
    controllerModule,
    config"""js7.web.server.auth.https-client-authentication = $controllerHttpsMutual""",
    httpPort = controllerHttpPort,
    httpsPort = controllerHttpsPort
  ) await 99.s
  protected def controllerHttpsMutual = false

  protected def waitUntilReady = true

  override def beforeAll() = {
    super.beforeAll()
    agents
    if (waitUntilReady) {
      controller.waitUntilReady()
    }
  }

  override def afterAll() = {
    controller.terminate() await 15.s
    controller.close()
    agents.map(_.terminate()) await 15.s
    for (a <- agents) a.close()
    super.afterAll()
  }

  private val usedVersionIds = mutable.Set[VersionId]()

  final def updateRepo(change: Seq[FileBased]): VersionId =
    updateRepo(change, Nil)

  final def updateRepo(
    change: Seq[FileBased],
    delete: Seq[TypedPath])
  : VersionId = {
    val versionId = VersionId.generate(usedVersionIds)
    updateRepo(versionId, change, delete)
    versionId
  }

  final def updateRepo(
    versionId: VersionId,
    change: Seq[FileBased] = Nil,
    delete: Seq[TypedPath] = Nil)
  : Unit = {
    usedVersionIds += versionId
    controller.executeCommandAsSystemUser(UpdateRepo(
      versionId,
      change map (_ withVersion versionId) map directoryProvider.fileBasedSigner.sign,
      delete)
    ).await(99.s).orThrow
  }
}
