package js7.provider

import cats.implicits._
import com.typesafe.config.{ConfigObject, ConfigUtil}
import java.nio.file.{Path, Paths}
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.convert.As._
import js7.base.generic.{Completed, SecretString}
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.HasCloser
import js7.base.web.{HttpClient, Uri}
import js7.common.akkautils.ProvideActorSystem
import js7.common.configutils.Configs.ConvertibleConfig
import js7.common.files.{DirectoryReader, PathSeqDiff, PathSeqDiffer}
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.{IOExecutor, Logger}
import js7.common.time.JavaTimeConverters._
import js7.controller.client.AkkaHttpControllerApi
import js7.controller.data.ControllerCommand
import js7.controller.data.ControllerCommand.{ReplaceRepo, UpdateAgentRefs, UpdateRepo}
import js7.controller.data.ControllerState.versionedItemJsonCodec
import js7.controller.workflow.WorkflowReader
import js7.core.crypt.generic.MessageSigners
import js7.core.item.{ItemPaths, TypedSourceReader}
import js7.data.agent.{AgentId, AgentRef}
import js7.data.item.IntentoryItems.diffVersionedItems
import js7.data.item.{IntentoryItems, ItemPath, VersionId, VersionedItem, VersionedItemSigner}
import js7.data.workflow.WorkflowPath
import js7.provider.Provider._
import js7.provider.configuration.ProviderConfiguration
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.AtomicAny
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

// Test in js7.tests.provider.ProviderTest
/**
  * @author Joacim Zschimmer
  */
final class Provider(
  itemSigner: VersionedItemSigner[VersionedItem],
  protected val conf: ProviderConfiguration)
extends HasCloser with Observing with ProvideActorSystem
{
  private val userAndPassword: Option[UserAndPassword] = for {
      userName <- conf.config.optionAs[String]("js7.provider.controller.user")
      password <- conf.config.optionAs[String]("js7.provider.controller.password")
    } yield UserAndPassword(UserId(userName), SecretString(password))
  protected val controllerApi = new AkkaHttpControllerApi(conf.controllerUri, userAndPassword, actorSystem = actorSystem,
    config = conf.config, keyStoreRef = conf.httpsConfig.keyStoreRef, trustStoreRefs = conf.httpsConfig.trustStoreRefs)
  protected def config = conf.config

  private val firstRetryLoginDurations = conf.config.getDurationList("js7.provider.controller.login-retry-delays")
    .asScala.map(_.toFiniteDuration)
  private val typedSourceReader = new TypedSourceReader(conf.liveDirectory, readers)
  private val newVersionId = new VersionIdGenerator
  private val lastEntries = AtomicAny(Vector.empty[DirectoryReader.Entry])

  def closeTask: Task[Completed] =
    controllerApi.logout()
      .guarantee(Task(controllerApi.close()))
      .memoize

  protected val relogin: Task[Completed] =
    controllerApi.logout().onErrorHandle(_ => ()) >>
      loginUntilReachable

  private def updateAgents: Task[Completed] = {
    val agentRefs = config.getObject("js7.provider.agents").asScala
      .collect { case (name, obj: ConfigObject) =>
        AgentRef(AgentId(name), Uri(obj.toConfig.getString("uri")))
      }
      .toVector
    for {
      _ <- loginUntilReachable
     completed <- controllerApi
       .retryUntilReachable()(
         controllerApi.executeCommand(UpdateAgentRefs(agentRefs)))
       .map((_: ControllerCommand.Response) => Completed)
    } yield completed
  }

  // We don't use ReplaceRepo because it changes every existing object only because of changed signature.
  private def replaceControllerConfiguration(versionId: Option[VersionId]): Task[Checked[Completed]] =
    for {
      _ <- loginUntilReachable
      currentEntries = readDirectory()
      checkedCommand = toReplaceRepoCommand(versionId getOrElse newVersionId(), currentEntries.map(_.file))
      response <- checkedCommand
        .traverse(o => HttpClient.liftProblem(
          controllerApi.executeCommand(o).map((_: ControllerCommand.Response) => Completed)))
        .map(_.flatten)
    } yield {
      if (response.isRight) {
        lastEntries := currentEntries
      }
      response
    }

  /** Compares the directory with the Controller's repo and sends the difference.
    * Parses each file, so it may take some time for a big configuration directory. */
  def initiallyUpdateControllerConfiguration(versionId: Option[VersionId] = None): Task[Checked[Completed]] = {
    val localEntries = readDirectory()
    for {
      _ <- loginUntilReachable
      checkedDiff <- controllerDiff(localEntries)
      checkedCompleted <- checkedDiff
        .traverse(execute(versionId, _))
        .map(_.flatten)
    } yield {
      for (_ <- checkedCompleted) {
        lastEntries := localEntries
      }
      checkedCompleted
    }
  }

  @TestOnly
  def testControllerDiff: Task[Checked[IntentoryItems.Diff[ItemPath, VersionedItem]]] =
    loginUntilReachable >> controllerDiff(readDirectory())

  /** Compares the directory with the Controller's repo and sends the difference.
    * Parses each file, so it may take some time for a big configuration directory. */
  private def controllerDiff(localEntries: Seq[DirectoryReader.Entry]): Task[Checked[IntentoryItems.Diff[ItemPath, VersionedItem]]] =
    for {
      pair <- Task.parZip2(readLocalItem(localEntries.map(_.file)), fetchControllerItemSeq)
      (checkedLocalItemSeq, controllerItemSeq) = pair
    } yield
      checkedLocalItemSeq.map(o => itemDiff(o, controllerItemSeq))

  private def readLocalItem(files: Seq[Path]) =
    Task { typedSourceReader.readVersionedItems(files) }

  private def itemDiff(aSeq: Seq[VersionedItem], bSeq: Seq[VersionedItem]): IntentoryItems.Diff[ItemPath, VersionedItem] =
    IntentoryItems.Diff.fromRepoChanges(diffVersionedItems(aSeq, bSeq, ignoreVersion = true))

  def updateControllerConfiguration(versionId: Option[VersionId] = None): Task[Checked[Completed]] =
    for {
      _ <- loginUntilReachable
      last = lastEntries.get()
      currentEntries = readDirectory()
      checkedCompleted <- toItemDiff(PathSeqDiffer.diff(currentEntries, last))
        .traverse(
          execute(versionId, _))
        .map(_.flatten)
    } yield
      checkedCompleted.flatMap(completed =>
        if (!lastEntries.compareAndSet(last, currentEntries)) {
          val problem = Problem.pure("Provider has been concurrently used")
          logger.debug(problem.toString)
          Left(problem)
        } else
          Right(completed))

  protected lazy val loginUntilReachable: Task[Completed] =
    Task.defer {
      if (controllerApi.hasSession)
        Task.pure(Completed)
      else
        controllerApi.loginUntilReachable(retryLoginDurations)
          .map { completed =>
            logger.info("Logged-in at Controller")
            completed
          }
    }

  private def execute(versionId: Option[VersionId], diff: IntentoryItems.Diff[ItemPath, VersionedItem]): Task[Checked[Completed.type]] =
    if (diff.isEmpty && versionId.isEmpty)
      Task(Checked.completed)
    else {
      val v = versionId getOrElse newVersionId()
      logUpdate(v, diff)
      HttpClient.liftProblem(
        controllerApi.executeCommand(toUpdateRepo(v, diff))
          .map((_: ControllerCommand.Response) => Completed))
    }

  private def logUpdate(versionId: VersionId, diff: IntentoryItems.Diff[ItemPath, VersionedItem]): Unit = {
    logger.info(s"Version ${versionId.string}")
    for (o <- diff.deleted            .sorted) logger.info(s"Delete ${o.pretty}")
    for (o <- diff.added  .map(_.path).sorted) logger.info(s"Add ${o.pretty}")
    for (o <- diff.updated.map(_.path).sorted) logger.info(s"Update ${o.pretty}")
  }

  private def toUpdateRepo(versionId: VersionId, diff: IntentoryItems.Diff[ItemPath, VersionedItem]) =
    UpdateRepo(
      versionId,
      change = (diff.added ++ diff.updated).map(_ withVersion versionId) map itemSigner.sign,
      delete = diff.deleted)

  private def fetchControllerItemSeq: Task[Seq[VersionedItem]] =
    controllerApi.workflows.map(_.orThrow)

  private def readDirectory(): Vector[DirectoryReader.Entry] =
    DirectoryReader.entries(conf.liveDirectory).toVector

  private def toItemDiff(diff: PathSeqDiff): Checked[IntentoryItems.Diff[ItemPath, VersionedItem]] = {
    val checkedAdded = typedSourceReader.readVersionedItems(diff.added)
    val checkedChanged = typedSourceReader.readVersionedItems(diff.changed)
    val checkedDeleted: Checked[Vector[ItemPath]] =
      diff.deleted.toVector
        .traverse(path => ItemPaths.fileToItemPath(itemPathCompanions, conf.liveDirectory, path))
    (checkedAdded, checkedChanged, checkedDeleted) mapN ((add, chg, del) => IntentoryItems.Diff(add, chg, del))
  }

  private def toReplaceRepoCommand(versionId: VersionId, files: Seq[Path]): Checked[ReplaceRepo] =
    typedSourceReader.readVersionedItems(files)
      .map(items => ReplaceRepo(versionId, items.map(x => itemSigner.sign(x withVersion versionId))))

  private def retryLoginDurations: Iterator[FiniteDuration] =
    firstRetryLoginDurations.iterator ++ Iterator.continually(firstRetryLoginDurations.lastOption getOrElse 10.seconds)
}

object Provider
{
  private val itemPathCompanions = Set(WorkflowPath)
  private val logger = Logger(getClass)
  private val readers = WorkflowReader :: Nil

  def apply(conf: ProviderConfiguration): Checked[Provider] = {
    val itemSigner = {
      val typeName = conf.config.getString("js7.provider.sign-with")
      val configPath = "js7.provider.private-signature-keys." + ConfigUtil.quoteString(typeName)
      val keyFile = Paths.get(conf.config.getString(s"$configPath.key"))
      val password = SecretString(conf.config.getString(s"$configPath.password"))
      MessageSigners.typeToMessageSignersCompanion(typeName)
        .flatMap(companion => companion.checked(keyFile.byteArray, password))
        .map(messageSigner => new VersionedItemSigner(messageSigner, versionedItemJsonCodec))
    }.orThrow
    Right(new Provider(itemSigner, conf))
  }

  def observe(conf: ProviderConfiguration)(implicit s: Scheduler, iox: IOExecutor): Checked[Observable[Completed]] =
    for (provider <- Provider(conf)) yield
      Observable.fromTask(provider.updateAgents)
        .flatMap(_ =>
          provider.observe
            .guarantee(provider.closeTask.map((_: Completed) => ())))
}
