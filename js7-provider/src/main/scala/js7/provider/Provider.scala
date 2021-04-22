package js7.provider

import cats.implicits._
import com.typesafe.config.{ConfigObject, ConfigUtil}
import java.nio.file.{Path, Paths}
import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.convert.As._
import js7.base.generic.{Completed, SecretString}
import js7.base.io.file.FileUtils.syntax._
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.thread.IOExecutor
import js7.base.time.JavaTimeConverters._
import js7.base.time.ScalaTime._
import js7.base.utils.HasCloser
import js7.base.web.Uri
import js7.common.akkautils.ProvideActorSystem
import js7.common.crypt.generic.MessageSigners
import js7.common.files.{DirectoryReader, PathSeqDiff, PathSeqDiffer}
import js7.controller.client.AkkaHttpControllerApi
import js7.controller.workflow.WorkflowReader
import js7.core.item.{ItemPaths, TypedSourceReader}
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.controller.ControllerState.versionedItemJsonCodec
import js7.data.item.VersionedItems.diffVersionedItems
import js7.data.item.{ItemPath, ItemSigner, VersionId, VersionedItem, VersionedItems}
import js7.data.workflow.WorkflowPath
import js7.provider.Provider._
import js7.provider.configuration.ProviderConfiguration
import js7.proxy.ControllerApi
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
  itemSigner: ItemSigner[VersionedItem],
  protected val conf: ProviderConfiguration)
extends HasCloser with Observing with ProvideActorSystem
{
  private val userAndPassword: Option[UserAndPassword] = for {
      userName <- conf.config.optionAs[String]("js7.provider.controller.user")
      password <- conf.config.optionAs[String]("js7.provider.controller.password")
    } yield UserAndPassword(UserId(userName), SecretString(password))
  protected val httpControllerApi = new AkkaHttpControllerApi(conf.controllerUri, userAndPassword, actorSystem = actorSystem,
    config = conf.config, keyStoreRef = conf.httpsConfig.keyStoreRef, trustStoreRefs = conf.httpsConfig.trustStoreRefs)
  private val controllerApi = new ControllerApi(Seq(AkkaHttpControllerApi.admissionToApiResource(
    Admission(conf.controllerUri, userAndPassword), conf.httpsConfig)(actorSystem)))
  protected def config = conf.config

  private val firstRetryLoginDurations = conf.config.getDurationList("js7.provider.controller.login-retry-delays")
    .asScala.map(_.toFiniteDuration)
  private val typedSourceReader = new TypedSourceReader(conf.liveDirectory, readers)
  private val newVersionId = new VersionIdGenerator
  private val lastEntries = AtomicAny(Vector.empty[DirectoryReader.Entry])

  def stop: Task[Completed] =
    Task.defer {
      logger.debug("stop")
      httpControllerApi.tryLogout
        .guarantee(Task {
          httpControllerApi.close()
          close()
        })
        .memoize
    }

  private def updateAgents: Task[Completed] = {
    val agentRefs = config.getObject("js7.provider.agents").asScala
      .view
      .collect { case (name, obj: ConfigObject) =>
        AgentRef(AgentPath(name), Uri(obj.toConfig.getString("uri")))
      }
      .toSeq
    controllerApi.updateUnsignedSimpleItems(agentRefs)
      .map {
        case Left(problem) =>
          logger.error(problem.toString)
          Completed
        case Right(completed) => completed
      }
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
  def testControllerDiff: Task[Checked[VersionedItems.Diff[ItemPath, VersionedItem]]] =
    loginUntilReachable >> controllerDiff(readDirectory())

  /** Compares the directory with the Controller's repo and sends the difference.
    * Parses each file, so it may take some time for a big configuration directory. */
  private def controllerDiff(localEntries: Seq[DirectoryReader.Entry]): Task[Checked[VersionedItems.Diff[ItemPath, VersionedItem]]] =
    for {
      pair <- Task.parZip2(readLocalItem(localEntries.map(_.file)), fetchControllerItemSeq)
      (checkedLocalItemSeq, controllerItemSeq) = pair
    } yield
      checkedLocalItemSeq.map(o => itemDiff(o, controllerItemSeq))

  private def readLocalItem(files: Seq[Path]) =
    Task { typedSourceReader.readVersionedItems(files) }

  private def itemDiff(aSeq: Seq[VersionedItem], bSeq: Seq[VersionedItem]): VersionedItems.Diff[ItemPath, VersionedItem] =
    VersionedItems.Diff.fromRepoChanges(diffVersionedItems(aSeq, bSeq, ignoreVersion = true))

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
      if (httpControllerApi.hasSession)
        Task.completed
      else
        httpControllerApi.loginUntilReachable(retryLoginDurations)
          .map { completed =>
            logger.info("Logged-in at Controller")
            completed
          }
    }

  private def execute(versionId: Option[VersionId], diff: VersionedItems.Diff[ItemPath, VersionedItem]): Task[Checked[Completed]] =
    if (diff.isEmpty && versionId.isEmpty)
      Task(Checked.completed)
    else {
      val v = versionId getOrElse newVersionId()
      logUpdate(v, diff)
      controllerApi.updateRepo(itemSigner, v, diff)
    }

  private def logUpdate(versionId: VersionId, diff: VersionedItems.Diff[ItemPath, VersionedItem]): Unit = {
    logger.info(s"Version ${versionId.string}")
    for (o <- diff.deleted            .sorted) logger.info(s"Delete ${o.pretty}")
    for (o <- diff.added  .map(_.path).sorted) logger.info(s"Add ${o.pretty}")
    for (o <- diff.changed.map(_.path).sorted) logger.info(s"Change ${o.pretty}")
  }

  private def fetchControllerItemSeq: Task[Seq[VersionedItem]] =
    httpControllerApi.workflows.map(_.orThrow)

  private def readDirectory(): Vector[DirectoryReader.Entry] =
    DirectoryReader.entries(conf.liveDirectory).toVector

  private def toItemDiff(diff: PathSeqDiff): Checked[VersionedItems.Diff[ItemPath, VersionedItem]] = {
    val checkedAdded = typedSourceReader.readVersionedItems(diff.added)
    val checkedChanged = typedSourceReader.readVersionedItems(diff.changed)
    val checkedDeleted: Checked[Vector[ItemPath]] =
      diff.deleted.toVector
        .traverse(path => ItemPaths.fileToItemPath(itemPathCompanions, conf.liveDirectory, path))
    (checkedAdded, checkedChanged, checkedDeleted) mapN ((add, chg, del) => VersionedItems.Diff(add, chg, del))
  }

  private def retryLoginDurations: Iterator[FiniteDuration] =
    firstRetryLoginDurations.iterator ++ Iterator.continually(firstRetryLoginDurations.lastOption getOrElse 10.s)
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
        .map(messageSigner => new ItemSigner(messageSigner, versionedItemJsonCodec))
    }.orThrow
    Right(new Provider(itemSigner, conf))
  }

  def observe(stop: Task[Unit], conf: ProviderConfiguration)(implicit s: Scheduler, iox: IOExecutor): Checked[Observable[Completed]] =
    for (provider <- Provider(conf)) yield
      Observable.fromTask(provider.updateAgents)
        .flatMap(_ => provider.observe(stop))
        .guarantee(provider.stop.map((_: Completed) => ()))
}
