package js7.provider

import cats.implicits._
import com.typesafe.config.ConfigUtil
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
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.common.akkautils.ProvideActorSystem
import js7.common.crypt.generic.MessageSigners
import js7.common.files.{DirectoryReader, PathSeqDiff, PathSeqDiffer}
import js7.controller.client.AkkaHttpControllerApi
import js7.controller.workflow.WorkflowReader
import js7.core.item.{ItemPaths, SimpleItemReader, TypedSourceReader}
import js7.data.agent.AgentRef
import js7.data.controller.ControllerState.signableItemJsonCodec
import js7.data.item.ItemOperation.AddVersion
import js7.data.item.{InventoryItem, InventoryItemDiff, InventoryItemDiff_, InventoryItemPath, ItemOperation, ItemSigner, SignableItem, UnsignedSimpleItem, VersionId, VersionedItem, VersionedItemPath}
import js7.data.subagent.SubagentItem
import js7.data.workflow.{WorkflowControl, WorkflowPath, WorkflowPathControl}
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
  itemSigner: ItemSigner[SignableItem],
  protected val conf: ProviderConfiguration)
extends HasCloser with Observing with ProvideActorSystem
{
  private val userAndPassword: Option[UserAndPassword] = for {
      userName <- conf.config.optionAs[String]("js7.provider.controller.user")
      password <- conf.config.optionAs[String]("js7.provider.controller.password")
    } yield UserAndPassword(UserId(userName), SecretString(password))

  protected val httpControllerApi = new AkkaHttpControllerApi(
    conf.controllerUri, userAndPassword,
    actorSystem = actorSystem,
    conf.config, conf.httpsConfig)

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
  def testControllerDiff: Task[Checked[InventoryItemDiff_]] =
    loginUntilReachable >> controllerDiff(readDirectory())

  /** Compares the directory with the Controller's repo and sends the difference.
    * Parses each file, so it may take some time for a big configuration directory. */
  private def controllerDiff(localEntries: Seq[DirectoryReader.Entry])
  : Task[Checked[InventoryItemDiff_]] =
    for {
      pair <- Task.parZip2(readLocalItems(localEntries.map(_.file)), fetchControllerItems)
      (checkedLocalItemSeq, controllerItems) = pair
    } yield
      checkedLocalItemSeq.map(
        InventoryItemDiff.diff(_, controllerItems, ignoreVersion = true))

  private def readLocalItems(files: Seq[Path]): Task[Checked[Seq[InventoryItem]]] =
    Task { typedSourceReader.readItems(files) }

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

  private def execute(versionId: Option[VersionId], diff: InventoryItemDiff_)
  : Task[Checked[Completed]] =
    if (diff.isEmpty && versionId.isEmpty)
      Task(Checked.completed)
    else {
      val v = versionId getOrElse newVersionId()
      logUpdate(v, diff)
      updateItems(itemSigner, v, diff)
    }

  private def updateItems(
    itemSigner: ItemSigner[SignableItem],
    versionId: VersionId,
    diff: InventoryItemDiff_)
  : Task[Checked[Completed]] = {
    val addVersion = Observable.fromIterable(
      diff.containsVersionedItem ? AddVersion(versionId))

    val addOrChange = Observable
      .fromIterable(diff.addedOrChanged)
      .map {
        case item: VersionedItem => item.withVersion(versionId)
        case o => o
      }
      .map {
        case item: UnsignedSimpleItem => ItemOperation.AddOrChangeSimple(item)
        case item: SignableItem => ItemOperation.AddOrChangeSigned(itemSigner.toSignedString(item))
      }

    val remove = Observable.fromIterable(diff.removed).map(ItemOperation.Remove(_))

    controllerApi.updateItems(addVersion ++ addOrChange ++ remove)
  }

  private def logUpdate(versionId: VersionId, diff: InventoryItemDiff_): Unit = {
    logger.info(s"Version ${versionId.string}")
    for (o <- diff.removed.sorted) logger.info(s"Delete $o")
    for (o <- diff.addedOrChanged.map(_.path: InventoryItemPath).sorted)
      logger.info(s"AddOrChange $o")
  }

  private def fetchControllerItems: Task[Iterable[InventoryItem]] =
    httpControllerApi.snapshot()
      .map(_.items.filter(o =>
        !o.isInstanceOf[WorkflowPathControl] &&
          !o.isInstanceOf[WorkflowControl]))

  private def readDirectory(): Vector[DirectoryReader.Entry] =
    DirectoryReader.entries(conf.liveDirectory).toVector

  private def toItemDiff(diff: PathSeqDiff): Checked[InventoryItemDiff_] = {
    val checkedAddedOrChanged = typedSourceReader.readItems(diff.added ++ diff.changed)
    val checkedDeleted: Checked[Vector[VersionedItemPath]] =
      diff.deleted.toVector
        .traverse(path => ItemPaths.fileToVersionedItemPath(versionedItemPathCompanions, conf.liveDirectory, path))
    (checkedAddedOrChanged, checkedDeleted)
      .mapN((add, del) => InventoryItemDiff(add, del))
  }

  private def retryLoginDurations: Iterator[FiniteDuration] =
    firstRetryLoginDurations.iterator ++ Iterator.continually(firstRetryLoginDurations.lastOption getOrElse 10.s)
}

object Provider
{
  private val versionedItemPathCompanions = Set[VersionedItemPath.AnyCompanion](WorkflowPath)
  private val logger = Logger(getClass)
  private val readers = Seq(
    WorkflowReader,
    SimpleItemReader(AgentRef),
    SimpleItemReader(SubagentItem))

  def apply(conf: ProviderConfiguration): Checked[Provider] = {
    val itemSigner = {
      val typeName = conf.config.getString("js7.provider.sign-with")
      val configPath = "js7.provider.private-signature-keys." + ConfigUtil.quoteString(typeName)
      val keyFile = Paths.get(conf.config.getString(s"$configPath.key"))
      val password = SecretString(conf.config.getString(s"$configPath.password"))
      MessageSigners.typeToMessageSignersCompanion(typeName)
        .flatMap(companion => companion.checked(keyFile.byteArray, password))
        .map(messageSigner => new ItemSigner(messageSigner, signableItemJsonCodec))
    }.orThrow
    Right(new Provider(itemSigner, conf))
  }

  def observe(stop: Task[Unit], conf: ProviderConfiguration)(implicit s: Scheduler, iox: IOExecutor): Checked[Observable[Completed]] =
    for (provider <- Provider(conf)) yield
      provider
        .observe(stop)
        .guarantee(provider.stop.map((_: Completed) => ()))
}
