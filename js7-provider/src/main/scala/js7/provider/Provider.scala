package js7.provider

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, ResourceIO}
import cats.implicits.*
import com.typesafe.config.ConfigUtil
import fs2.Stream
import java.nio.file.{Path, Paths}
import js7.base.Problems.UnknownSignatureTypeProblem
import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.configutils.Configs.{ConvertibleConfig, RichConfig}
import js7.base.convert.As.*
import js7.base.crypt.generic.SignatureProviderRegister
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.service.{MainService, Service}
import js7.base.time.WallClock
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichEitherF, RichPartialFunction, continueWithLast}
import js7.base.utils.{Atomic, ProgramTermination}
import js7.common.files.{DirectoryReader, PathSeqDiff, PathSeqDiffer}
import js7.common.pekkohttp.web.MinimumWebServer
import js7.common.pekkoutils.Pekkos
import js7.controller.client.{HttpControllerApi, PekkoHttpControllerApi}
import js7.controller.workflow.WorkflowReader
import js7.core.item.{ItemPaths, SimpleItemReader, TypedSourceReader}
import js7.data.agent.AgentRef
import js7.data.calendar.Calendar
import js7.data.controller.ControllerState.signableItemJsonCodec
import js7.data.item.ItemOperation.AddVersion
import js7.data.item.{InventoryItem, InventoryItemDiff, InventoryItemDiff_, InventoryItemPath, ItemOperation, ItemSigner, SignableItem, UnsignedSimpleItem, VersionId, VersionedItem, VersionedItemPath}
import js7.data.plan.PlanSchema
import js7.data.subagent.SubagentItem
import js7.data.workflow.{WorkflowControl, WorkflowPath, WorkflowPathControl}
import js7.provider.Provider.*
import js7.provider.configuration.ProviderConfiguration
import js7.proxy.ControllerApi
import org.apache.pekko.actor.ActorSystem
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.*

// Test in js7.tests.provider.ProviderTest

/**
 * @author Joacim Zschimmer
 */
final class Provider(
  itemSigner: ItemSigner[SignableItem],
  protected val httpControllerApi: HttpControllerApi,
  controllerApi: ControllerApi,
  protected val conf: ProviderConfiguration)
  (using protected val IORuntime: IORuntime)
extends
  Observing, MainService, Service.StoppableByRequest:

  protected type Termination = ProgramTermination

  protected def config = conf.config

  private val firstRetryLoginDurations = conf.config
    .nonEmptyFiniteDurations("js7.provider.controller.login-retry-delays")
    .orThrow
  private val typedSourceReader = new TypedSourceReader(conf.liveDirectory, readers)
  private val newVersionId = new VersionIdGenerator
  private val lastEntries = Atomic(Vector.empty[DirectoryReader.Entry])

  val untilTerminated: IO[ProgramTermination] =
    untilStopped.as(ProgramTermination())

  protected def start =
    startService(run)

  private def run: IO[Unit] =
    if conf.testSuppressStart then
      untilStopRequested
    else
      stream.compile.drain

  //override protected def stop =
  //  IO(close()) *> super.stop

  /** Compares the directory with the Controller's repo and sends the difference.
   * Parses each file, so it may take some time for a big configuration directory. */
  def initiallyUpdateControllerConfiguration(versionId: Option[VersionId] = None)
  : IO[Checked[Unit]] =
    for
      localEntries <- readDirectory
      checkedDiff <- controllerDiff(localEntries)
      checkedCompleted <- checkedDiff
        .traverse(execute(versionId, _))
        .map(_.flatten)
    yield
      for _ <- checkedCompleted do
        lastEntries := localEntries
      checkedCompleted

  @TestOnly
  def testControllerDiff: IO[Checked[InventoryItemDiff_]] =
    for
      localEntries <- readDirectory
      diff <- controllerDiff(localEntries)
    yield diff

  /** Compares the directory with the Controller's repo and sends the difference.
   * Parses each file, so it may take some time for a big configuration directory. */
  private def controllerDiff(localEntries: Seq[DirectoryReader.Entry])
  : IO[Checked[InventoryItemDiff_]] =
    for
      (checkedLocalItemSeq, controllerItems) <- IO.both(
        readLocalItems(localEntries.map(_.file)),
        fetchControllerItems)
    yield
      checkedLocalItemSeq.map:
        InventoryItemDiff.diff(_, controllerItems, ignoreVersion = true)

  private def readLocalItems(files: Seq[Path]): IO[Checked[Seq[InventoryItem]]] =
    IO(typedSourceReader.readItems(files))

  def updateControllerConfiguration(versionId: Option[VersionId] = None): IO[Checked[Unit]] =
    for
      _ <- loginUntilReachable
      last = lastEntries.get()
      currentEntries <- readDirectory
      checked <- toItemDiff(PathSeqDiffer.diff(currentEntries, last))
        .traverse(
          execute(versionId, _))
        .map(_.flatten)
    yield
      checked.flatMap: _ =>
        if !lastEntries.compareAndSet(last, currentEntries) then
          val problem = Problem.pure("Provider has been concurrently used")
          logger.debug(problem.toString)
          Left(problem)
        else
          Right(())

  protected lazy val loginUntilReachable: IO[Unit] =
    IO.defer:
      if httpControllerApi.hasSession then
        IO.unit
      else
        httpControllerApi.loginUntilReachable(retryLoginDurations).void
          .map: x =>
            logger.info("Logged-in at Controller")
            x

  private def execute(versionId: Option[VersionId], diff: InventoryItemDiff_)
  : IO[Checked[Unit]] =
    if diff.isEmpty && versionId.isEmpty then
      IO(Checked.unit)
    else
      val v = versionId getOrElse newVersionId()
      logUpdate(v, diff)
      updateItems(itemSigner, v, diff)

  private def updateItems(
    itemSigner: ItemSigner[SignableItem],
    versionId: VersionId,
    diff: InventoryItemDiff_)
  : IO[Checked[Unit]] =
    val addVersion = Stream.iterable(
      diff.containsVersionedItem ? AddVersion(versionId))

    val addOrChange = Stream
      .iterable(diff.addedOrChanged)
      .map:
        case item: VersionedItem => item.withVersion(versionId)
        case o => o
      .map:
        case item: UnsignedSimpleItem => ItemOperation.AddOrChangeSimple(item)
        case item: SignableItem => ItemOperation.AddOrChangeSigned(itemSigner.toSignedString(item))

    val remove = Stream.iterable(diff.removed).map(ItemOperation.Remove(_))

    controllerApi.updateItems(addVersion ++ addOrChange ++ remove)
      .rightAs(())

  private def logUpdate(versionId: VersionId, diff: InventoryItemDiff_): Unit =
    logger.info(s"Version ${versionId.string}")
    for o <- diff.removed.sorted do logger.info(s"Delete $o")
    for o <- diff.addedOrChanged.map(_.path: InventoryItemPath).sorted do
      logger.info(s"AddOrChange $o")

  private def fetchControllerItems: IO[Iterable[InventoryItem]] =
    httpControllerApi
      .retryUntilReachable()(
        httpControllerApi.snapshot())
      .map:
        _.items.filter:
          case _: WorkflowPathControl => false
          case _: WorkflowControl => false
          case PlanSchema.Global => false
          case _ => true

  private def readDirectory: IO[Vector[DirectoryReader.Entry]] =
    IO(DirectoryReader.entries(conf.liveDirectory).toVector)

  private def toItemDiff(diff: PathSeqDiff): Checked[InventoryItemDiff_] =
    val checkedAddedOrChanged = typedSourceReader.readItems(diff.added ++ diff.changed)
    val checkedDeleted: Checked[Vector[VersionedItemPath]] =
      diff.deleted.toVector
        .traverse(path => ItemPaths.fileToVersionedItemPath(versionedItemPathCompanions, conf.liveDirectory, path))
    (checkedAddedOrChanged, checkedDeleted)
      .mapN((add, del) => InventoryItemDiff(add, del))

  private def retryLoginDurations: Iterator[FiniteDuration] =
    firstRetryLoginDurations.iterator.continueWithLast


object Provider:
  private val versionedItemPathCompanions = Set[VersionedItemPath.AnyCompanion](WorkflowPath)
  private val logger = Logger[this.type]
  private val readers = Seq(
    WorkflowReader,
    SimpleItemReader(using AgentRef),
    SimpleItemReader(using SubagentItem),
    SimpleItemReader(using Calendar))

  def resource(conf: ProviderConfiguration)(using IORuntime): ResourceIO[Provider] =
    for
      given ActorSystem <- Pekkos.actorSystemResource("Provider", conf.config)
      _ <- MinimumWebServer.service(conf)
      provider <- resource2(conf)
    yield
      provider

  private def resource2(conf: ProviderConfiguration)
    (using ioRuntime: IORuntime, actorSystem: ActorSystem)
  : ResourceIO[Provider] =
    val userAndPassword: Option[UserAndPassword] = for
      userName <- conf.config.optionAs[String]("js7.provider.controller.user")
      password <- conf.config.optionAs[String]("js7.provider.controller.password")
    yield UserAndPassword(UserId(userName), SecretString(password))
    for
      api <- PekkoHttpControllerApi.resource(
        Admission(conf.controllerUri, userAndPassword), conf.httpsConfig)
      controllerApi <- ControllerApi.resource(
        PekkoHttpControllerApi.admissionsToApiResource(
          Nel.one(Admission(conf.controllerUri, userAndPassword)),
          conf.httpsConfig))

      itemSigner = toItemSigner(conf).orThrow
      provider <- Service.resource(new Provider(itemSigner, api, controllerApi, conf))
    yield provider

  private def toItemSigner(conf: ProviderConfiguration): Checked[ItemSigner[SignableItem]] =
    val typeName = conf.config.getString("js7.provider.sign-with")
    val configPath = "js7.provider.private-signature-keys." + ConfigUtil.quoteString(typeName)
    val keyFile = Paths.get(conf.config.getString(s"$configPath.key"))
    val password = SecretString(conf.config.getString(s"$configPath.password"))
    SignatureProviderRegister(WallClock, conf.config).nameToDocumentSignerCompanion
      .rightOr(typeName, UnknownSignatureTypeProblem(typeName))
      .flatMap(companion => companion.checked(keyFile.byteArray, password))
      .map(messageSigner => new ItemSigner(messageSigner, signableItemJsonCodec))
