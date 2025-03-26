package js7.data.event

import cats.syntax.semigroup.*
import io.circe.{Encoder, Json}
import java.io.{BufferedReader, FileInputStream, InputStreamReader}
import java.nio.file.Path
import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
final case class JournalHeader(
  typeName: Option[String]/*since v2.6*/,
  journalId: JournalId,
  eventId: EventId,
  generation: Long,
  totalEventCount: Long,
  totalRunningTime: FiniteDuration,
  timestamp: Timestamp,
  initiallyStartedAt: Timestamp,
  version: String,
  js7Version: String,
  buildId: String):

  override def toString: String =
    s"JournalHeader($journalId, $eventId, #$generation, total=$totalEventCount, " +
      s"$timestamp, ${totalRunningTime.pretty} (${totalRunningTime.toSeconds}s), " +
      s"$initiallyStartedAt, $version, $js7Version, $buildId)"


object JournalHeader:
  val Version = "1"
  private val logger = Logger[this.type]
  private val compatibility = Map("0.42" -> "1")

  implicit val jsonCodec: TypedJsonCodec[JournalHeader] =
    intelliJuseImport(FiniteDurationJsonEncoder)
    given Encoder[Timestamp] = Timestamp.StringTimestampJsonEncoder

    TypedJsonCodec[JournalHeader](
      Subtype.named[JournalHeader](
        deriveRenamingCodec[JournalHeader](Map(
          "startedAt" -> "initiallyStartedAt"/*COMPATIBLE with 2.0*/)),
        "JS7.Journal"))

  def readJournalHeader(file: Path): JournalHeader =
    autoClosing(
      new BufferedReader(new InputStreamReader(new FileInputStream(file.toFile)))
    ): file =>
      file.readLine().parseJsonAs[JournalHeader].orThrow

  def checkedHeader[S <: BasicState[S]](
    json: Json,
    journalFileForInfo: Path,
    expectedJournalId: JournalId)
    (implicit S: BasicState.Companion[S])
  : Checked[JournalHeader] =
    checkedHeader(json, journalFileForInfo, S.name, expectedJournalId)

  def checkedHeader(
    json: Json,
    journalFileForInfo: Path,
    expectedType: String,
    expectedJournalId: JournalId)
  : Checked[JournalHeader] =
    for
      header <-
        json.as[JournalHeader].toChecked.mapProblem(problem =>
          Problem.pure(
            s"Not a valid JS7 journal file: $journalFileForInfo. Expected a JournalHeader instead of ${json.compactPrint}:"
          ) |+| problem)
      _ <- checkedHeader(header, journalFileForInfo, expectedType, Some(expectedJournalId))
    yield header

  def checkedHeader[S <: BasicState[S]](
    header: JournalHeader, journalFileForInfo: Path, expectedJournalId: Option[JournalId])
    (implicit S: BasicState.Companion[S])
  : Checked[Unit] =
    checkedHeader(header, journalFileForInfo, S.name, expectedJournalId)

  def checkedHeader(
    header: JournalHeader,
    journalFileForInfo: Path,
    expectedType: String,
    expectedJournalId: Option[JournalId])
  : Checked[Unit] =
    for
      _ <- header.typeName.fold(Checked.unit)(typeName => header.typeName.forall(_ == expectedType) !!
        JournalTypeMismatchProblem(journalFileForInfo, expectedType, typeName))
      _ <- expectedJournalId.fold(Checked.unit)(expected => (expected == header.journalId) !!
        JournalIdMismatchProblem(
          journalFileForInfo, expectedJournalId = expected, foundJournalId = header.journalId))
    yield
      if compatibility.getOrElse(header.version, header.version) != Version then
        Left(Problem(
          s"Journal file has version ${header.version} but $Version is expected. Incompatible journal file: $journalFileForInfo"))
      else
        for o <- expectedJournalId do logger.debug(
          s"JournalHeader of file '${journalFileForInfo.getFileName}' is as expected, journalId=$o")
        Right(())

  final case class JournalTypeMismatchProblem(file: Path, expected: String, typeName: String) extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "file" -> file.getFileName.toString,
      "typeName" -> typeName,
      "expected" -> expected)
  object JournalTypeMismatchProblem extends Problem.Coded.Companion

  final case class JournalIdMismatchProblem(
    file: Path,
    expectedJournalId: JournalId,
    foundJournalId: JournalId)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "file" -> file.getFileName.toString,
      "expectedJournalId" -> expectedJournalId.string,
      "foundJournalId" -> foundJournalId.string)
  object JournalIdMismatchProblem extends Problem.Coded.Companion
