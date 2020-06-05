package js7.data.filebased

import cats.instances.either._
import cats.instances.vector._
import cats.syntax.traverse._
import io.circe.Decoder
import js7.base.crypt.Signed
import js7.base.crypt.donotcrypt.DoNotVerifySignatureVerifier
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Collections._
import js7.base.utils.Collections.implicits._
import js7.base.utils.Memoizer
import js7.base.utils.ScalaUtils._
import js7.data.crypt.FileBasedVerifier
import js7.data.filebased.Repo.Entry
import js7.data.filebased.RepoEvent.{FileBasedAdded, FileBasedAddedOrChanged, FileBasedChanged, FileBasedDeleted, FileBasedEvent, VersionAdded}
import org.jetbrains.annotations.TestOnly

/**
  * Representation of versioned FileBased (configuration objects).
  * @param versionSet `versionSet == versions.toSet`
  * @author Joacim Zschimmer
  */
final case class Repo private(
  versions: List[VersionId],
  versionSet: Set[VersionId],
  pathToVersionToSignedFileBased: Map[TypedPath, List[Entry]],
  fileBasedVerifier: FileBasedVerifier[FileBased])
{
  assertThat(versions.nonEmpty || pathToVersionToSignedFileBased.isEmpty)

  /** `fileBasedVerifier` is not compared - for testing only. */
  override def equals(o: Any) = o match {
    case o: Repo => versions == o.versions && pathToVersionToSignedFileBased == o.pathToVersionToSignedFileBased
    case _ => false
  }

  import Repo._

  lazy val versionId: VersionId = versions.headOption getOrElse VersionId.Anonymous

  lazy val currentVersion: Map[TypedPath, Signed[FileBased]] =
    (for {
      (path, versionToFileBased) <- pathToVersionToSignedFileBased
      fileBased <- versionToFileBased.head.maybeSignedFileBased
    } yield path -> fileBased)

  private lazy val typeToPathToCurrentFileBased: FileBased.Companion_ => Map[TypedPath, Signed[FileBased]] =
    Memoizer.nonStrict { companion: FileBased.Companion_ =>
      currentVersion collect {
        case (path, signedFileBased) if signedFileBased.value.companion == companion =>
          path -> signedFileBased
      }
    }

  /** Returns the difference to the repo as events. */
  def fileBasedToEvents(versionId: VersionId, changed: Iterable[Signed[FileBased]], deleted: Iterable[TypedPath] = Nil)
  : Checked[Seq[RepoEvent]] =
    checkVersion(versionId, changed) flatMap { changed =>
      val addedOrChanged = changed flatMap toAddedOrChanged
      addedOrChanged.checkUniqueness(_.path) map { _ =>
        val deletedEvents = deleted
          .filterNot(addedOrChanged.map(_.path).toSet)  // delete and change?
          .filter(currentVersion.keySet)  // delete unknown?
          .map(FileBasedDeleted.apply)
        Vector(VersionAdded(versionId)) ++ deletedEvents ++ addedOrChanged
      }
    }

  private def checkVersion(versionId: VersionId, signedFileBased: Iterable[Signed[FileBased]]): Checked[Vector[Signed[FileBased]]] =
    signedFileBased.toVector.traverse(o =>
      (o.value.id.versionId match {
        case `versionId` => Right(o)
        case _ => Left(ObjectVersionDoesNotMatchProblem(versionId, o.value.id))
      }): Checked[Signed[FileBased]])

  private def toAddedOrChanged(signedFileBased: Signed[FileBased]): Option[RepoEvent.FileBasedEvent] = {
    val fileBased = signedFileBased.value
    currentVersion.get(fileBased.path) match {
      //case Some(existing) if existing.value.withVersion(fileBased.id.versionId) == fileBased =>
      case Some(existing) if existing == signedFileBased =>
        None

      case Some(_) =>
        Some(FileBasedChanged(fileBased.path, signedFileBased.signedString))

      case None =>
        Some(FileBasedAdded(fileBased.path, signedFileBased.signedString))
    }
  }

  def typedCount[A <: FileBased](implicit A: FileBased.Companion[A]): Int =
    pathToVersionToSignedFileBased.values.view.flatten.count {
      case Entry(_, Some(signed)) if signed.value.companion == A => true
      case _ => false
    }

  def currentTyped[A <: FileBased](implicit A: FileBased.Companion[A]): Map[A#Path, A] =
    typeToPathToCurrentFileBased(A).view.mapValues(_.value).toMap.asInstanceOf[Map[A#Path, A]]

  def currentFileBaseds: Iterable[FileBased] =
    currentSignedFileBaseds.map(_.value)

  def currentSignedFileBaseds: Iterable[Signed[FileBased]] =
    currentVersion.values

  def applyEvents(events: Seq[RepoEvent]): Checked[Repo] = {
    var result = Checked(this)
    val iterator = events.iterator
    while (result.isRight && iterator.hasNext) {
      result = result flatMap (_.applyEvent(iterator.next()))
    }
    result
  }

  def applyEvent(event: RepoEvent): Checked[Repo] =
    event match {
      case VersionAdded(version) =>
        if (versionSet contains version)
          DuplicateVersionProblem(version)
        else
          Right(copy(
            versions = version :: versions,
            versionSet = versionSet + version))

      case event: FileBasedEvent =>
        if (versions.isEmpty)
          Problem.pure(s"Missing initial VersionAdded event for Repo")
        else
          event match {
            case event @ FileBasedAddedOrChanged(path, signedString) =>
              fileBasedVerifier.verify(signedString).map(_.fileBased).flatMap(fileBased =>
                if (path != fileBased.path)
                  Problem.pure(s"Error in FileBasedAddedOrChanged event: path=$path does not equal path=${fileBased.path}")
                else if (fileBased.path.isAnonymous)
                  Problem.pure(s"Adding an anonymous ${fileBased.companion.name} is not allowed")
                else if (fileBased.id.versionId != versionId)
                  EventVersionDoesNotMatchProblem(versionId, event)
                else
                  Right(addEntry(fileBased.path, Some(js7.base.crypt.Signed(fileBased withVersion versionId, signedString)))))

            case FileBasedDeleted(path) =>
              Right(addEntry(path, None))
          }
    }

  private def addEntry(path: TypedPath, fileBasedOption: Option[Signed[FileBased]]): Repo = {
    val version = versions.head
    copy(pathToVersionToSignedFileBased =
      pathToVersionToSignedFileBased +
        (path ->
          (Entry(version,fileBasedOption) :: (pathToVersionToSignedFileBased.getOrElse(path, Nil)))))
  }

  /** Returns the current FileBased to a Path. */
  def pathTo[A <: FileBased](path: A#Path)(implicit A: FileBased.Companion[A]): Checked[A] =
    pathToVersionToSignedFileBased
      .checked(path)
      .flatMap { entries =>
        val entry = entries.head
        entry
          .maybeSignedFileBased
          .toChecked(DeletedProblem(path ~ entry.versionId))
          .map(_.value.asInstanceOf[A])
      }

  /** Returns the FileBased to a FileBasedId. */
  def idTo[A <: FileBased](id: FileBasedId[A#Path])(implicit A: FileBased.Companion[A]): Checked[A] =
    idToSigned[A](id).map(_.value)

  /** Returns the FileBased to a FileBasedId. */
  def idToSigned[A <: FileBased](id: FileBasedId[A#Path])(implicit A: FileBased.Companion[A]): Checked[Signed[A]] =
    for {
      versionToSignedFileBased <- pathToVersionToSignedFileBased.checked(id.path)
      fileBasedOption <- versionToSignedFileBased
        .collectFirst { case Entry(id.versionId, o) => o }
        .toChecked(Problem(s"No such '$id'"))
        .orElse(
          for {
            history <- historyBefore(id.versionId)
            fb <- versionToSignedFileBased.fileBasedOption(history) toChecked Problem(s"No such '$id'")
          } yield fb
        ): Checked[Option[Signed[FileBased]]]
      signedFileBased <- fileBasedOption.toChecked(DeletedProblem(id))
    } yield signedFileBased.copy(signedFileBased.value.cast[A])

  /** Converts the Repo to an event sequence, regarding only a given type. */
  def eventsFor(is: TypedPath.AnyCompanion => Boolean): Seq[RepoEvent] =
    toEvents collect {
      case e: VersionAdded => e
      case e: FileBasedEvent if is(e.path.companion) => e
    }

  /** Converts the Repo to an event sequence. */
  private[filebased] def toEvents: Seq[RepoEvent] = {
    val versionToFileBasedOptions: Map[VersionId, Seq[Either[Signed[FileBased/*added/updated*/], TypedPath/*deleted*/]]] =
      pathToVersionToSignedFileBased.toVector.sortBy(_._1)/*for testing*/
        .flatMap { case (path, versionToFileBasedOpt) =>
          versionToFileBasedOpt
            .map(entry => entry.versionId -> entry.maybeSignedFileBased.map(Left.apply).getOrElse(Right(path)))
        }
        .groupBy(_._1).view.mapValues(_ map (_._2))
        .toMap
    versions.tails
      .map {
        case Nil =>  // Last of tails
          Vector.empty
        case version :: history =>
          Vector(VersionAdded(version)) ++
            versionToFileBasedOptions.getOrElse(version, Nil).map {
              case Left(signedFileBased) =>
                val vToF = pathToVersionToSignedFileBased(signedFileBased.value.path)
                if (vToF.fileBasedOption(history).flatten.isDefined)
                  FileBasedChanged(signedFileBased.value.path, signedFileBased.signedString)
                else
                  FileBasedAdded(signedFileBased.value.path, signedFileBased.signedString)
              case Right(path) =>
                FileBasedDeleted(path)
            }
      }
      .toVector.reverse.flatten
  }

  private[filebased] def historyBefore(versionId: VersionId): Checked[List[VersionId]] =
    versions.dropWhile(versionId.!=) match {
      case Nil => Problem(s"No such '$versionId'")
      case _ :: tail => Right(tail)
    }

  def newVersionId(): VersionId =
    VersionId.generate(isKnown = versions.contains)

  override def toString = s"Repo($versions," +  //FIXME
    pathToVersionToSignedFileBased
      .keys.toSeq.sorted
      .map(path =>
        pathToVersionToSignedFileBased(path)
          .map(entry => entry.maybeSignedFileBased.fold(s"${entry.versionId} deleted")(_ => s"${entry.versionId} added"))
      ) + ")"
}

object Repo
{
  def ofJsonDecoder(fileBasedJsonDecoder: Decoder[FileBased]): Repo =
    signatureVerifying(new FileBasedVerifier(DoNotVerifySignatureVerifier, fileBasedJsonDecoder))

  def signatureVerifying(fileBasedVerifier: FileBasedVerifier[FileBased]): Repo =
    new Repo(Nil, Set.empty, Map.empty, fileBasedVerifier)

  @TestOnly
  private[filebased] object testOnly {
    implicit final class OpRepo(private val underlying: Repo.type) extends AnyVal {
      def fromOp(versionIds: Seq[VersionId], operations: Iterable[Operation], fileBasedVerifier: FileBasedVerifier[FileBased]) =
        new Repo(
          versionIds.toList,
          versionIds.toSet,
          operations.toVector.view.reverse.map(_.fold(o => (o.value.id.path, o.value.id.versionId -> Some(o)), o => (o.path, o.versionId -> None)))
            .groupMap(_._1)(_._2)
            .mapValuesStrict(_.map(o => Entry(o._1, o._2)).toList),
          fileBasedVerifier)
    }

    sealed trait Operation {
      def fold[A](whenChanged: Signed[FileBased] => A, whenDeleted: FileBasedId_ => A): A
    }
    final case class Changed(fileBased: Signed[FileBased]) extends Operation {
      def fold[A](whenChanged: Signed[FileBased] => A, whenDeleted: FileBasedId_ => A) = whenChanged(fileBased)
    }
    final case class Deleted(id: FileBasedId_) extends Operation {
      def fold[A](whenChanged: Signed[FileBased] => A, whenDeleted: FileBasedId_ => A) = whenDeleted(id)
    }
  }

  final case class Entry private(versionId: VersionId, maybeSignedFileBased:  Option[Signed[FileBased]])

  private implicit class RichVersionToFileBasedOption(private val versionToFileBasedOption: List[Entry])
  extends AnyVal {
    /** None: no such version; Some(None): deleted */
    def fileBasedOption(history: List[VersionId]): Option[Option[Signed[FileBased]]] =
      history.map(v => versionToFileBasedOption.collectFirst { case Entry(`v`, o) => o }).headOption.flatten
  }

  final case class DeletedProblem private[Repo](id: FileBasedId_)
    extends Problem.Lazy(s"Has been deleted: $id")

  final case class DuplicateVersionProblem private[Repo](versionId: VersionId)
    extends Problem.Lazy(s"Duplicate VersionId '${versionId.string}'")

  final case class ObjectVersionDoesNotMatchProblem(versionId: VersionId, fileBasedId: FileBasedId[_ <: TypedPath]) extends Problem.Coded {
    def arguments = Map("versionId" -> versionId.string, "id" -> fileBasedId.toString)
  }

  final case class EventVersionDoesNotMatchProblem(versionId: VersionId, event: FileBasedAddedOrChanged) extends Problem.Coded {
    def arguments = Map("versionId" -> versionId.string, "event" -> event.toShortString)
  }
}
