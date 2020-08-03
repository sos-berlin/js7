package js7.data.filebased

import cats.instances.either._
import cats.instances.vector._
import cats.syntax.traverse._
import io.circe.Decoder
import js7.base.crypt.Signed
import js7.base.problem.Checked._
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Collections._
import js7.base.utils.Collections.implicits._
import js7.base.utils.Memoizer
import js7.base.utils.ScalaUtils.syntax._
import js7.data.crypt.FileBasedVerifier
import js7.data.filebased.Repo.Entry
import js7.data.filebased.RepoEvent.{FileBasedAdded, FileBasedAddedOrChanged, FileBasedChanged, FileBasedDeleted, FileBasedEvent, VersionAdded}
import org.jetbrains.annotations.TestOnly
import scala.collection.{View, mutable}

/**
  * Representation of versioned FileBased (configuration objects).
  * @param versionSet `versionSet == versions.toSet`
  * @author Joacim Zschimmer
  */
final case class Repo private(
  versions: List[VersionId],
  versionSet: Set[VersionId],
  pathToVersionToSignedFileBased: Map[TypedPath, List[Entry]],
  fileBasedVerifier: Option[FileBasedVerifier[FileBased]])
{
  assertThat(versions.nonEmpty || pathToVersionToSignedFileBased.isEmpty)

  /** `fileBasedVerifier` is not compared - for testing only. */
  override def equals(o: Any) = o match {
    case o: Repo => versions == o.versions && pathToVersionToSignedFileBased == o.pathToVersionToSignedFileBased
    case _ => false
  }

  import Repo._

  lazy val versionId: VersionId = versions.headOption getOrElse VersionId.Anonymous

  def currentVersionSize =
    currentSignedFileBaseds.size

  lazy val currentVersion: Map[TypedPath, Signed[FileBased]] =
    currentSignedFileBaseds.view.map(o => o.value.path -> o).toMap

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
    checkVersion(versionId, changed)
      .flatMap { changed =>
        val addedOrChanged = changed flatMap toAddedOrChanged
        for (_ <- addedOrChanged.checkUniqueness(_.path)) yield {
          lazy val addedSet = addedOrChanged.view.collect { case a: FileBasedAdded => a.path }.toSet
          val deletedEvents = deleted.view
            .filterNot(addedSet)  // delete and change?
            .filter(exists)  // delete unknown?
            .map(FileBasedDeleted.apply)
          (new View.Single(VersionAdded(versionId)) ++ deletedEvents ++ addedOrChanged)
            .toVector
        }
      }

  private def diffCurrentVersion(base: Repo): Seq[RepoChange] = {
    // Optimized for small differences (less materialization, more views)
    val added = currentFileBaseds.view
      .filter(o => !base.exists(o.path))
      .map(o => o.path -> o)
      .toMap[TypedPath, FileBased]
    val updated = currentFileBaseds.view
      .filter(o => base.pathToFileBased(o.path).exists(_.id.versionId != o.id.versionId))
    val deleted = base.currentFileBaseds.view
      .map(_.path)
      .filter(path => !exists(path) && !added.contains(path))
    (deleted.map(RepoChange.Deleted.apply) ++
      updated.map(RepoChange.Updated.apply) ++
      added.values.view.map(RepoChange.Added.apply)
    ).toVector
      .sortBy(_.path)
  }

  private def checkVersion(versionId: VersionId, signedFileBased: Iterable[Signed[FileBased]]): Checked[Vector[Signed[FileBased]]] =
    signedFileBased.toVector.traverse(o =>
      o.value.id.versionId match {
        case `versionId` => Right(o)
        case _ => Left(ObjectVersionDoesNotMatchProblem(versionId, o.value.id))
      })

  private def toAddedOrChanged(signedFileBased: Signed[FileBased]): Option[RepoEvent.FileBasedEvent] = {
    val path = signedFileBased.value.path
    pathToSigned(path) match {
      case Right(`signedFileBased`) => None
      case Right(_) => Some(FileBasedChanged(signedFileBased))
      case Left(_) => Some(FileBasedAdded(signedFileBased))
    }
  }

  def typedCount[A <: FileBased](implicit A: FileBased.Companion[A]): Int =
    pathToVersionToSignedFileBased.values.view.flatten.count {
      case Entry(_, Some(signed)) if signed.value.companion == A => true
      case _ => false
    }

  def currentTyped[A <: FileBased](implicit A: FileBased.Companion[A]): Map[A#Path, A] =
    typeToPathToCurrentFileBased(A).view.mapValues(_.value).toMap.asInstanceOf[Map[A#Path, A]]

  def currentFileBaseds: View[FileBased] =
    currentSignedFileBaseds.view.map(_.value)

  def currentSignedFileBaseds: View[Signed[FileBased]] =
    for {
      versionToFileBased <- pathToVersionToSignedFileBased.values.view
      fileBased <- versionToFileBased.head.maybeSignedFileBased
    } yield fileBased

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
          DuplicateKey("VersionId", version)
        else
          Right(copy(
            versions = version :: versions,
            versionSet = versionSet + version))

      case event: FileBasedEvent =>
        if (versions.isEmpty)
          Problem.pure(s"Missing initial VersionAdded event for Repo")
        else
          event match {
            case event: FileBasedAdded =>
              if (exists(event.path))
                Left(DuplicateKey("TypedPath", event.path))
              else
                addOrChange(event)

            case event: FileBasedChanged =>
              pathToFileBased(event.path)
                .flatMap(_ => addOrChange(event))

            case FileBasedDeleted(path) =>
              for (_ <- pathToFileBased(event.path)) yield
                addEntry(path, None)
          }
    }

  private def addOrChange(event: FileBasedAddedOrChanged): Checked[Repo] = {
    fileBasedVerifier match {
      case Some(verifier) =>
        verifier.verify(event.signedString).map(_.fileBased).flatMap(fileBased =>
          if (event.path != fileBased.path)
            Problem.pure(s"Error in FileBasedAddedOrChanged event: path=${event.path} does not equal path=${fileBased.path}")
          else if (fileBased.path.isAnonymous)
            Problem.pure(s"Adding an anonymous ${fileBased.companion.name} is not allowed")
          else if (fileBased.id.versionId != versionId)
            EventVersionDoesNotMatchProblem(versionId, event)
          else
            Right(addEntry(fileBased.path, Some(js7.base.crypt.Signed(fileBased withVersion versionId, event.signedString)))))

      case None =>
        Right(addEntry(event.path, Some(event.signed)))
    }
  }

  def verify[A <: FileBased](signed: Signed[A]): Checked[A] =
    fileBasedVerifier match {
      case Some(verifier) =>
        verifier.verify(signed.signedString).map(_.fileBased).flatMap(fileBased =>
          if (fileBased != signed.value)
            Left(Problem.pure("fileBased != signed.value"))
          else
            Right(signed.value))

      case None =>
        Right(signed.value)
    }

  private def addEntry(path: TypedPath, fileBasedOption: Option[Signed[FileBased]]): Repo = {
    val version = versions.head
    copy(pathToVersionToSignedFileBased =
      pathToVersionToSignedFileBased +
        (path ->
          (Entry(version, fileBasedOption) :: (pathToVersionToSignedFileBased.getOrElse(path, Nil)))))
  }

  def exists(path: TypedPath): Boolean =
    pathToVersionToSignedFileBased.get(path) match {
      case None => false
      case Some(entries) => entries.head.maybeSignedFileBased.isDefined  // Deleted?
    }

  /** Returns the current FileBased to a Path. */
  def pathTo[A <: FileBased](path: A#Path)(implicit A: FileBased.Companion[A]): Checked[A] =
    pathToVersionToSignedFileBased
      .checked(path)
      .flatMap { entries =>
        val entry = entries.head
        entry
          .maybeSignedFileBased
          .toChecked(FileBasedDeletedProblem(path ~ entry.versionId))
          .map(_.value.asInstanceOf[A])
      }

  def pathToFileBased(path: TypedPath): Checked[FileBased] =
    pathToSigned(path)
      .map(_.value)

  private def pathToSigned(path: TypedPath): Checked[Signed[FileBased]] =
    pathToVersionToSignedFileBased
      .checked(path)
      .flatMap { entries =>
        val entry = entries.head
        entry
          .maybeSignedFileBased
          .toChecked(FileBasedDeletedProblem(path ~ entry.versionId))
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
        .toChecked(UnknownKeyProblem("FileBasedId", id))
        .orElse(
          for {
            history <- historyBefore(id.versionId)
            fb <- findInHistory(versionToSignedFileBased, history.contains) toChecked UnknownKeyProblem("FileBasedId", id)
          } yield fb
        ): Checked[Option[Signed[FileBased]]]
      signedFileBased <- fileBasedOption.toChecked(FileBasedDeletedProblem(id))
    } yield signedFileBased.copy(signedFileBased.value.cast[A])

  /** Converts the Repo to an event sequence, regarding only a given type. */
  def eventsFor(is: TypedPath.AnyCompanion => Boolean): Seq[RepoEvent] =
    toEvents collect {
      case e: VersionAdded => e
      case e: FileBasedEvent if is(e.path.companion) => e
    }

  /** Convert the Repo to an event sequence ordered by VersionId. */
  private[filebased] def toEvents: Seq[RepoEvent] = {
    type DeletedOrUpdated = Either[TypedPath/*deleted*/, Signed[FileBased/*added/updated*/]]
    val versionToChanges: Map[VersionId, Seq[DeletedOrUpdated]] =
      pathToVersionToSignedFileBased.toVector
        .flatMap { case (path, entries) =>
          entries.map(entry =>
            entry.versionId -> entry.maybeSignedFileBased.fold[DeletedOrUpdated](Left(path))(Right.apply))
        }
        .groupBy(_._1).view.mapValues(_.map(_._2))
        .toMap
    val versionSet = mutable.HashSet() ++ versions
    versions.view.map { version =>
      versionSet -= version
      Vector(VersionAdded(version)) ++
        versionToChanges.getOrElse(version, Nil).map {
          case Left(path) =>
            FileBasedDeleted(path)
          case Right(signedFileBased) =>
            val entries = pathToVersionToSignedFileBased(signedFileBased.value.path)
            if (findInHistory(entries, versionSet).flatten.isDefined)
              FileBasedChanged(signedFileBased)
            else
              FileBasedAdded(signedFileBased)
        }
      }
      .toVector.reverse.flatten
  }

  private[filebased] def historyBefore(versionId: VersionId): Checked[List[VersionId]] =
    versions.dropWhile(versionId.!=) match {
      case Nil => UnknownKeyProblem("VersionId", versionId)
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
    new Repo(Nil, Set.empty, Map.empty, None)

  def signatureVerifying(fileBasedVerifier: FileBasedVerifier[FileBased]): Repo =
    new Repo(Nil, Set.empty, Map.empty, Some(fileBasedVerifier))

  @TestOnly
  private[filebased] object testOnly {
    implicit final class OpRepo(private val underlying: Repo.type) extends AnyVal {
      def fromOp(versionIds: Seq[VersionId], operations: Iterable[Operation], fileBasedVerifier: Option[FileBasedVerifier[FileBased]]) =
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

  /** None: not known at or before this VersionId; Some(None): deleted at or before this VersionId. */
  private def findInHistory(entries: List[Entry], isKnownVersion: VersionId => Boolean): Option[Option[Signed[FileBased]]] =
    entries
      .dropWhile(e => !isKnownVersion(e.versionId))
      .map(_.maybeSignedFileBased)
      .headOption

  final case class FileBasedDeletedProblem private[Repo](id: FileBasedId_)
    extends Problem.Lazy(s"Has been deleted: $id")

  final case class ObjectVersionDoesNotMatchProblem(versionId: VersionId, fileBasedId: FileBasedId[_ <: TypedPath]) extends Problem.Coded {
    def arguments = Map("versionId" -> versionId.string, "id" -> fileBasedId.toString)
  }

  final case class EventVersionDoesNotMatchProblem(versionId: VersionId, event: FileBasedAddedOrChanged) extends Problem.Coded {
    def arguments = Map("versionId" -> versionId.string, "event" -> event.toShortString)
  }
}
