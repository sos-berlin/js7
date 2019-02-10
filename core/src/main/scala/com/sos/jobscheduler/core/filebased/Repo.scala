package com.sos.jobscheduler.core.filebased

import cats.data.Validated.{Invalid, Valid}
import cats.instances.vector._
import cats.syntax.traverse._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Collections._
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.common.scalautil.Memoizer
import com.sos.jobscheduler.data.crypt.Signed
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, FileBasedAddedOrChanged, FileBasedChanged, FileBasedDeleted, FileBasedEvent, VersionAdded}
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId, FileBasedId_, RepoEvent, TypedPath, VersionId}
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable
import scala.collection.immutable.{Iterable, Seq}

/**
  * Representation of versioned FileBased (configuration objects).
  * @author Joacim Zschimmer
  */
final case class Repo private(
  versions: List[VersionId],
  idToSignedFileBased: Map[FileBasedId_, Option[Signed[FileBased]]],
  fileBasedVerifier: FileBasedVerifier)
{
  assert(versions.nonEmpty || idToSignedFileBased.isEmpty)

  import Repo._

  lazy val versionId: VersionId = versions.headOption getOrElse VersionId.Anonymous

  private lazy val pathToVersionToSignedFileBased: Map[TypedPath, Map[VersionId, Option[Signed[FileBased]]]] =
    idToSignedFileBased.map { case (id, maybeSigned) ⇒ (id.path, id.versionId, maybeSigned) }
      .groupBy(_._1)
      .mapValuesStrict(_ toKeyedMap (_._2) mapValuesStrict (_._3))

  lazy val currentVersion: Map[TypedPath, Signed[FileBased]] =
    for {
      (path, versionToFileBased) ← pathToVersionToSignedFileBased
      fileBasedOption ← versionToFileBased.fileBasedOption(versions)
      fileBased ← fileBasedOption
    } yield path → fileBased

  private lazy val typeToPathToCurrentFileBased: FileBased.Companion_ ⇒ Map[TypedPath, Signed[FileBased]] =
    Memoizer.nonStrict { companion: FileBased.Companion_ ⇒
      currentVersion collect {
        case (path, signedFileBased) if signedFileBased.value.companion == companion ⇒
          path → signedFileBased
      }
    }

  /** Returns the difference to the repo as events. */
  def fileBasedToEvents(versionId: VersionId, changed: Iterable[Signed[FileBased]], deleted: Iterable[TypedPath] = Nil)
  : Checked[Seq[RepoEvent]] =
    checkVersion(versionId, changed) flatMap { changed ⇒
      val addedOrChanged = changed flatMap toAddedOrChanged
      addedOrChanged.checkUniqueness(_.path) map { _ ⇒
        val deletedEvents = deleted
          .filterNot(addedOrChanged.map(_.path).toSet)  // delete and change?
          .filter(currentVersion.keySet)  // delete unknown?
          .map(FileBasedDeleted.apply)
        Vector(VersionAdded(versionId)) ++ deletedEvents ++ addedOrChanged
      }
    }

  private def checkVersion(versionId: VersionId, signedFileBased: Iterable[Signed[FileBased]]): Checked[Vector[Signed[FileBased]]] =
    signedFileBased.toVector.traverse(o ⇒ o.value.id.versionId match {
      case `versionId` ⇒ Valid(o)
      case _ ⇒ Invalid(Problem(s"Expected version '${versionId.string}' in '${o.value.id}'"))
    })

  private def toAddedOrChanged(signedFileBased: Signed[FileBased]): Option[RepoEvent.FileBasedEvent] = {
    val fileBased = signedFileBased.value
    currentVersion.get(fileBased.path) match {
      //case Some(existing) if existing.value.withVersion(fileBased.id.versionId) == fileBased ⇒
      case Some(existing) if existing == signedFileBased ⇒
        None

      case Some(_) ⇒
        Some(FileBasedChanged(fileBased.path, signedFileBased.signedString))

      case None ⇒
        Some(FileBasedAdded(fileBased.path, signedFileBased.signedString))
    }
  }

  def currentTyped[A <: FileBased](implicit A: FileBased.Companion[A]): Map[A#Path, A] =
    typeToPathToCurrentFileBased(A).mapValues(_.value).asInstanceOf[Map[A#Path, A]]

  def currentFileBaseds: immutable.Iterable[FileBased] =
    currentSignedFileBaseds.map(_.value)

  def currentSignedFileBaseds: immutable.Iterable[Signed[FileBased]] =
    currentVersion.values.toImmutableIterable

  def applyEvents(events: Seq[RepoEvent]): Checked[Repo] = {
    var result = Checked(this)
    val iterator = events.iterator
    while (result.isValid && iterator.hasNext) {
      result = result flatMap (_.applyEvent(iterator.next()))
    }
    result
  }

  def applyEvent(event: RepoEvent): Checked[Repo] =
    event match {
      case VersionAdded(version) ⇒
        if (versions exists version.==)
          DuplicateVersionProblem(version)
        else
          Valid(copy(versions = version :: versions))

      case event: FileBasedEvent ⇒
        if (versions.isEmpty)
          Problem(s"Missing first event VersionAdded for Repo")
        else
          event match {
            case event @ FileBasedAddedOrChanged(path, signedString) ⇒
              fileBasedVerifier.verify(signedString) flatMap { signed ⇒
                val fileBased = signed.value
                if (path != fileBased.path)
                  Problem(s"Error in FileBasedAddedOrChanged: path=$path does not equals path=${fileBased.path}")
                else if (fileBased.path.isAnonymous)
                  Problem(s"Adding an anonymous ${fileBased.companion.name}?")
                else if (fileBased.id.versionId != versionId)
                  Problem(s"Version '${versionId.string}' expected in ${event.toShortString}")
                else
                  Valid(addEntry(fileBased.path, Some(Signed(fileBased withVersion versionId, signedString))))
              }

            case FileBasedDeleted(path) ⇒
              Valid(addEntry(path, None))
          }
    }

  private def addEntry(path: TypedPath, fileBasedOption: Option[Signed[FileBased]]): Repo = {
    val version = versions.head
    copy(idToSignedFileBased = idToSignedFileBased + ((path % version) → fileBasedOption))
  }

  def pathToCurrentId[P <: TypedPath](path: P): Checked[FileBasedId[P]] =
    for {
      vToF ← pathToVersionToSignedFileBased.checked(path)
      o ← vToF.fileBasedOption(versions) toChecked Problem(s"No such path '$path'")/*should no happen*/
      signedFileBased ← o toChecked Problem(s"Has been deleted: $path")
    } yield signedFileBased.value.id.asInstanceOf[FileBasedId[P]]

  /** Returns the FileBased to a FileBasedId. */
  def idTo[A <: FileBased](id: FileBasedId[A#Path])(implicit A: FileBased.Companion[A]): Checked[A] =
    idToSigned[A](id) map (_.value)

  /** Returns the FileBased to a FileBasedId. */
  private def idToSigned[A <: FileBased](id: FileBasedId[A#Path])(implicit A: FileBased.Companion[A]): Checked[Signed[A]] =
    for {
      versionToSignedFileBased ← pathToVersionToSignedFileBased.checked(id.path)
      fileBasedOption ← versionToSignedFileBased.checked(id.versionId) orElse (
        for {
          history ← historyBefore(id.versionId)
          fb ← versionToSignedFileBased.fileBasedOption(history) toChecked Problem(s"No such '$id'")
        } yield fb): Checked[Option[Signed[FileBased]]]
      signedFileBased ← fileBasedOption.toChecked(DeletedProblem(id))
    } yield signedFileBased.copy(signedFileBased.value.cast[A])

  /** Converts the Repo to an event sequence, regarding only a given type. */
  def eventsFor(is: TypedPath.AnyCompanion ⇒ Boolean): Seq[RepoEvent] =
    toEvents collect {
      case e: VersionAdded ⇒ e
      case e: FileBasedEvent if is(e.path.companion) ⇒ e
    }

  /** Converts the Repo to an event sequence. */
  private[filebased] def toEvents: Seq[RepoEvent] = {
    val versionToFileBasedOptions: Map[VersionId, Seq[Either[Signed[FileBased/*added/updated*/], TypedPath/*deleted*/]]] =
      pathToVersionToSignedFileBased.toVector.sortBy(_._1)/*for testing*/
        .flatMap { case (path, versionToFileBasedOpt) ⇒
          versionToFileBasedOpt map { case (v, opt) ⇒ v → (opt map Left.apply getOrElse Right(path)) }
        }
        .groupBy(_._1).mapValues(_ map (_._2))
    versions.tails
      .map {
        case Nil ⇒  // Last of tails
          Vector.empty
        case version :: history ⇒
          Vector(VersionAdded(version)) ++
            versionToFileBasedOptions.getOrElse(version, Nil).map {
              case Left(signedFileBased) ⇒
                val vToF = pathToVersionToSignedFileBased(signedFileBased.value.path)
                if (vToF.fileBasedOption(history).flatten.isDefined)
                  FileBasedChanged(signedFileBased.value.path, signedFileBased.signedString)
                else
                  FileBasedAdded(signedFileBased.value.path, signedFileBased.signedString)
              case Right(path) ⇒
                FileBasedDeleted(path)
            }
      }
      .toVector.reverse.flatten
  }

  private[filebased] def historyBefore(versionId: VersionId): Checked[List[VersionId]] =
    versions.dropWhile(versionId.!=) match {
      case Nil ⇒ Problem(s"No such '$versionId'")
      case _ :: tail ⇒ Valid(tail)
    }

  def newVersionId(): VersionId =
    VersionId.generate(isKnown = versions.contains)

  override def toString = s"Repo($versions," +
    idToSignedFileBased.keys.toVector.sortBy(_.toString).map(id ⇒ idToSignedFileBased(id).fold(s"$id deleted")(_.value.id.toString)) +
    ")"
}

object Repo
{
  def empty(fileBasedVerifier: FileBasedVerifier) =
    new Repo(Nil, Map.empty, fileBasedVerifier)

  @TestOnly
  private[filebased] def apply(versionIds: Seq[VersionId], fileBased: Iterable[Entry], fileBasedVerifier: FileBasedVerifier) =
    new Repo(versionIds.toList, fileBased.map(_.fold(o ⇒ o.value.id → Some(o), _ → None)).uniqueToMap, fileBasedVerifier)

  private[filebased] sealed trait Entry {
    def fold[A](whenChanged: Signed[FileBased] ⇒ A, whenDeleted: FileBasedId_ ⇒ A): A
  }
  private[filebased] final case class Changed(fileBased: Signed[FileBased]) extends Entry {
    def fold[A](whenChanged: Signed[FileBased] ⇒ A, whenDeleted: FileBasedId_ ⇒ A) = whenChanged(fileBased)
  }
  private[filebased] final case class Deleted(id: FileBasedId_) extends Entry {
    def fold[A](whenChanged: Signed[FileBased] ⇒ A, whenDeleted: FileBasedId_ ⇒ A) = whenDeleted(id)
  }

  ///** Computes `a` - `b`, ignoring VersionId, and returning `Seq[RepoEvent.FileBasedEvent]`.
  //  * Iterables must contain only one version per path.
  //  */
  //private[filebased] def diff(a: Iterable[FileBased], b: Iterable[FileBased]): Seq[RepoEvent.FileBasedEvent] =
  //  diff(a toKeyedMap (_.path): Map[TypedPath, FileBased],
  //       b toKeyedMap (_.path): Map[TypedPath, FileBased])
  //
  ///** Computes `a` - `b`, ignoring VersionId, and returning `Seq[RepoEvent.FileBasedEvent]`.
  //  */
  //private def diff(a: Map[TypedPath, FileBased], b: Map[TypedPath, FileBased]): Seq[RepoEvent.FileBasedEvent] = {
  //  val addedPaths = a.keySet -- b.keySet
  //  val deletedPaths = b.keySet -- a.keySet
  //  val changedPaths = (a.keySet intersect b.keySet) filter (path ⇒ a(path) != b(path))
  //  deletedPaths.toImmutableSeq.map(FileBasedDeleted.apply) ++
  //    addedPaths.toImmutableSeq.map(a).map(o ⇒ FileBasedAdded(o.withoutVersion)) ++
  //    changedPaths.toImmutableSeq.map(a).map(o ⇒ FileBasedChanged(o.withoutVersion))
  //}

  private implicit class RichVersionToFileBasedOption(private val versionToFileBasedOption: Map[VersionId, Option[Signed[FileBased]]])
  extends AnyVal {
    /** None: no such version; Some(None): deleted */
    def fileBasedOption(history: List[VersionId]): Option[Option[Signed[FileBased]]] =
      history.collectFirst { case v if versionToFileBasedOption contains v ⇒ versionToFileBasedOption(v) }
  }

  final case class DeletedProblem private[Repo](id: FileBasedId_)
    extends Problem.Lazy(s"Has been deleted: $id")

  final case class DuplicateVersionProblem private[Repo](versionId: VersionId)
    extends Problem.Lazy(s"Duplicate VersionId '${versionId.string}'")
}
