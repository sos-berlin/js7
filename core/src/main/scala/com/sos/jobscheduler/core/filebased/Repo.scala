package com.sos.jobscheduler.core.filebased

import cats.data.Validated.{Invalid, Valid}
import cats.instances.vector._
import cats.syntax.traverse._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Collections._
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.common.scalautil.Memoizer
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, FileBasedAddedOrChanged, FileBasedChanged, FileBasedDeleted, FileBasedEvent, VersionAdded}
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId, FileBasedId_, RepoEvent, TypedPath, VersionId}
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable
import scala.collection.immutable.{Iterable, Seq}

/**
  * Representation of versioned FileBased (configuration objects).
  * @author Joacim Zschimmer
  */
final case class Repo private(versions: List[VersionId], idToFileBased: Map[FileBasedId_, Option[FileBased]])
{
  assert(versions.nonEmpty || idToFileBased.isEmpty)

  import Repo._

  lazy val versionId: VersionId = versions.headOption getOrElse VersionId.Anonymous

  lazy val pathToVersionToFileBased: Map[TypedPath, Map[VersionId, Option[FileBased]]] =
    idToFileBased.map { case (id, fileBased) ⇒ (id.path, id.versionId, fileBased) }
      .groupBy(_._1)
      .mapValuesStrict(_ toKeyedMap (_._2) mapValuesStrict (_._3))

  lazy val currentVersion: Map[TypedPath, FileBased] =
    for {
      (path, versionToFileBased) ← pathToVersionToFileBased
      fileBasedOption ← versionToFileBased.fileBasedOption(versions)
      fileBased ← fileBasedOption
    } yield path → fileBased

  private lazy val currentPathToFileBased = currentFileBaseds toKeyedMap (_.path: TypedPath)

  private val typeToPathToCurrentFileBased: FileBased.Companion_ ⇒ Map[TypedPath, FileBased] =
    Memoizer.nonStrict { companion: FileBased.Companion_ ⇒
      currentVersion collect {
        case (path, fileBased) if fileBased.companion == companion ⇒
          path → fileBased
      }
    }

  def fileBasedToEvents(versionId: VersionId, changed: Iterable[FileBased], deleted: Iterable[TypedPath] = Nil)
  : Checked[Seq[RepoEvent]] =
    normalizeVersion(versionId, changed) flatMap { changed ⇒
      val addedOrChanged = changed flatMap toAddedOrChanged
      addedOrChanged.checkUniqueness(_.path) map { _ ⇒
        val deletedEvents = deleted
          .filterNot(addedOrChanged.map(_.path).toSet)  // delete and change?
          .filter(currentPathToFileBased.keySet)  // delete unknown?
          .map(FileBasedDeleted.apply)
        Vector(VersionAdded(versionId)) ++ deletedEvents ++ addedOrChanged
      }
    }

  private def normalizeVersion(versionId: VersionId, fileBased: Iterable[FileBased]): Checked[Vector[FileBased]] =
    fileBased.toVector.traverse(o ⇒ o.id.versionId match {
      case `versionId` ⇒ Valid(o)
      case VersionId.Anonymous ⇒ Valid(o withVersion versionId)
      case _ ⇒ Invalid(Problem(s"Expected version '${versionId.string}' in '${o.id}'"))
    })

  private def toAddedOrChanged(fileBased: FileBased): Option[RepoEvent.FileBasedEvent] =
    currentPathToFileBased.get(fileBased.path) match {
      case Some(existing) if existing.withVersion(fileBased.id.versionId) == fileBased ⇒
        None

      case Some(_) ⇒
        Some(FileBasedChanged(fileBased.withoutVersion))

      case None ⇒
        Some(FileBasedAdded(fileBased.withoutVersion))
    }

  def onlyCurrentVersion: Repo =
    new Repo(
      versions.head :: Nil,
      currentVersion.values.map(fileBased ⇒ fileBased.id → Some(fileBased)).toMap)

  def currentTyped[A <: FileBased](implicit A: FileBased.Companion[A]): Map[A#Path, A] =
    typeToPathToCurrentFileBased(A).asInstanceOf[Map[A#Path, A]]

  def currentFileBaseds: immutable.Iterable[FileBased] =
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
            case event @ FileBasedAddedOrChanged(fileBased) ⇒
              if (fileBased.path.isAnonymous)
                Problem(s"Adding an anonymous ${fileBased.companion.name}?")
              else if (!fileBased.id.versionId.isAnonymous)
                Problem(s"Version in ${event.toShortString} must not be set")
              else
                Valid(addEntry(fileBased.path, Some(fileBased withVersion versionId)))

            case FileBasedDeleted(path) ⇒
              Valid(addEntry(path, None))
          }
    }

  private def addEntry(path: TypedPath, fileBasedOption: Option[FileBased]): Repo = {
    val version = versions.head
    copy(idToFileBased = idToFileBased + ((path % version) → fileBasedOption))
  }

  def pathToCurrentId[P <: TypedPath](path: P): Checked[FileBasedId[P]] =
    for {
      vToF ← pathToVersionToFileBased.checked(path)
      o ← vToF.fileBasedOption(versions) toChecked Problem(s"No such path '$path'")/*should no happen*/
      fileBased ← o toChecked Problem(s"Has been deleted: $path")
    } yield fileBased.id.asInstanceOf[FileBasedId[P]]

  /** Returns the FileBased to a FileBasedId. */
  def idTo[A <: FileBased](id: FileBasedId[A#Path])(implicit A: FileBased.Companion[A]): Checked[A] =
    for {
      versionToFileBased ← pathToVersionToFileBased.checked(id.path)
      fileBasedOption ← versionToFileBased.checked(id.versionId) orElse (
        for {
          history ← historyBefore(id.versionId)
          fb ← versionToFileBased.fileBasedOption(history) toChecked Problem(s"No such '$id'")
        } yield fb): Checked[Option[FileBased]]
      fileBased ← fileBasedOption.toChecked(DeletedProblem(id))
    } yield fileBased.cast[A]

  /** Converts the Repo to an event sequence, regarding only a given type. */
  def eventsFor(is: TypedPath.AnyCompanion ⇒ Boolean): Seq[RepoEvent] =
    toEvents collect {
      case e: VersionAdded ⇒ e
      case e: FileBasedEvent if is(e.path.companion) ⇒ e
    }

  /** Converts the Repo to an event sequence. */
  private[filebased] def toEvents: Seq[RepoEvent] = {
    val versionToFileBasedOptions: Map[VersionId, Seq[Either[FileBased/*added/changed*/, TypedPath/*deleted*/]]] =
      pathToVersionToFileBased.toVector.sortBy(_._1)/*for testing*/
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
              case Left(fileBased) ⇒
                val vToF = pathToVersionToFileBased(fileBased.path)
                if (vToF.fileBasedOption(history).flatten.isDefined)
                  FileBasedChanged(fileBased.withoutVersion)
                else
                  FileBasedAdded(fileBased.withoutVersion)
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

  def newVersionId(): VersionId = {
    val v = VersionId("#" + Timestamp.now.toIsoString)
    if (!versions.contains(v))
      v
    else
      Iterator.from(1).map(i ⇒ VersionId(s"${v.string}-$i")).collectFirst { case w if !versions.contains(w) ⇒ w } .get
  }

  override def toString = s"Repo($versions,${idToFileBased.keys.toVector.sortBy(_.toString).map(id ⇒ idToFileBased(id).fold(s"$id deleted")(_.id.toString))})"
}

object Repo {
  val empty = new Repo(Nil, Map.empty)

  @TestOnly
  private[filebased] def apply(versionIds: Seq[VersionId], fileBased: Iterable[Entry]) =
    new Repo(versionIds.toList, fileBased.map(_.fold(o ⇒ o.id → Some(o), _ → None)).uniqueToMap)

  private[filebased] sealed trait Entry {
    def fold[A](whenChanged: FileBased ⇒ A, whenDeleted: FileBasedId_ ⇒ A): A
  }
  private[filebased] final case class Changed(fileBased: FileBased) extends Entry {
    def fold[A](whenChanged: FileBased ⇒ A, whenDeleted: FileBasedId_ ⇒ A) = whenChanged(fileBased)
  }
  private[filebased] final case class Deleted(id: FileBasedId_) extends Entry {
    def fold[A](whenChanged: FileBased ⇒ A, whenDeleted: FileBasedId_ ⇒ A) = whenDeleted(id)
  }

  /** Computes `a` - `b`, ignoring VersionId, and returning `Seq[RepoEvent.FileBasedEvent]`.
    * Iterables must contain only one version per path.
    */
  private[filebased] def diff(a: Iterable[FileBased], b: Iterable[FileBased]): Seq[RepoEvent.FileBasedEvent] =
    diff(a toKeyedMap (_.path): Map[TypedPath, FileBased],
         b toKeyedMap (_.path): Map[TypedPath, FileBased])

  /** Computes `a` - `b`, ignoring VersionId, and returning `Seq[RepoEvent.FileBasedEvent]`.
    */
  private def diff(a: Map[TypedPath, FileBased], b: Map[TypedPath, FileBased]): Seq[RepoEvent.FileBasedEvent] = {
    val addedPaths = a.keySet -- b.keySet
    val deletedPaths = b.keySet -- a.keySet
    val changedPaths = (a.keySet intersect b.keySet) filter (path ⇒ a(path) != b(path))
    deletedPaths.toImmutableSeq.map(FileBasedDeleted.apply) ++
      addedPaths.toImmutableSeq.map(a).map(o ⇒ FileBasedAdded(o.withoutVersion)) ++
      changedPaths.toImmutableSeq.map(a).map(o ⇒ FileBasedChanged(o.withoutVersion))
  }

  private implicit class RichVersionToFileBasedOption(private val versionToFileBasedOption: Map[VersionId, Option[FileBased]])
  extends AnyVal {
    /** None: no such version; Some(None): deleted */
    def fileBasedOption(history: List[VersionId]): Option[Option[FileBased]] =
      history.collectFirst { case v if versionToFileBasedOption contains v ⇒ versionToFileBasedOption(v) }
  }

  final case class DeletedProblem private[Repo](id: FileBasedId_)
    extends Problem.Lazy(s"Has been deleted: $id")

  final case class DuplicateVersionProblem private[Repo](versionId: VersionId)
    extends Problem.Lazy(s"Duplicate VersionId '${versionId.string}'")
}
