package com.sos.scheduler.engine.data.folder

import com.google.common.base.Splitter
import com.sos.scheduler.engine.base.sprayjson.SprayJson.lazyRootFormat
import com.sos.scheduler.engine.data.filebased.{AbsolutePath, HasPath, TypedPath}
import scala.collection.JavaConversions._
import scala.collection.immutable
import scala.reflect.ClassTag
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final case class FolderTree[A](
  path: FolderPath,
  leafs: immutable.Seq[A],
  subfolders: immutable.Seq[FolderTree[A]]) {

  def nonEmpty = !isEmpty
  def isEmpty = leafs.isEmpty && subfolders.isEmpty
}

object FolderTree {

  implicit def myJsonFormat[A: JsonFormat]: RootJsonFormat[FolderTree[A]] = lazyRootFormat(jsonFormat3(apply))

  val nameOrdering: Ordering[FolderTree[_]] = Ordering.by((o: FolderTree[_]) ⇒ o.path.name)
  private val PathSplitter = Splitter.on('/')

  implicit def ordering[A]: Ordering[FolderTree[A]] = Ordering by { _.path }

  def fromHasPaths[A <: HasPath](root: FolderPath, hasPaths: immutable.Seq[A]): FolderTree[A] =
    fromAny(root, hasPaths, _.path)

  def fromAny[A, P <: TypedPath: ClassTag](root: FolderPath, objects: immutable.Seq[A], toPath: A ⇒ P): FolderTree[A] =
    fromAny2[A](root, objects map { o ⇒ (split(toPath(o)) drop root.nesting) → o })

  private def fromAny2[A](root: FolderPath, allPathValues: immutable.Seq[(Iterable[String], A)]): FolderTree[A] = {
    def fromNameSeqs(folderPath: FolderPath, subpathValues: immutable.Seq[(Iterable[String], A)]): FolderTree[A] = {
      val (leafPaths, folderPaths) = subpathValues partition { case (nameSeq, obj) ⇒ nameSeq.tail.isEmpty }
      val leafs = leafPaths map { case (_, obj) ⇒ obj }
      val subfolders = for ((name, paths) ← (folderPaths groupBy { case (nameSeq, obj) ⇒ nameSeq.head }).toVector.sortBy { _._1 })
        yield fromNameSeqs(folderPath subfolder name, for ((nameSeq, obj) ← paths) yield nameSeq.tail → obj)
      new FolderTree(folderPath, leafs, subfolders)
    }
    fromNameSeqs(root, allPathValues)
  }

  private[folder] def split(path: AbsolutePath): Vector[String] =
    PathSplitter.split(path.string stripPrefix "/").toVector
}
