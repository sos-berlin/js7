package com.sos.scheduler.engine.data.folder

import com.sos.scheduler.engine.data.filebased.{FileBasedType, TypedPath}

final case class FolderPath(string: String) extends TypedPath {
  requireIsAbsolute()

  // TODO Sollte nicht mit Schrägstrich enden ?

  def fileBasedType = FileBasedType.folder

  def /(name: String): FolderPath = {
    require(!name.contains('/'), "Name must not contains a slash '/'")
    FolderPath(s"$withoutSlash/$name")
  }

  lazy val lastName: String = withoutSlash lastIndexOf '/' match {
    case -1 ⇒ ""
    case slash ⇒ withoutSlash substring slash + 1
  }

  lazy val nesting = withoutSlash count { _ == '/' }

  private lazy val withoutSlash = string stripSuffix "/"
}

object FolderPath extends TypedPath.Companion[FolderPath] {
  def Root = FolderPath("/")
}
