package com.sos.scheduler.engine.data.filebased

/**
  * @author Joacim Zschimmer
  */
final case class UnknownTypedPath(string: String) extends TypedPath {
  validate()

  override def companion = UnknownTypedPath
}

object UnknownTypedPath extends TypedPath.Companion[UnknownTypedPath]{
  override def fileBasedType = FileBasedType.Unknown

  override val camelName = "Unknown"

  override protected[engine] def isEmptyAllowed = true
  override protected[engine] def isSingleSlashAllowed = true
  override protected[engine] def isCommaAllowed = true
}
