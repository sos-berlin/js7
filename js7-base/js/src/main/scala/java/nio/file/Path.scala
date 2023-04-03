package java.nio.file

final class Path(string: String) {
  override def toString = string

  def toFile: java.io.File =
    throw new NotImplementedError
}
