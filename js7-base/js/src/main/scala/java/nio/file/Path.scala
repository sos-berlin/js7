package java.nio.file

final class Path(string: String) {
  override def toString = string

  def toFile: Nothing = throw new NotImplementedError
}
