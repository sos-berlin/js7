package java.nio.file

object Paths:

  def get(string: String): Path =
    new Path(string)
