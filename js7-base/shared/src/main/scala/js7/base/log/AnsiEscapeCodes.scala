package js7.base.log

object AnsiEscapeCodes:

  /** Reset color and mode. */
  @inline val resetColor = "\u001B[0m"
  @inline val bold = "\u001B[1m"
  @inline val black = "\u001B[30m"
  @inline val red = "\u001B[31m"
  @inline val green = "\u001B[32m"
  @inline val blue = "\u001B[34m"
  @inline val magenta = "\u001B[38;5;13m"
  @inline val orange = "\u001B[38;5;9m"
