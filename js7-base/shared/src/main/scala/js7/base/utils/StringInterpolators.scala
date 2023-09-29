package js7.base.utils

object StringInterpolators:

  def interpolate(
    stringContext: StringContext, args: Seq[Any],
    argToString: Any => String = _.toString,
    reserve: Int = 100)
  : String =
    val builder = new java.lang.StringBuilder(stringContext.parts.view.map(_.length).sum + reserve)
    interpolateTo(stringContext, args, argToString: Any => String)(builder)
    builder.toString

  def interpolateTo(
    stringContext: StringContext, args: Seq[Any],
    argToString: Any => String = _.toString)
    (appendable: Appendable)
  : Unit =
    StringContext.checkLengths(args, stringContext.parts)
    val p = stringContext.parts.iterator
    appendable.append(p.next())
    for arg <- args do
      appendable.append(argToString(arg))
      appendable.append(p.next())
