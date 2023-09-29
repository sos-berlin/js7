package js7.base.utils

/**
  * @author Joacim Zschimmer
  */
object IntelliJUtils:
  /** Das Scala-Plugin erkennt nicht alle nötigen Imports impliziter Namen und organisiert sie weg. */
  def intelliJuseImport(o: => Any): Unit = {}
