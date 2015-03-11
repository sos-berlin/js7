package com.sos.scheduler.engine.taskserver.module

import com.sos.scheduler.engine.taskserver.module.java.JavaModule
import com.sos.scheduler.engine.taskserver.module.shell.ShellModule

/**
 * @author Joacim Zschimmer
 */
trait Module {
  def moduleLanguage: ModuleLanguage
}

object Module {
  def apply(moduleLanguage: ModuleLanguage, script: Script, javaClassNameOption: Option[String]) =
    moduleLanguage match {
      case ShellModuleLanguage ⇒
        for (name ← javaClassNameOption) throw new IllegalArgumentException(s"language '$moduleLanguage' conflicts with parameter javaClass='$name'")
        new ShellModule(script)
      case JavaModuleLanguage ⇒
        JavaModule(
          newClassInstance = javaClassNameOption match {
            case Some(o) ⇒ () ⇒ Class.forName(o).newInstance()
            case None ⇒ throw new NoSuchElementException(s"Language '$moduleLanguage' requires a class name")
          })
      case _ ⇒ throw new IllegalArgumentException(s"Unsupported language $moduleLanguage")
    }
}
