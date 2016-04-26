package com.sos.scheduler.engine.taskserver.module

import com.sos.scheduler.engine.common.scalautil.ScalaUtils.implicitClass
import org.jetbrains.annotations.TestOnly
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
sealed trait ModuleArguments {
  def language: ModuleLanguage
}

object ModuleArguments {
  def apply(
    language: ModuleLanguage,
    javaClassNameOption: Option[String],
    script: Script): ModuleArguments =
  {
    def requireUnused[A](name: String, option: Option[A]) =
      for (o ← option) throw new IllegalArgumentException(s"language='$language' conflicts with parameter $name='$o'")

    language match {
      case ShellModuleLanguage ⇒
        requireUnused("java_class", javaClassNameOption)
        ShellModuleArguments(script)

      case JavaModuleLanguage ⇒
        JavaModuleArguments(
          className = javaClassNameOption getOrElse { throw new IllegalArgumentException(s"language='$language' requires a class name") } )

      case JavaScriptModuleLanguage(scriptLanguage) ⇒
        requireUnused("java_class", javaClassNameOption)
        JavaScriptModuleArguments(scriptLanguage, script)
    }
  }

  final case class ShellModuleArguments(script: Script)
  extends ModuleArguments {
    def language = ShellModuleLanguage
  }

  case class JavaModuleArguments(className: String)
  extends ModuleArguments {
    def language = JavaModuleLanguage
  }

  @TestOnly
  final class TestJavaModuleArguments[A: ClassTag](val newModule: () ⇒ A)
  extends JavaModuleArguments(implicitClass[A].getName)

  final case class JavaScriptModuleArguments(scriptLanguage: String, script: Script)
  extends ModuleArguments {
    def language = JavaModuleLanguage
  }
}
