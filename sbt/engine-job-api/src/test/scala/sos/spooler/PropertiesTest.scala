package sos.spooler

import com.sos.scheduler.engine.common.scalautil.Logger
import java.lang.reflect.{Method, Modifier, Type}
import org.junit.runner.RunWith
import org.reflections.Reflections
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.collection.JavaConversions._
import sos.spooler.PropertiesTest._

/** Prüft für alle Idispatch-Klassen, ob die zugehörigen Bean-Klassen korrekt sind. */
@RunWith(classOf[JUnitRunner])
final class PropertiesTest extends FreeSpec {

  for (javaClass ← new Reflections(sosSpoolerPackage.getName).getSubTypesOf(classOf[Idispatch]).toSeq sortBy { _.getName }) {
    val beanClass = Class.forName(javaClass.getName + "Bean")
    val beanClassName = beanClass.getSimpleName

    def normalizedCalls(c: Class[_], normalize: Method ⇒ NormalizedCall): Set[NormalizedCall] =
      (c.getDeclaredMethods.toSeq filter isPublic filterNot isDeprecated filterNot isIgnored map normalize).toSet
    def isPublic(m: Method) = (m.getModifiers & Modifier.PUBLIC) != 0
    def isDeprecated(m: Method) = m.getAnnotation(classOf[Deprecated]) != null
    def isIgnored(m: Method) = ignoredMethodNames contains m.getName

    s"${beanClass.getSimpleName}" in {
      val javaMethods = normalizedCalls(javaClass, normalizeJavaMethod)
      val beanMethods = normalizedCalls(beanClass, normalizeBeanMethod)
      val goodMethods = javaMethods intersect beanMethods
      val missingMethods = javaMethods -- beanMethods
      val wrongMethods = beanMethods -- javaMethods

      for (m ← goodMethods) logger debug s"Good: $beanClassName $m"
      if (missingMethods.nonEmpty) fail(s"Missing methods in $beanClassName: ${NormalizedCall.toString(missingMethods)} - forgot @SchedulerGetter?")
      if (wrongMethods.nonEmpty) fail(s"Wrong methods in $beanClassName: ${NormalizedCall.toString(wrongMethods)}")
    }
  }
}

object PropertiesTest {
  private val ignoredMethodNames = Set("toBean", "getDelegate")
  private val sosSpoolerPackage = getClass.getPackage
  private val logger = Logger(getClass)

  private def normalizeJavaMethod(m: Method): NormalizedCall = {
    def toTypeName(o: Type) = o match {
      case o: Class[_] if o.getPackage == sosSpoolerPackage ⇒ TypeName(TypeName.schedulerClassPrefix + o.getSimpleName)
      case _ ⇒ TypeName(o.toString)
    }

    val name = m.getName
    val returnType = m.getGenericReturnType
    val annotatedAsGetter = m.getAnnotation(classOf[SchedulerGetter]) != null
    m.getGenericParameterTypes match {
      case Array(typ) if (name startsWith "set_") && isVoid(returnType) ⇒
        require(!annotatedAsGetter, s"Setter $name should not be annotated with @SchedulerGetter")
        Setter(name.substring(4), toTypeName(typ))
      case Array() if (name startsWith "is_") && isBoolean(returnType) ⇒
        Getter(name.substring(3), toTypeName(returnType))
      case Array() if !isVoid(returnType) && annotatedAsGetter ⇒
        Getter(name, toTypeName(returnType))
      case parameterTypes ⇒
        require(!annotatedAsGetter, s"Method $name should not be annotated with @SchedulerGetter")
        NormalizedMethod(name, toTypeName(returnType), parameterTypes map toTypeName)
    }
  }

  private def normalizeBeanMethod(m: Method): NormalizedCall = {
    def toTypeName(o: Type) = o match {
      case o: Class[_] if o.getName startsWith "sos.spooler" ⇒
        require(o.getName endsWith "Bean", s"Method ${m.getName} uses non-bean $o")
        TypeName(TypeName.schedulerClassPrefix + (o.getSimpleName stripSuffix "Bean"))
      case _ ⇒
        TypeName(o.toString)
    }

    def propertyName(offset: Int) = m.getName()(offset).toLower +: m.getName.substring(offset + 1)

    val name = m.getName
    val returnType = m.getGenericReturnType
    def isCamel(prefix: String)(o: String) = o.length > prefix.length && (o startsWith prefix) && o(prefix.length).isUpper
    m.getGenericParameterTypes match {
      case Array(typ) if isCamel("set")(name) && isVoid(returnType) ⇒
        Setter(propertyName(3), toTypeName(typ))
      case Array() if isCamel("get")(name) && !isVoid(returnType) ⇒
        Getter(propertyName(3), toTypeName(returnType))
      case Array() if isCamel("is")(name) && isBoolean(returnType) ⇒
        Getter(propertyName(2), toTypeName(returnType))
      case parameterTypes ⇒
        NormalizedMethod(name, toTypeName(returnType), parameterTypes map toTypeName)
    }
  }

  private def isVoid(t: Type) =
    t.toString == "void"

  private def isBoolean(t: Type) =
    t.toString == "boolean"

  private sealed trait NormalizedCall

  object NormalizedCall {
    def toString(o: Iterable[NormalizedCall]) = o.toSeq sortBy { _.toString } mkString ", "
  }

  private case class Getter(name: String, typ: TypeName)
  extends NormalizedCall {
    override def toString = s"$name: $typ (getter)"
  }

  private case class Setter(name: String, typ: TypeName)
  extends NormalizedCall {
    override def toString = s"$name: $typ (setter)"
  }

  private case class NormalizedMethod(name: String, resultType: TypeName, parameterTypes: Seq[TypeName])
  extends NormalizedCall {
    override def toString =
      s"$name(${parameterTypes mkString ", "}): $resultType (method)"
  }

  private case class TypeName(string: String) {
    override def toString = string.toString
  }

  private object TypeName {
    val schedulerClassPrefix = "Scheduler-"
  }
}
