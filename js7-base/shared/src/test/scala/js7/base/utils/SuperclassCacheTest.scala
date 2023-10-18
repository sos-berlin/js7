package js7.base.utils

import js7.base.test.OurTestSuite
import js7.base.utils.SuperclassCacheTest.*

/**
  * @author Joacim Zschimmer
  */
final class SuperclassCacheTest extends OurTestSuite:
  "test" in:
    val cache = new SuperclassCache(classOf[Super])
    assert(cache.assignableClasses(classOf[Super]) == Set(classOf[Super]))
    assert(cache.assignableClasses(classOf[A]) == Set(classOf[Super], classOf[A]))
    assert(cache.assignableClasses(classOf[B]) == Set(classOf[Super], classOf[B]))
    assert(cache.assignableClasses(classOf[A1]) == Set(classOf[Super], classOf[A], classOf[A1]))
    assert(cache.assignableClasses(classOf[A2]) == Set(classOf[Super], classOf[A], classOf[A1], classOf[A2]))
    assert(cache.assignableClasses(classOf[B1]) == Set(classOf[Super], classOf[B], classOf[B1]))

    // Differs for different JVM versions
    //assert(cache.toString == """SuperclassCache(6 classes) ⏎
    // |  js7.base.utils.SuperclassCacheTest$A2 ->
    // |    js7.base.utils.SuperclassCacheTest$A
    // |    js7.base.utils.SuperclassCacheTest$A1
    // |    js7.base.utils.SuperclassCacheTest$A2
    // |    js7.base.utils.SuperclassCacheTest$Super
    // |  js7.base.utils.SuperclassCacheTest$B1 ->
    // |    js7.base.utils.SuperclassCacheTest$B
    // |    js7.base.utils.SuperclassCacheTest$B1
    // |    js7.base.utils.SuperclassCacheTest$Super
    // |  js7.base.utils.SuperclassCacheTest$B ->
    // |    js7.base.utils.SuperclassCacheTest$B
    // |    js7.base.utils.SuperclassCacheTest$Super
    // |  js7.base.utils.SuperclassCacheTest$A ->
    // |    js7.base.utils.SuperclassCacheTest$A
    // |    js7.base.utils.SuperclassCacheTest$Super
    // |  js7.base.utils.SuperclassCacheTest$A1 ->
    // |    js7.base.utils.SuperclassCacheTest$A
    // |    js7.base.utils.SuperclassCacheTest$A1
    // |    js7.base.utils.SuperclassCacheTest$Super
    // |  js7.base.utils.SuperclassCacheTest$Super ->
    // |    js7.base.utils.SuperclassCacheTest$Super""".stripMargin)


object SuperclassCacheTest:
  private sealed trait X
  private sealed trait SuperSuper
  private sealed trait Super extends SuperSuper
  private sealed trait A extends Super
  private sealed trait B extends Super
  private sealed class A1 extends A
  private sealed class A2 extends A1, A
  private sealed trait B1 extends X, B
