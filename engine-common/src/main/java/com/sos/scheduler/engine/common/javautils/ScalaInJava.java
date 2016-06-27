package com.sos.scheduler.engine.common.javautils;

/**
 * Public functions for use of Scala in Java.
 *
 * @author Joacim Zschimmer
 */
public class ScalaInJava {
    private ScalaInJava() {}

    /**
     * @return A copy of the argument list as an immutable.Set, ignoring duplicates
     */
    @SafeVarargs
    public static <A> scala.collection.immutable.Set<A> toScalaSet(A... o) {
        @SuppressWarnings("varargs")
        scala.collection.immutable.Set<A> result = ScalaInJavaHelper$.MODULE$.toScalaSet(o);
        return result;
    }
}
