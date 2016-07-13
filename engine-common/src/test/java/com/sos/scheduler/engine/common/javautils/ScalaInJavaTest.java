package com.sos.scheduler.engine.common.javautils;

import java.util.Optional;
import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class ScalaInJavaTest {

    @Test public void toScalaTest() {
        scala.collection.immutable.Set<String> scalaSet = ScalaInJava.toScalaSet("eins", "zwei");
        assertTrue(scalaSet.apply("eins"));
        assertTrue(scalaSet.apply("zwei"));
        assertFalse(scalaSet.apply("drei"));
        assertEquals(scalaSet.size(), 2);
    }

    @Test public void toJavaOption() {
        assertEquals(Optional.empty(), ScalaInJava.toJavaOptional(scala.None$.empty()));
        assertEquals(Optional.of(7), ScalaInJava.toJavaOptional(new scala.Some<>(7)));
    }

    @Test public void toScalaOption() {
        assertEquals(scala.None$.empty(), ScalaInJava.toScalaOption(Optional.empty()));
        assertEquals(new scala.Some<>(7), ScalaInJava.toScalaOption(Optional.of(7)));
    }
}
