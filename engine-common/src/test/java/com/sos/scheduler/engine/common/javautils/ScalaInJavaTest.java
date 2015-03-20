package com.sos.scheduler.engine.common.javautils;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class ScalaInJavaTest {
    @Test public void test() {
        scala.collection.immutable.Set<String> scalaSet = ScalaInJava.toScalaSet("eins", "zwei");
        assertTrue(scalaSet.apply("eins"));
        assertTrue(scalaSet.apply("zwei"));
        assertFalse(scalaSet.apply("drei"));
        assertEquals(scalaSet.size(), 2);
    }
}
