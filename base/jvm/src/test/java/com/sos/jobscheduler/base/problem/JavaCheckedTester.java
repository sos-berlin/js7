package com.sos.jobscheduler.base.problem;

import cats.data.Validated;

/**
 * @author Joacim Zschimmer
 */
final class JavaCheckedTester
{
    // Access to cryptic JobScheduler internals for this test only. Not an offical API.
    @SuppressWarnings("unchecked")
    private static final JavaChecked<String> valid = new JavaChecked<>(new Validated.Valid("VALID"));
    @SuppressWarnings("unchecked")
    private static final JavaChecked<String> invalid = new JavaChecked<>(new Validated.Invalid(Problem$.MODULE$.pure("PROBLEM")));

    private JavaCheckedTester() {}

    static void testIsValid() {
        assert valid.isValid();
        assert invalid.isInvalid();
    }

    static void testIsInvalid() {
        assert !valid.isInvalid();
        assert invalid.isInvalid();
    }

    static void testToOptional() {
        assert valid.toOptional().isPresent();
        assert !invalid.toOptional().isPresent();
        assertEqual(valid.toOptional().get(), "VALID");
    }

    static void testProblem() {
        assert !valid.problem().isPresent();
        assert invalid.problem().isPresent();
        assertEqual(invalid.problem().get().toString(), "PROBLEM");
    }

    static void testGet() {
        assertEqual(valid.get(), "VALID");
        try {
            invalid.get();
            throw new RuntimeException("PROBLEM");
        } catch (ProblemException e) {
            assertEqual(e.getMessage(), "PROBLEM");
        }
    }

    private static void assertEqual(String string, String expected) {
        assert string.equals(expected) : string + " did not equal "/*IntelliJ string*/ + expected;
    }
}
