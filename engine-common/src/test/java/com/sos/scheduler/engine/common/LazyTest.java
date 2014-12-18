package com.sos.scheduler.engine.common;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

public final class LazyTest {
    private int i = 0;

    @Test public void testApply() throws Exception {
        Lazy<Integer> lazy = new Lazy<Integer>() {
            @Override protected Integer compute() { return ++i; }
        };
        assertThat(lazy.get(), equalTo(1));
        assertThat(lazy.get(), equalTo(1));
    }
}
