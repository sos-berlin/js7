package com.sos.scheduler.engine.common.time;

import org.junit.Test;

import java.util.Date;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.closeTo;

public final class LoadMeterTest {
    private static final double epsilon = 0.0001;
    
    @Test public void testMeter() {
        int beatDuration = 1000;
        int beatCount = 10;
        LoadMeter m = new LoadMeter(beatDuration, beatCount);
        long t = new Date().getTime() / beatDuration * beatDuration;
        m.meter(0, t);
        m.meter(1, t+100);
        m.meter(0, t+100+900);                      // 1. Sekunde: 0.1
        m.meter(1, t+100+900+2000);                 // 2. und 3. Sekunde: 1
        m.meter(0, t+100+900+2000+1750);            // 4. Sekunde: 0
        m.meter(1, t+100+900+2000+1750+750);        // 5. Sekunde: 0.25
        m.meter(0, t+100+900+2000+1750+750+500);    // 6. Sekunde: 0.5

        float[] expected = { 0.1f, 1, 1, 0, 0.25f, 0.5f };
        int n = expected.length;
        
        List<Float> h = m.getHistory();
        for (int i = 0; i < n; i++) assertThat((double)h.get(n - 1 - i), closeTo(expected[i], epsilon));

        float sum = 0;
        for (int i = 1; i <= n; i++) {
            sum += expected[n-i];
            assertThat((double)m.loadOfLastPeriods(i), closeTo(sum/i, epsilon));
        }
    }
}