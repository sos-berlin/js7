package js7.data_for_java.schedule;

import java.time.Duration;
import java.time.Instant;
import java.time.ZoneId;
import java.util.List;
import java.util.stream.Stream;
import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;
import static js7.data_for_java.vavr.VavrUtils.getOrThrow;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

final class JScheduleSimulatorTester
{
    private JScheduleSimulatorTester() {}

    static void testScheduleSimulator(String scheduleJson) {
        JSchedule schedule = getOrThrow(
            JSchedule.fromJson(scheduleJson));

        JScheduleCalculator calculator = getOrThrow(
            JScheduleCalculator.checked(
                schedule,
                ZoneId.of("UTC"),
                Duration.ZERO/*dateOffset, the shift of the businessday*/));

        Stream<JScheduleSimulator.Scheduled> result = calculator.simulate(
            JTimeInterval.of(
                Instant.parse("2021-10-04T00:00:00Z"),
                Duration.ofHours(24)),
            Duration.ZERO/*Duration of a cycle*/);

        List<Instant> starts = result
            .map(o -> o.start())
            .collect(toList());
        assertThat(starts, equalTo(asList(
            Instant.parse("2021-10-04T09:10:00Z"),
            Instant.parse("2021-10-04T09:15:00Z"),
            Instant.parse("2021-10-04T09:20:00Z"),
            Instant.parse("2021-10-04T10:10:00Z"),
            Instant.parse("2021-10-04T10:15:00Z"))));
    }
}
