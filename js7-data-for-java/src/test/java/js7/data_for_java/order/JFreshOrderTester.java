package js7.data_for_java.order;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import js7.data.order.OrderId;
import js7.data.workflow.WorkflowPath;
import js7.data_for_java.workflow.position.JLabel;
import js7.data_for_java.workflow.position.JPosition;
import js7.data_for_java.workflow.position.JPositionOrLabel;
import static java.util.Arrays.asList;
import static java.util.Collections.emptyMap;
import static js7.data_for_java.vavr.VavrUtils.getOrThrow;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

final class JFreshOrderTester {
    private JFreshOrderTester() {}

    static void test() {
        test1();
        test2();
    }

    private static void test1() {
        JFreshOrder order = JFreshOrder.of(OrderId.of("ORDER"), WorkflowPath.of("WORKFLOW"));
        assertThat(order.id(), equalTo(OrderId.of("ORDER")));
        assertThat(order.asScala().workflowPath(), equalTo(WorkflowPath.of("WORKFLOW")));
    }

    private static void test2() {
        Optional<JPositionOrLabel> startPosition =
            Optional.of(getOrThrow(JPosition.fromList(asList(1))));
        Set<JPositionOrLabel> stopPositions = new HashSet<>(Arrays.<JPositionOrLabel>asList(
            getOrThrow(JPosition.fromList(asList(1, "then", 2))),
            JLabel.of("LABEL")));
        JFreshOrder order = JFreshOrder.of(
            OrderId.of("ORDER"), WorkflowPath.of("WORKFLOW"),
            emptyMap(), Optional.empty(), false,
            startPosition, stopPositions);
        assertThat(order.startPosition(), equalTo(startPosition));
        assertThat(order.stopPositions(), equalTo(stopPositions));
    }
}
