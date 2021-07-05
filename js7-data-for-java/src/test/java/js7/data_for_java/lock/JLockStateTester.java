package js7.data_for_java.lock;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import js7.data.order.OrderId;
import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

class JLockStateTester
{
    private JLockStateTester() {}

    static void testAvailable(JLockState lockState) {
        assertThat(lockState.isAvailable(), equalTo(true));
    }

    static void testNotAvailable(JLockState lockState) {
        assertThat(lockState.isAvailable(), equalTo(false));
    }

    static void testOrderIds(JLockState lockState) {
        Collection<OrderId> orderIds = lockState.orderIds();
        assertThat(new ArrayList<>(orderIds), equalTo(asList(OrderId.of("A"))));
    }

    static void testQueuedOrderIds(JLockState lockState) {
        List<OrderId> orderIds = lockState.queuedOrderIds();
        assertThat(orderIds, equalTo(asList(OrderId.of("Q"))));
    }
}
