package js7.data_for_java.order;

import java.util.function.Predicate;
import js7.data.order.Order;
import static js7.data_for_java.common.JPredicates.toScalaPredicate;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

/**
 * @author Joacim Zschimmer
 */
public final class JOrderPredicatesTester
{
    private final Order<Order.State> aOrder;
    private final Order<Order.State> bOrder;

    JOrderPredicatesTester(Order<Order.State> aOrder, Order<Order.State> bOrder) {
        this.aOrder = aOrder;
        this.bOrder = bOrder;
    }

    void test() {
        testJavaPredicate1();
        testJavaPredicate2();
    }

    private void testJavaPredicate1() {
        Predicate<Order<Order.State>> predicate = order -> order.id() == aOrder.id();
        assertThat(toScalaPredicate(predicate).apply(aOrder), equalTo(true));
        assertThat(toScalaPredicate(predicate).apply(bOrder), equalTo(false));
    }

    private void testJavaPredicate2() {
        assertThat(toScalaPredicate(this::isMyOrder).apply(aOrder), equalTo(true));
        assertThat(toScalaPredicate(this::isMyOrder).apply(bOrder), equalTo(false));
    }

    private boolean isMyOrder(Order<Order.State> order) {
        return order.id() == aOrder.id();
    }
}
