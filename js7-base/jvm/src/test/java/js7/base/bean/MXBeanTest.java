package js7.base.bean;

import java.lang.management.ManagementFactory;
import java.math.BigDecimal;
import java.util.concurrent.atomic.AtomicLong;
import javax.management.MBeanServer;
import javax.management.ObjectName;

final class MXBeanTest {

    private static final MBeanServer beanServer = ManagementFactory.getPlatformMBeanServer();

    static void main(String[] args) throws Exception {
        testSimple();
        test2();
    }

    private static void testSimple() throws Exception {
        // Register the bean once
        var objectName = new ObjectName("joc:type=SimpleMXBean");
        var myBean = new SimpleBean();
        beanServer.registerMBean(myBean, objectName);
        try {
            // Read the value like the /metrics web service would see it (test only)
            var count = (Integer)beanServer.getAttribute(objectName, "Count");
            if (count != 7) throw new AssertionError("Count is not 7");
            System.out.println("testSimple succeeded");
        } finally {
            // When your application is done with the bean, unregister it
            beanServer.unregisterMBean(objectName);
        }
    }

    private static void test2() throws Exception {
        // Register the bean once
        var objectName = new ObjectName("joc:type=TestMXBean");
        var myBean = new TestBean();
        beanServer.registerMBean(myBean, objectName);
        try {
            // Your application updates the values
            myBean.count = 7;
            myBean.cpuTime = new BigDecimal("1.23");
            myBean.counter.incrementAndGet();

            // Check the values like the /metrics web service would see them (test only)
            test2check(objectName);
        } finally {
            // When your application is done with the bean, unregister it
            beanServer.unregisterMBean(objectName);
        }
    }

    private static void test2check(ObjectName objectName) throws Exception {
        var count = (Integer)beanServer.getAttribute(objectName, "Count");
        var cpuTime = (BigDecimal)beanServer.getAttribute(objectName, "CpuTime");
        var counter = (Long)beanServer.getAttribute(objectName, "Counter");
        if (count == 7
            && cpuTime.equals(new BigDecimal("1.23"))
            && counter == 1)
            System.out.println("test1 succeeded");
        else
            throw new AssertionError("Test failed");
    }


    /** An MXBean requires an interface with get-methods.
      * Prometheus accepts numeric values and will not accept Strings.
      */
    public interface SimpleMXBean {
        int getCount();
    }

    /** An MXBean requires an implememtation which implements the corresponding MXBean interface.
      * The getters should be fast.
      */
    private static final class SimpleBean implements SimpleMXBean {
        @Override
        public int getCount() {
            return 7;
        }
    }

    /** An MXBean requires an interface with get-methods.
      * Prometheus accepts numeric values and will not accept Strings.
      */
    public interface TestMXBean {
        int getCount();
        long getCounter();

        /** CPU time in seconds (not milliseconds), as Prometheus requires. */
        BigDecimal getCpuTime();
    }


    /** An MXBean requires an implememtation which implements the corresponding MXBean interface.
      * The getters should be fast.
      */
    public static final class TestBean implements TestMXBean {
        /** This examples leaves the variables public.
         * One may prefer encapsulating setter methods.
         * The application fills in the current values.
         * The values will be read in the background via its getters
         * (this is what the /metrics web service does). */
        int count = 0;
        AtomicLong counter = new AtomicLong();
        BigDecimal cpuTime = new BigDecimal(0);

        @Override
        public int getCount() {
            return count;
        }

        @Override
        public long getCounter() {
            return counter.longValue();
        }

        @Override
        public BigDecimal getCpuTime() {
            return cpuTime;
        }
    }
}
