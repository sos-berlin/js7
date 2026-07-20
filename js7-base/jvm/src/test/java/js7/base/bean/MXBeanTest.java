package js7.base.bean;

import java.lang.management.ManagementFactory;
import java.math.BigDecimal;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.LongAdder;
import javax.management.MBeanServer;
import javax.management.ObjectName;

final class MXBeanTest {

    private static final MBeanServer beanServer = ManagementFactory.getPlatformMBeanServer();

    static void main(String[] args) throws Exception {
        var objectName = new ObjectName("joc:type=MyMXBean");
        var myBean = new MyMXBeanImpl();
        beanServer.registerMBean(myBean, objectName);
        try {
            myBean.count = 7;
            myBean.cpuTime = new BigDecimal("1.23");
            myBean.threadSafeCounter.incrementAndGet();
            myBean.veryOftenUsedThreadSafeCounter.increment();
            test(objectName);
        } finally {
            beanServer.unregisterMBean(objectName);
        }
    }

    private static void test(ObjectName objectName) throws Exception {
        var count = (Integer)beanServer.getAttribute(objectName, "Count");
        var cpuTime = (BigDecimal)beanServer.getAttribute(objectName, "CpuTime");
        var threadSafeCounter = (Long)beanServer.getAttribute(objectName, "ThreadSafeCounter");
        var veryOftenUsedThreadSafeCounter =
            (Long)beanServer.getAttribute(objectName, "VeryOftenUsedThreadSafeCounter");
        if (count == 7
            && cpuTime.equals(new BigDecimal("1.23"))
            && threadSafeCounter == 1
            && veryOftenUsedThreadSafeCounter == 1)
            System.out.println("Test succeeded");
        else
            throw new AssertionError("Test failed");
    }


    /** An MXBean requires an interface with get-methods.
      * Prometheus accepts numeric values and will not accept Strings.
      */
    public interface MyMXBean {
        int getCount();
        long getThreadSafeCounter();
        long getVeryOftenUsedThreadSafeCounter();

        /** CPU time in seconds (not milliseconds), as Prometheus requires. */
        BigDecimal getCpuTime();
    }


    /** An MXBean requires an implememtation which implements the MXBean interface.
      * The getters should be fast.
      */
    private static final class MyMXBeanImpl implements MyMXBean {
        /** This examples leaves the variables public.
         * One may prefer encapsulating setter methods.
         * The application fills in the current values.
         * The values will be read in the background via its getters
         * (this is what the /metrics web service does). */
        int count = 0;
        AtomicLong threadSafeCounter = new AtomicLong();
        LongAdder veryOftenUsedThreadSafeCounter = new LongAdder();
        BigDecimal cpuTime = new BigDecimal(0);

        @Override
        public int getCount() {
            return count;
        }

        @Override
        public long getThreadSafeCounter() {
            return threadSafeCounter.longValue();
        }

        @Override
        public long getVeryOftenUsedThreadSafeCounter() {
            return veryOftenUsedThreadSafeCounter.longValue();
        }

        @Override
        public BigDecimal getCpuTime() {
            return cpuTime;
        }
    }
}
