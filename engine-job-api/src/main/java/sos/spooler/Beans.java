package sos.spooler;

public final class Beans {
    private Beans() {}

    public static <A extends HasBean<B>, B extends Bean<A>> B toBean(A o) {
        return o == null? null : o.toBean();
    }

    public static <A extends HasBean<B>, B extends Bean<A>> A toDelegate(B o) {
        return o == null? null : o.getDelegate();
    }
}
