package com.sos.scheduler.engine.data.order;

public enum OrderStateTransition {
    error(0),
    success(1),
    keepState(2);

    private final int cppCode;

    OrderStateTransition(int cppCode) {
        this.cppCode = cppCode;
    }

    public static OrderStateTransition ofCppCode(int cppCode) {
        for (OrderStateTransition o: values())
            if (o.cppCode == cppCode) return o;
        throw new RuntimeException("Unknown OrderStateTransition.cppCode: "+cppCode);
    }
}
