package com.sos.scheduler.engine.common.log;

import org.slf4j.bridge.SLF4JBridgeHandler;

import java.util.logging.Handler;
import java.util.logging.LogManager;
import java.util.logging.Logger;

public final class LoggingFunctions {
    public static void enableJavaUtilLoggingOverSLF4J() {
        disableJavaUtilLogging();
        SLF4JBridgeHandler.install();
    }

    public static void disableJavaUtilLogging() {
        Logger rootLogger = LogManager.getLogManager().getLogger("");
        for (Handler h: rootLogger.getHandlers())
            rootLogger.removeHandler(h);
    }

    private LoggingFunctions() {}
}
