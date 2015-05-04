package com.sos.scheduler.engine.agent.tests.api;

import com.google.common.base.Strings;
import java.util.HashMap;
import java.util.Map;
import sos.spooler.Job_impl;

/**
 * @author Andreas Liebert
 */
public final class LogJob extends Job_impl {

    public static final String LogTestPrefix = "##THIS IS ";
    public static final String LogTestSuffix = "##";
    public static final String SpoolerInitMessage = LogTestPrefix + "spooler_init" + LogTestSuffix;
    public static final String SpoolerOpenMessage = LogTestPrefix + "spooler_open" + LogTestSuffix;
    public static final String SpoolerCloseMessage = LogTestPrefix + "spooler_close" + LogTestSuffix;
    public static final String SpoolerExitMessage = LogTestPrefix + "spooler_exit" + LogTestSuffix;

    public static final Map<String,String> LogMessages = new HashMap<String,String>() {
        {
            add("info");
            add("warn");
            add("debug");
            //add("error");
            for (int i = 2; i <= 9; i++) {
                add("debug" + i);
            }

        }

        private void add(String logLevel){
            put(logLevel, LogTestPrefix + logLevel + LogTestSuffix);
        }
    };


    @Override
    public boolean spooler_init() {
        spooler_log.info(SpoolerInitMessage);
        return true;
    }

    @Override
    public boolean spooler_open() {
        spooler_log.info(SpoolerOpenMessage);
        return true;
    }

    @Override
    public void spooler_close() {
        spooler_log.info(SpoolerCloseMessage);
    }

    @Override
    public void spooler_exit() {
        spooler_log.info(SpoolerExitMessage);
    }

    @Override
    public boolean spooler_process() {
        String logLevel = spooler_task.params().value("log_level");
        if (Strings.isNullOrEmpty(logLevel)) {
            for(String level : LogMessages.keySet()){
                printLogMessage(level);
            }
        }
        else{
            printLogMessage(logLevel);
        }
        return spooler_task.order() != null;
    }

    private void printLogMessage(String logLevel){
        String message = LogMessages.get(logLevel);
        switch (logLevel) {
            case "info": spooler_log.info(message);
                testOtherLogFunctions();
                break;
            case "warn": spooler_log.warn(message);
                break;
            case "error": spooler_log.error(message);
                break;
            case "debug": spooler_log.debug(message);
                break;
            case "debug2": spooler_log.debug2(message);
                break;
            case "debug3": spooler_log.debug3(message);
                break;
            case "debug4": spooler_log.debug4(message);
                break;
            case "debug5": spooler_log.debug5(message);
                break;
            case "debug6": spooler_log.debug6(message);
                break;
            case "debug7": spooler_log.debug7(message);
                break;
            case "debug8": spooler_log.debug8(message);
                break;
            case "debug9": spooler_log.debug9(message);
                break;
        }
    }

    private void testOtherLogFunctions() {
        if (Strings.isNullOrEmpty(spooler_log.filename())){
            throw new RuntimeException("spooler_log.filename() is empty");
        }
        String testString = "034uffr348";
        spooler_log.info(testString);
        String last = spooler_log.last("info");
        if (!testString.equals(last)) {
            throw new RuntimeException("spooler_log.last returns unexpected result");
        }
        spooler_log.set_level(spooler_log.level());
        spooler_log.set_mail_on_error(spooler_log.mail_on_error());
        spooler_log.set_mail_on_process(spooler_log.mail_on_process());
        spooler_log.set_mail_on_success(spooler_log.mail_on_success());
        spooler_log.set_mail_on_warning(spooler_log.mail_on_warning());
        //TODO Fails: spooler_log.start_new_file();
    }
}
