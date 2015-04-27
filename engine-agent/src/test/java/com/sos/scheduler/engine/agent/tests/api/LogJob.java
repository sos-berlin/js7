package com.sos.scheduler.engine.agent.tests.api;

import com.google.common.base.Strings;
import java.io.File;
import sos.spooler.Job_impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import static com.sos.scheduler.engine.agent.tests.api.SchedulerAPIIT.TestTextFilename;

/**
 * Created by Andreas Liebert on 10.04.2015.
 */
public class LogJob extends Job_impl {

    public static final String LogTestPrefix = "##THIS IS ";
    public static final String LogTestSuffix = "##";
    public static final String SpoolerInitMessage = LogTestPrefix+"spooler_init"+LogTestSuffix;
    public static final String SpoolerOpenMessage = LogTestPrefix+"spooler_open"+LogTestSuffix;
    public static final String SpoolerCloseMessage = LogTestPrefix+"spooler_close"+LogTestSuffix;
    public static final String SpoolerExitMessage = LogTestPrefix+"spooler_exit"+LogTestSuffix;

    public static final HashMap<String,String> LogMessages = new HashMap<String,String>(){
        {
            add("info");
            add("warn");
            add("debug");

            for (int i=2; i<=9; i++){
                add("debug"+i);
            }

            //add("error");
        }

        private void add(String logLevel){
            put (logLevel, LogTestPrefix+logLevel+LogTestSuffix);
        }
    };


    @Override
    public boolean spooler_init() throws Exception {
        spooler_log.info(SpoolerInitMessage);
        return true;
    }

    @Override
    public boolean spooler_open() throws Exception {
        spooler_log.info(SpoolerOpenMessage);
        return true;
    }

    @Override
    public void spooler_close() throws Exception {
        spooler_log.info(SpoolerCloseMessage);
    }

    @Override
    public void spooler_exit() throws Exception {
        spooler_log.info(SpoolerExitMessage);
    }

    @Override
    public boolean spooler_process() throws Exception {
        String logLevel = spooler_task.params().value("log_level");
        if (Strings.isNullOrEmpty(logLevel)){
            logLevel="info";
        }
        String message = LogMessages.get(logLevel);
        switch (logLevel){
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

        return (spooler_task.order() != null);
    }

    private void testOtherLogFunctions() throws Exception {
        if (Strings.isNullOrEmpty(spooler_log.filename())){
            throw new Exception("spooler_log.filename() is empty");
        }
        String testString = "034uffr348";
        spooler_log.info(testString);
        String last = spooler_log.last("info");
        if (!testString.equals(last)){
            throw new Exception("spooler_log.last returns unexpected result");
        }
        spooler_log.set_level(spooler_log.level());
        spooler_log.log_file(new File(spooler_job.configuration_directory(), TestTextFilename()));
        spooler_log.set_mail_on_error(spooler_log.mail_on_error());
        spooler_log.set_mail_on_process(spooler_log.mail_on_process());
        spooler_log.set_mail_on_success(spooler_log.mail_on_success());
        spooler_log.set_mail_on_warning(spooler_log.mail_on_warning());

        //TODO Fails: spooler_log.start_new_file();
    }


}
