package com.sos.scheduler.engine.agent.tests.api;

import sos.spooler.Job_impl;

import java.util.HashMap;

/**
 * Created by Andreas Liebert on 10.04.2015.
 */
public class ApiJob extends Job_impl {

    public static final String LogTestPrefix = "##THIS IS ";
    public static final String LogTestSuffix = "##";

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
    public boolean spooler_process() throws Exception {
        String logLevel = spooler_task.params().value("log_level");
        String message = LogMessages.get(logLevel);
        switch (logLevel){
            case "info": spooler_log.info(message);
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

    
}
