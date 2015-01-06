package com.sos.scheduler.engine.data.log;

import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.sos.scheduler.engine.data.event.AbstractEvent;
import com.sos.scheduler.engine.data.message.MessageCode;
import scala.Option;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.annotation.Nullable;

public class LogEvent extends AbstractEvent {
    private static final Pattern codePattern = Pattern.compile("([A-Z]+(-[0-9A-Z]+)+)( .*)?", Pattern.DOTALL);

    private final SchedulerLogLevel level;
    private final String line;

    protected LogEvent(SchedulerLogLevel level, String message) {
        this.level = level;
        this.line = message;
    }

    public final Option<MessageCode> codeOption() {
        String code = getCodeOrNull();
        return code == null? Option.<MessageCode>apply(null) : Option.apply(new MessageCode(code));
    }

    @JsonIgnore @Nullable
    public final String getCodeOrNull() {
        return messageCodeFromLineOrNull(line);
    }

    public final SchedulerLogLevel level() {
        return level;
    }

    @JsonGetter
    public final String message() {
        return line;
    }

    @Override public String toString() {
        return super.toString() +", line="+ line;
    }

    @Nullable static String messageCodeFromLineOrNull(String line) {
        Matcher m = codePattern.matcher(line);
        return m.matches()? m.group(1) : null;
    }

    public static LogEvent of(SchedulerLogLevel level, String line) {
        switch (level) {
            case info: return new InfoLogEvent(line);
            case warning: return new WarningLogEvent(line);
            case error: return new ErrorLogEvent(line);
            default: return new LogEvent(level, line);
        }
    }
}
