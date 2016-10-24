package sos.spooler.jobs;

import java.util.Objects;
import sos.spooler.HasBean;
import static sos.spooler.Beans.toBean;

/**
 * @author Andreas Liebert
 */
public class ScriptAdapterHelper {

    private static final String beanLanguagePrefix = "java";
    private static final String methodLanguagePrefix = "javax.script";

    private ScriptAdapterHelper() {}

    static Parameters parseLanguageParameter(String prefixedLanguageString) {
        String[] parts = prefixedLanguageString.split(":", 2);
        if (parts.length != 2) throw new RuntimeException("Invalid language='"+ prefixedLanguageString +"'");
        boolean isBeanCall = languagePrefixIsBeanCall(parts[0]);
        String language = parts[1];
        return new Parameters(language, isBeanCall);
    }

    @SuppressWarnings("unchecked")
    static Object conditionalToBean(boolean isToBean, HasBean<?> o) {
        return isToBean? toBean(o) : o;
    }
    static boolean languagePrefixIsBeanCall(String prefix) {
        if (prefix.equals(beanLanguagePrefix))
            return true;
        else
        if (prefix.startsWith(methodLanguagePrefix))
            return false;
        else
            throw new RuntimeException("Invalid language prefix '"+ prefix+ "'. '"+ beanLanguagePrefix +"' or '"+ methodLanguagePrefix + "' expected");
    }

    static final class Parameters {
        final String language;
        final boolean isUsingBean;

        Parameters(String language, boolean isBeanCall) {
            this.language = language;
            this.isUsingBean = isBeanCall;
        }

        @Override public boolean equals(Object o) {
            return language.equals(((Parameters)o).language) && isUsingBean == ((Parameters)o).isUsingBean;
        }

        @Override public int hashCode() {
            // This is to please Lint.
            return Objects.hashCode(language) + Boolean.hashCode(isUsingBean);
        }
    }
}
