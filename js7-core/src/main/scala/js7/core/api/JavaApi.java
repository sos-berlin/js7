package js7.core.api;

public class JavaApi
{
    public String quoteString(String string) {
        return Api$.MODULE$.quoteString(string);
    }
}
