namespace sos.spooler
{
    using net.sf.jni4net.jni;
    using net.sf.jni4net.utils;

    public static class JniValueConverter
    {
        public static Value ParStrongCp2J(IJvmProxy obj)
        {
            return Convertor.ParStrongCp2J(obj);
        }

        public static Value ParStrongCp2J(string obj)
        {
            return new Value()
            {
                _object = (JniHandle)Convertor.StrongCp2J((java.lang.String)obj)
            };
        }
    }
}
