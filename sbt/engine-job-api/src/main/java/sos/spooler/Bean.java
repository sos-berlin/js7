package sos.spooler;

/** "Bean" nur in dem Sinne, dass die Getter und Setter wie Properties von einem Skript benutzt werden können -
 * ist nicht serialisierbar wie eine JavaBean. */
@SuppressWarnings("rawtypes")
public interface Bean<T extends HasBean> {
    T getDelegate();
}
