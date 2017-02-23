package sos.spooler;

@SuppressWarnings("rawtypes")
public interface HasBean<B extends Bean> {
    B toBean();
}
