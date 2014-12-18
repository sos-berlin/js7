package sos.spooler;

public interface HasBean<B extends Bean> {
    B toBean();
}
