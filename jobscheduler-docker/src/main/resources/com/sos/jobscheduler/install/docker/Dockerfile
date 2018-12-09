FROM openjdk:12

# UID of the 'jobscheduler'. The mounted volume /var/opt/jobscheduler must be rwx for this UID
# Used as group ID, too.
ARG UID

EXPOSE 4444 4445
CMD "/bin/bash"
ENV LANG=en_US.UTF-8

# Only tiny improvment:
RUN $JAVA_HOME/bin/jlink --no-header-files --no-man-pages \
    --add-modules java.base,java.compiler,java.desktop,java.logging,java.management,java.security.jgss,java.sql,java.xml,jdk.management,jdk.unsupported \
    --output /opt/jobscheduler-java
ENV JAVA_HOME /opt/jobscheduler-java

ADD files /
ADD jobscheduler-install-[1-9]*.tgz /opt/

RUN chown root:root -R /opt/jobscheduler-[1-9]* && \
    chmod o-w -R /opt/jobscheduler-[1-9]* && \
    ln -s /opt/jobscheduler-[1-9]* /opt/jobscheduler && \
    groupadd --gid ${UID:-1000} jobscheduler && \
    useradd --uid ${UID:-1000} --gid jobscheduler --home-dir /home/jobscheduler --no-create-home --shell /bin/bash jobscheduler && \
    mkdir -p /var/opt/jobscheduler/testMasterAgent && \
    chown jobscheduler:jobscheduler -R /home/jobscheduler /var/opt/jobscheduler

# ~jobscheduler/bin contains scripts master and agent, useable for "docker run".
ENV PATH /home/jobscheduler/bin:/usr/local/bin:/usr/bin:/bin
USER jobscheduler

# Possible start:
# docker run --publish=4444:4444 -ti --tag=jobscheduler-agent jobscheduler agent
