FROM openjdk:10-jre-slim

# UID of the 'jobscheduler'. The mounted volume /var/opt/jobscheduler must be rwx for this UID
# Used as group ID, too.
ARG UID

EXPOSE 4444 4445
CMD "/bin/bash"

ADD files /
ADD jobscheduler-install-[1-9]*.tgz /opt/

RUN chown root:root -R /opt/jobscheduler-[1-9]* && \
    chmod o-w -R /opt/jobscheduler-[1-9]* && \
    ln -s /opt/jobscheduler-[1-9]* /opt/jobscheduler && \
    addgroup --gid ${UID:-1000} jobscheduler && \
    yes | adduser --uid ${UID:-1000} --ingroup jobscheduler --home /home/jobscheduler --no-create-home --shell /bin/bash --disabled-password jobscheduler && \
    mkdir -p /var/opt/jobscheduler/testMasterAgent && \
    chown jobscheduler:jobscheduler -R /home/jobscheduler /var/opt/jobscheduler

# ~jobscheduler/bin contains scripts master and agent, useable for "docker run".
ENV PATH /home/jobscheduler/bin:/usr/local/bin:/usr/bin:/bin
USER jobscheduler

# Possible start:
# docker run --publish=4444:4444 -ti --tag=jobscheduler-agent jobscheduler agent
