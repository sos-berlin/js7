FROM almalinux:8.5

ENV LANG C.UTF-8

RUN \
  sbtVersion=1.6.2 &&\
  \
  : yum -y update `# may take several minutes` &&\
  yum -y install git unzip which sudo openssl nodejs java-1.8.0-openjdk-devel &&\
  yum clean all &&\
  \
  : chmod u+s /bin/ping &&\
  \
  mkdir --parents /opt/lib/sbt-$sbtVersion &&\
  curl --location https://github.com/sbt/sbt/releases/download/v$sbtVersion/sbt-$sbtVersion.tgz |\
    tar xzf - --directory=/opt/lib/sbt-$sbtVersion --strip-components=1 sbt/ &&\
  ln -snf /opt/lib/sbt-$sbtVersion/bin/sbt /usr/local/bin/sbt

RUN \
  adduser --comment "JobScheduler build" --no-create-home build &&\
  mkdir --parents /home/build/mount/{.cache,.ivy2,.npm,.sbt} &&\
  ln -s /home/build/mount/{.cache,.ivy2,.npm,.sbt} /home/build/ &&\
  chown -R build.build /home/build/ &&\
  : echo "build ALL=(ALL) NOPASSWD: ALL" >>/etc/sudoers

USER build
WORKDIR /home/build/js7
