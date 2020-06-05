if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

cd
export PATH="$HOME/bin:/opt/jobscheduler/bin:$JAVA_HOME/bin:$PATH"
