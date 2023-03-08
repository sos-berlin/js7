if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

cd
export PATH="$HOME/bin:/opt/js7/bin:$JAVA_HOME/bin:$PATH"
export LESS="-RS"
