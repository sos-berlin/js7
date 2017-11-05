if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

cd
export PATH="$HOME/bin:$JAVA_HOME/bin:$PATH:"
