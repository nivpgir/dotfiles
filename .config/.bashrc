# ~/.bashrc

source $PIAMH_CONF_DIR/session-env.sh
case $- in
    *i*) source $PIAMH_CONF_DIR/interactive-bash-session.sh
	 ;;
esac
