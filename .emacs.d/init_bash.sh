# Reset the prompt for remote TRAMP shells.
if [ "${INSIDE_EMACS/*tramp*/tramp}" == "tramp" ] ; then
#   PS1="[\u@\h \w]$ "
   PS1="[$(echo $PS1 | cut -d':' -f 1-2 ):\w]$ "
fi
