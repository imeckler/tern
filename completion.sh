_tern_completion() {
  PROJECTS=( $( ls -1 ~/.tern/projects ) )

  if [ "${#COMP_WORDS[@]}" -lt "3" ]; then
    COMPREPLY=($(compgen -W "new update complete ""$(printf "'%s' " "${PROJECTS[@]}")" --  "${COMP_WORDS[1]}"))
    #echo $COMPREPLY
    return
  elif [ "${#COMP_WORDS[@]}" == "3" ]; then
    COMPREPLY=($(compgen -W "$(printf "'%s' " "${PROJECTS[@]}")" --  "${COMP_WORDS[2]}"))
    return
  fi
}

complete -F _tern_completion tern
