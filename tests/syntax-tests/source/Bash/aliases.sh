#!/usr/bin/env bash

alias mv='mv -v'
alias -- -='cd -'
alias -p
alias -p -- -='cd -' ll='ls -l'
alias -- plus+='printf plus' dot.name='printf dot'
alias first='printf first' second='printf second'
alias -- -
alias -- -='cd -'; printf '%s\n' done

# Ordinary assignment operators keep their existing meaning.
count+=1
printf '%s\n' 'alias -- -= is inside a string'
