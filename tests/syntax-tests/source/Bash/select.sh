#!/usr/bin/env bash
options="one two"
select action in $options; do
    case "$action" in
        one) echo first; ;;
        two) echo second; ;;
        *) break; ;;
    esac
done

select choice; do
    printf '%s\n' "$choice"
    break
done

# These words are arguments, strings or parts of longer names.
printf '%s\n' select in "select in"
selection=select
selecting action in options
