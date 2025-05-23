#!/usr/bin/env bash

TITLE=$1

yabai_winid() {
    yabai -m query --windows | jq -r \
      --arg title "$TITLE" \
      '.[] | select(.app == "kitty" and .title == $title) | .id'
}

get_terminal_cmd() {
    case "$1" in
        "scratchpad")
            echo nvim ~/scratchpad.md
            ;;
        "llm")
            echo ollama run phi3
            ;;
        *)
            echo bash
            ;;
    esac
}

run_for_yabai() {
    win_id=$(yabai_winid)

    if [ -z "$win_id" ]; then
      open -na "kitty" --args --title "$TITLE" -e `get_terminal_cmd $1`
      sleep 0.5  # Give it time to launch

      # Set it to float, center, and make sticky
      win_id=$(yabai_winid)

      if [ -n "$win_id" ]; then
        yabai -m window "$win_id" --scratchpad scratchpad
        yabai -m window "$win_id" --grid 20:100:0:0:80:8
        yabai -m window "$win_id" --scratchpad recover
      fi
    else
      # If visible, minimize it; if minimized, restore it
      is_minimized=$(yabai -m query --windows --window "$win_id" | jq -r '.["is-minimized"]')
      echo is_minimized: $is_minimized
      echo win_id: $win_id
      if [ "$is_minimized" = "true" ]; then
        yabai -m window  --deminimize "$win_id"
        yabai -m window  --focus "$win_id"
      else
        yabai -m window  --minimize "$win_id"
      fi
    fi
}

# TODO; add support for other window managers
run_for_yabai $1
