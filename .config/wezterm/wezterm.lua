
local wezterm = require 'wezterm';
return {
  font = wezterm.font("Roboto Mono"),
  hide_tab_bar_if_only_one_tab = true,
  default_prog = { "bash", "--login", "-i" }
}
