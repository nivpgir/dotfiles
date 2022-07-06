local wzt = require 'wezterm';
local default_prog = "bash";
if string.find(wzt.target_triple, "msvc") then
  default_prog = "C:/Program Files/Git/bin/" .. default_prog
end

wzt.on("window-config-reloaded", function(window, pane)
  window:toast_notification("wezterm", "configuration reloaded!", nil, 4000)
end)

-- ssh_domains = [];
return {
  -- font = wzt.font("Roboto Mono"),
  hide_tab_bar_if_only_one_tab = true,
  default_domain = "local",
  default_prog = { default_prog, "--login", "-i" },
  -- ssh_domains = ssh_domains,
  enable_wayland = true
}
