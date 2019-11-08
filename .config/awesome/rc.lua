-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")
require("awful.remote")
require("screenful")


-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")

-- we need the awesome version before the hotkeys popup require and
-- this is the soonest we can get it
awesome_version = ""
version_string_cmd = [[ bash -c "
awesome -v | head -1 | awk '{print $2}'
"
]]
output = awful.spawn.easy_async(version_string_cmd, function(stdout, stderr, reason, exit_code)
				   naughty.notify { text = stdout, title = "Awesome version" }
				   naughty.notify { text = stdout:gsub("\n$", ""), title = "Awesome version" }
				   awesome_version = stdout:gsub("\n$", "")
end)


local menubar = require("menubar")

local hotkeys_popup = require("awful.hotkeys_popup")
if awesome_version == "v4.3" then
   hotkeys_popup = require("awful.hotkeys_popup").widget
end
local lain = require("lain")

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")


-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
   naughty.notify({ preset = naughty.config.presets.critical,
		    title = "Oops, there were errors during startup!",
		    text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
   local in_error = false
   awesome.connect_signal("debug::error", function (err)
			     -- Make sure we don't go into an endless error loop
			     if in_error then return end
			     in_error = true

			     naughty.notify({ preset = naughty.config.presets.critical,
					      title = "Oops, an error happened!",
					      text = tostring(err) })
			     in_error = false
   end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")

-- This is used later as the default terminal and editor to run.
-- terminal = "xterm"
terminal = "gnome-terminal" or "xterm"
editor = os.getenv("EDITOR") or "nano"
editor_cmd = terminal .. " -e " .. editor


-- setup xkb to alter mod keys for caps as mod3
xkbhomedir = os.getenv("HOME").."/.xkbconf"
awful.util.spawn("xkbcomp -I"..xkbhomedir.." "..xkbhomedir.."/keymap/custom :0")

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod3"


function do_simple(p, orientation)
   local wa = p.workarea
   local cls = p.clients
   if orientation == "vertical" then
      wa.width, wa.height = wa.height, wa.width
      wa.x, wa.y = wa.y, wa.x
   end

   if #cls > 0 then
      for k, c in ipairs(cls) do
	 local g = {}
	 g.height = wa.height
	 g.width = wa.width / #cls
	 g.y = 0
	 g.x = (k-1) * wa.width / #cls
	 g.y = g.y + wa.y
	 g.x = g.x + wa.x
	 if orientation == 'vertical' then
	    g.width, g.height = g.height, g.width
	    g.x, g.y = g.y, g.x
	 end
	 p.geometries[c] = g
      end
   end
end

local simple = {}
simple.name = "simple"
function simple.arrange(p)
   return do_simple(p, "horizontal")
end
simple.vertical = {}
simple.vertical.name = "simplev"
function simple.vertical.arrange(p)
   return do_simple(p, "vertical")
end

-- function do_stack(p)
--    local wa = p.workarea
--    local cls = p.clients
--    if #cls > 0 then
--       local top_cls = {}
--       local bot_cls = {}
--       local f
--       -- first divide all client to:
--       -- the ones which should be above the focused
--       -- the ones which should be below the focused
--       -- the focused client
--       for i, c in ipairs(cls) do
-- 	 naughty.notify{title=tostring(c.name), text=""..tostring(c).."=="..tostring(c)}
-- 	 if c == client.focus then
-- 	    f = c
-- 	    -- naughty.notify{title=tostring(c.name), text=""..tostring(c).."=="..tostring(f)}
-- 	 elseif not f then
-- 	    top_cls[#top_cls+1] = c
-- 	    -- naughty.notify{title=tostring(c.name), text=""..tostring(c).."=="..tostring(top_cls[#top_cls])}
-- 	 else
-- 	    bot_cls[#bot_cls+1] = c
-- 	    -- naughty.notify{title=tostring(c.name), text=""..tostring(c).."=="..tostring(bot_cls[#bot_cls])}
-- 	 end
--       end
--       -- next setup the top clients, offsetting each client by the
--       -- sum of the titlebar heights of of the clients before  him
--       local top_offset = 0
--       -- naughty.notify{title="top_cls",
-- 		     -- text=""..tostring(top_cls).." : "..#top_cls}
--       for i, c  in ipairs(top_cls) do
-- 	 -- naughty.notify{title="i: "..tostring(i), text="c"..tostring(c)}
-- 	 local g = {}
-- 	 local height = awful.titlebar(c).drawable:geometry().height + 5
-- 	 -- g.height = height
-- 	 g.height = height
-- 	 g.width = wa.width
-- 	 g.y = top_offset
-- 	 g.x = 0
-- 	 g.y = g.y + wa.y
-- 	 g.x = g.x + wa.x
-- 	 p.geometries[c] = g
-- 	 -- naughty.notify{title="t: "..c.name, text="("..g.x..":"..g.y..")"}
-- 	 top_offset = top_offset + height
-- 	 -- naughty.notify{title=tostring(i),
-- 	 -- 		text=tostring(c)}

--       end
--       -- now the same as what we did for the top ones,
--       -- but reversely, with the bottom ones
--       -- naughty.notify{title="bot_cls",
-- 		     -- text=""..tostring(bot_cls).." : "..#bot_cls}
--       local bot_offset = 0
--       for i = #bot_cls,1,-1 do
-- 	 local c = bot_cls[i]
-- 	 -- naughty.notify{title="i: "..tostring(i), text="c"..tostring(c)}
-- 	 local height = awful.titlebar(c).drawable:geometry().height + 5
-- 	 bot_offset = bot_offset + height
-- 	 local g = {}
-- 	 -- g.height = height
-- 	 g.height = height
-- 	 g.width = wa.width
-- 	 g.y = wa.height - bot_offset
-- 	 g.x = 0
-- 	 g.y = g.y + wa.y
-- 	 g.x = g.x + wa.x
-- 	 -- naughty.notify{title="b: "..(c.name), text="("..(g.x)..":"..(g.y)..")"}
-- 	 p.geometries[c] = g
-- 	 -- naughty.notify{title=tostring(i),
-- 	 -- 		text=tostring(c)}

--       end
--       -- lastly setup the focused window between the top and the bottom ones
--       local g = {}
--       g.height = wa.height - top_offset - bot_offset
--       g.width = wa.width
--       g.y = top_offset
--       g.x = 0
--       g.y = g.y + wa.y
--       g.x = g.x + wa.x
--       p.geometries[f] = g
--       -- naughty.notify{title="HI",text="HI"}
--       for i, v  in pairs(p.geometries) do
-- 	 -- naughty.notify{title="HI",text="HI"}
-- 	 naughty.notify{title=tostring(i),
-- 			text="<".."("..tostring(v.x)..","..tostring(v.y).."),"
-- 			   ..tostring(v.width)..","..tostring(v.height)..")"
-- 	 }
--       end
--       -- naughty.notify{title="f: "..f.name, text="("..g.x..":"..g.y..")"}
--       -- for i, v in ipairs(p.geometries) do
--       -- end
--    end
-- end

function do_stack(p)
   local wa = p.workarea
   local cls = p.clients
   if #cls > 0 then
      local top_cls = {}
      local bot_cls = {}
      local fidx
      -- first divide all client to:
      -- the ones which should be above the focused
      -- the ones which should be below the focused
      -- the focused client
      if not fidx then
	 for k, c in ipairs(cls) do
	    if c == client.focus then
	       fidx = k
	       break
	    end
	 end
      end

      -- next setup the top clients, offsetting each client by the
      -- sum of the titlebar heights of of the clients before  him
      local top_offset = 0
      for k = 1,fidx-1 do
	 local g = {}
	 local height = awful.titlebar(cls[k]).drawable:geometry().height + 5
	 p.geometries[cls[k]] = {
	    x = wa.x,
	    y = wa.y + top_offset,
	    height = height-1, --height,
	    width = wa.width
	 }
	 top_offset = top_offset + height
      end
      -- now the same as what we did for the top ones,
      -- but reversely, with the bottom ones
      local bot_offset = 0
      for k = fidx+1,#cls do
	 -- local c = bot_cls[i]
	 local height = awful.titlebar(cls[k]).drawable:geometry().height + 5
	 bot_offset = bot_offset + height
	 p.geometries[cls[k]] = {
	    height = height - 1, --height,
	    width = wa.width,
	    y = wa.y + wa.height - bot_offset,
	    x = wa.x,
	 }
      end
      -- lastly setup the focused window between the top and the bottom ones
      p.geometries[cls[fidx]] = {
      height = wa.height - top_offset - bot_offset,
      width = wa.width,
      y = wa.y + top_offset,
      x = wa.x
      }
      for i, v  in pairs(p.geometries) do
	 -- naughty.notify{title=tostring(i),
	 -- 		text="<".."("..tostring(v.x)..","..tostring(v.y).."),"
	 -- 		   ..tostring(v.width)..","..tostring(v.height)..")"
	 -- }
      end
   end
end

local stack = {}
stack.name = "stack"
function stack.arrange(p)
   return do_stack(p)
end

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
   -- awful.layout.suit.floating,
   simple.vertical,
   simple,
   lain.layout.termfair.center,
   awful.layout.suit.max,
   awful.layout.suit.tile.left,
   lain.layout.centerwork,
   -- lain.layout.centerwork.horizontal,
   -- awful.layout.suit.tile,
   -- awful.layout.suit.tile.bottom,
   -- awful.layout.suit.tile.top,
   -- awful.layout.suit.fair,
   -- awful.layout.suit.fair.horizontal,
   -- awful.layout.suit.spiral,
   -- awful.layout.suit.spiral.dwindle,
   -- awful.layout.suit.max.fullscreen,
   -- awful.layout.suit.magnifier,
   -- awful.layout.suit.corner.nw,
   -- awful.layout.suit.corner.ne,
   -- awful.layout.suit.corner.sw,
   -- awful.layout.suit.corner.se,
   stack,
}
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
   { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
			     { "open terminal", terminal }
}
		       })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
				     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a textclock widget
mytextclock = wibox.widget.textclock()

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
   awful.button({ }, 1, function(t) t:view_only() end),
   awful.button({ modkey }, 1, function(t)
	 if client.focus then
	    client.focus:move_to_tag(t)
	 end
   end),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ modkey }, 3, function(t)
	 if client.focus then
	    client.focus:toggle_tag(t)
	 end
   end),
   awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
   awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

local tasklist_buttons = gears.table.join(
   awful.button({ }, 1, function (c)
	 if c == client.focus then
	    c.minimized = true
	 else
	    c:emit_signal(
	       "request::activate",
	       "tasklist",
	       {raise = true}
	    )
	 end
   end),
   awful.button({ }, 3, function()
	 awful.menu.client_list({ theme = { width = 250 } })
   end),
   awful.button({ }, 4, function ()
	 awful.client.focus.byidx(1)
   end),
   awful.button({ }, 5, function ()
	 awful.client.focus.byidx(-1)
end))

local function set_wallpaper(s)
   -- Wallpaper
   if beautiful.wallpaper then
      local wallpaper = beautiful.wallpaper
      -- If wallpaper is a function, call it with the screen
      if type(wallpaper) == "function" then
	 wallpaper = wallpaper(s)
      end
      gears.wallpaper.maximized(wallpaper, s, true)
   end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

local brightness_widget = require("awesome-wm-widgets.brightnessarc-widget.brightnessarc")
local battery_widget = require("awesome-wm-widgets.batteryarc-widget.batteryarc")
local volume_widget = require("awesome-wm-widgets.volumebar-widget.volumebar")
awful.screen.connect_for_each_screen(function(s)
      -- Wallpaper
      set_wallpaper(s)

      -- Each screen has its own tag table.
      awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])

      -- Create a promptbox for each screen
      s.mypromptbox = awful.widget.prompt()
      -- Create an imagebox widget which will contain an icon indicating which layout we're using.
      -- We need one layoutbox per screen.
      s.mylayoutbox = awful.widget.layoutbox(s)
      s.mylayoutbox:buttons(gears.table.join(
			       awful.button({ }, 1, function () awful.layout.inc( 1) end),
			       awful.button({ }, 3, function () awful.layout.inc(-1) end),
			       awful.button({ }, 4, function () awful.layout.inc( 1) end),
			       awful.button({ }, 5, function () awful.layout.inc(-1) end)))
      -- Create a taglist widget
      if awesome_version == "v4.3" then
	 s.mytaglist = awful.widget.taglist {
	    screen  = s,
	    filter  = awful.widget.taglist.filter.all,
	    buttons = taglist_buttons
	 }

	 -- Create a tasklist widget
	 s.mytasklist = awful.widget.tasklist {
	    screen  = s,
	    filter  = awful.widget.tasklist.filter.currenttags,
	    buttons = tasklist_buttons
	 }
      else
	 s.mytaglist = awful.widget.taglist (s, awful.widget.taglist.filter.all, taglist_buttons)
	 s.mytasklist = awful.widget.tasklist (s, awful.widget.tasklist.filter.currenttags, tasklist_buttons)
      end

      -- Create the wibox
      s.mywibox = awful.wibar({ position = "top", screen = s })

      -- Add widgets to the wibox
      s.mywibox:setup {
	 layout = wibox.layout.align.horizontal,
	 { -- Left widgets
	    layout = wibox.layout.fixed.horizontal,
	    mylauncher,
	    s.mytaglist,
	    s.mypromptbox,
	 },
	 s.mytasklist, -- Middle widget
	 { -- Right widgets
	    layout = wibox.layout.fixed.horizontal,
	    mykeyboardlayout,
	    wibox.widget.systray(),
	    brightness_widget,
	    battery_widget,
	    volume_widget,
	    mytextclock,
	    s.mylayoutbox,
	 },
      }
end)
-- }}}

-- {{{ Mouse bindings
root.buttons(gears.table.join(
		awful.button({ }, 3, function () mymainmenu:toggle() end),
		awful.button({ }, 4, awful.tag.viewnext),
		awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = gears.table.join(
   awful.key({}, "XF86MonBrightnessUp", function () awful.spawn("light -A 5") end,
      {description = "increase brightness", group = "custom"}),
   awful.key({}, "XF86MonBrightnessDown", function () awful.spawn("light -U 5") end,
      {description = "decrease brightness", group = "custom"}),
   awful.key({ modkey, }, "F1", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end,
      {description = "help"}),
   awful.key({ modkey, }, "Left",   awful.tag.viewprev),
   awful.key({ modkey, }, "k", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end,
      { description = "show keybinds"}),
   awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
      {description = "go back", group = "tag"}),

   -- Layout manipulation
   awful.key({ modkey, "Shift" }, "w", function ()
	 awful.client.swap.global_bydirection(  "up")
				       end,
      {description = "swap with client above", group = "client"}),
   awful.key({ modkey, "Shift" }, "a", function () awful.client.swap.global_bydirection(  "left")    end,
      {description = "swap with client to the left", group = "client"}),
   awful.key({ modkey, "Shift" }, "s", function () awful.client.swap.global_bydirection(  "down")    end,
      {description = "swap with client below", group = "client"}),
   awful.key({ modkey, "Shift" }, "d", function () awful.client.swap.global_bydirection(  "right")    end,
      {description = "swap with client to the right", group = "client"}),
   awful.key({ modkey,         }, "w",
      function ()
	 awful.client.focus.global_bydirection( "up")
	 client.focus:emit_signal("request::activate","client.focus.global_bydirection", {raise=true})
      end,
      {description = "focus client above", group = "client"}),
   awful.key({ modkey,         }, "a",
      function ()
	 awful.client.focus.global_bydirection( "left")
	 client.focus:emit_signal("request::activate","client.focus.global_bydirection", {raise=true})
      end,
      {description = "focus client to the left", group = "client"}),
   awful.key({ modkey,         }, "s",
      function ()
	 awful.client.focus.global_bydirection( "down")
	 client.focus:emit_signal("request::activate","client.focus.global_bydirection", {raise=true})
      end,
      {description = "focus client below", group = "client"}),
   awful.key({ modkey,         }, "d",
      function ()
	 awful.client.focus.global_bydirection( "right")
	 client.focus:emit_signal("request::activate","client.focus.global_bydirection", {raise=true})
      end,
      {description = "focus client to the right", group = "client"}),
   awful.key({ modkey,         }, "e", function () awful.client.focus.byidx(1) end,
      {description = "focus the next client by index", group = "client"}),
   awful.key({ modkey,         }, "q", function () awful.client.focus.byidx(-1) end,
      {description = "focus the previous client by index", group = "client"}),
   awful.key({ modkey, "Shift" }, "e", function () awful.client.swap.byidx(1) end,
      {description = "swap the next client by index", group = "client"}),
   awful.key({ modkey, "Shift"}, "q", function () awful.client.swap.byidx(-1) end,
      {description = "swap the previous client by index", group = "client"}),

   awful.key({ modkey,         }, "u", awful.client.urgent.jumpto,
      {description = "jump to urgent client", group = "client"}),
   awful.key({ modkey,         }, "Tab",
      function ()
	 awful.client.focus.history.previous()
	 if client.focus then
	    client.focus:raise()
	 end
      end,
      {description = "go back", group = "client"}),

   awful.key({ modkey, }, "x",
      function () awful.screen.focus_bydirection("up", awful.screen.focused()) end,
      {description = "focus to screen above", group = "screen"}),
   awful.key({ modkey, }, "z",
      function () awful.screen.focus_bydirection("left", awful.screen.focused()) end,
      {description = "focus to screen to the left", group = "screen"}),
   awful.key({ modkey, }, "c",
      function () awful.screen.focus_bydirection("right", awful.screen.focused()) end,
      {description = "focus to screen to the right", group = "screen"}),

   awful.key({ modkey, "Shift", }, "x",
      function (c)
	 if awesome_version == "v4.3" then
	    local c = client.focus
	    if c then
	       c:move_to_screen(awful.screen.focused():get_next_in_direction("up"))
	    end
	 else
	    awful.client.movetoscreen(c, awful.screen.focused():get_next_in_direction("up"))
	 end
      end,
      {description = "focus to screen above", group = "screen"}),

   awful.key({ modkey, "Shift", }, "z",
      function (c)
	 if awesome_version == "v4.3" then
	    local c = client.focus
	    if c then
	       c:move_to_screen(awful.screen.focused():get_next_in_direction("left"))
	    end
	 else
	    awful.client.movetoscreen(c, awful.screen.focused():get_next_in_direction("left"))
	 end
      end,
      {description = "focus to screen to the left", group = "screen"}),

   awful.key({ modkey, "Shift", }, "c",
      function (c)
	 naughty.notify{ title = "c",  text = c}
	 if awesome_version == "v4.3" then
	    local c = client.focus
	    if c then
	       c:move_to_screen(awful.screen.focused():get_next_in_direction("right"))
	    end
	 else
	    awful.client.movetoscreen(c, awful.screen.focused():get_next_in_direction("right"))
	 end
      end,
      {description = "focus to screen to the right", group = "screen"}),

   -- awful.key({ modkey, "Shift", "Control"}, "x",
   -- 	 function (c)
   -- 	    if awesome_version == "v4.3" then
   -- 	       c:move_to_screen(awful.screen.focused():get_next_in_direction("down"))
   -- 	    else
   -- 	       awful.client.movetoscreen(c, awful.screen.focused():get_next_in_direction("down"))
   -- 	    end
   -- 	 end,
   -- 	 {description = "focus to screen below", group = "screen"}),

   -- Standard program
   awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
      {description = "open a terminal", group = "launcher"}),
   awful.key({ modkey, "Control" }, "r", awesome.restart,
      {description = "reload awesome", group = "awesome"}),
   awful.key({ modkey, "Shift"   }, "Escape", awesome.quit,
      {description = "quit awesome", group = "awesome"}),

   -- awful.key({ modkey, "m"}, "=",     function () awful.tag.incmwfact( 0.05)          end,
   --    {description = "increase master width factor", group = "layout"}),
   -- awful.key({ modkey, "m"}, "-",     function () awful.tag.incmwfact(-0.05)          end,
   --    {description = "decrease master width factor", group = "layout"}),
   -- awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
   --    {description = "increase the number of master clients", group = "layout"}),
   -- awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
   --    {description = "decrease the number of master clients", group = "layout"}),
   -- awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
   --    {description = "increase the number of columns", group = "layout"}),
   -- awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
   --    {description = "decrease the number of columns", group = "layout"}),
   awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
      {description = "select next", group = "layout"}),
   awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
      {description = "select previous", group = "layout"}),

   awful.key({ modkey, }, "-",
      function ()
	 local c = awful.client.restore()
	 -- Focus restored client
	 if c then
	    c:emit_signal(
	       "request::activate", "key.unminimize", {raise = true}
	    )
	 end
      end,
      {description = "restore minimized", group = "client"}),

   -- Prompt
   awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
      {description = "run prompt", group = "launcher"}),

   awful.key({ modkey }, "l",
      function ()
	 awful.prompt.run {
	    prompt       = "Run Lua code: ",
	    textbox      = awful.screen.focused().mypromptbox.widget,
	    exe_callback = awful.util.eval,
	    history_path = awful.util.get_cache_dir() .. "/history_eval"
	 }
      end,
      {description = "lua execute prompt", group = "awesome"}),
   -- Menubar
   awful.key({ modkey }, "p", function() menubar.show() end,
      {description = "show the menubar", group = "launcher"})
)

clientkeys = gears.table.join(
   awful.key({ modkey,           }, "f",
      function (c)
	 c.fullscreen = not c.fullscreen
	 c:raise()
      end,
      {description = "toggle fullscreen", group = "client"}),
   awful.key({ modkey,    }, "BackSpace",      function (c) c:kill()                         end,
      {description = "close", group = "client"}),
   awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
      {description = "toggle floating", group = "client"}),
   -- awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
   --    {description = "move to master", group = "client"}),
   -- awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
   --    {description = "move to screen", group = "client"}),
   awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
      {description = "toggle keep on top", group = "client"}),
   awful.key({ modkey, "Shift" }, "-",
      function (c)
	 -- The client currently has the input focus, so it cannot be
	 -- minimized, since minimized clients can't have the focus.
	 c.minimized = true
      end ,
      {description = "minimize", group = "client"}),
   awful.key({ modkey, }, "b",
      function (c)
	 c.maximized = not c.maximized
	 c:raise()
      end ,
      {description = "(un)maximize", group = "client"}),
   awful.key({ modkey, }, "v",
      function (c)
	 c.maximized_vertical = not c.maximized_vertical
	 c:raise()
      end ,
      {description = "(un)maximize vertically", group = "client"}),
   awful.key({ modkey, }, "h",
      function (c)
	 c.maximized_horizontal = not c.maximized_horizontal
	 c:raise()
      end ,
      {description = "(un)maximize horizontally", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
   globalkeys = gears.table.join(globalkeys,
				 -- View tag only.
				 awful.key({ modkey }, "#" .. i + 9,
				    function ()
				       local screen = awful.screen.focused()
				       local tag = screen.tags[i]
				       if tag then
					  tag:view_only()
				       end
				    end,
				    {description = "view tag #"..i, group = "tag"}),
				 -- Toggle tag display.
				 awful.key({ modkey, "Control" }, "#" .. i + 9,
				    function ()
				       local screen = awful.screen.focused()
				       local tag = screen.tags[i]
				       if tag then
					  awful.tag.viewtoggle(tag)
				       end
				    end,
				    {description = "toggle tag #" .. i, group = "tag"}),
				 -- Move client to tag.
				 awful.key({ modkey, "Shift" }, "#" .. i + 9,
				    function ()
				       if client.focus then
					  local tag = client.focus.screen.tags[i]
					  if tag then
					     client.focus:move_to_tag(tag)
					  end
				       end
				    end,
				    {description = "move focused client to tag #"..i, group = "tag"}),
				 -- Toggle tag on focused client.
				 awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
				    function ()
				       if client.focus then
					  local tag = client.focus.screen.tags[i]
					  if tag then
					     client.focus:toggle_tag(tag)
					  end
				       end
				    end,
				    {description = "toggle focused client on tag #" .. i, group = "tag"})
   )
end

clientbuttons = gears.table.join(
   awful.button({ }, 1, function (c)
	 c:emit_signal("request::activate", "mouse_click", {raise = true})
   end),
   awful.button({ modkey }, 1, function (c)
	 c:emit_signal("request::activate", "mouse_click", {raise = true})
	 awful.mouse.client.move(c)
   end),
   awful.button({ modkey }, 3, function (c)
	 c:emit_signal("request::activate", "mouse_click", {raise = true})
	 awful.mouse.client.resize(c)
   end)
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
   -- All clients will match this rule.
   { rule = { },
     properties = { border_width = beautiful.border_width,
		    border_color = beautiful.border_normal,
		    focus = awful.client.focus.filter,
		    raise = true,
		    keys = clientkeys,
		    buttons = clientbuttons,
		    screen = awful.screen.preferred,
		    placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
   },

   -- Floating clients.
   { rule_any = {
	instance = {
	   "DTA",  -- Firefox addon DownThemAll.
	   "copyq",  -- Includes session name in class.
	   "pinentry",
	},
	class = {
	   "Arandr",
	   "Blueman-manager",
	   "Gpick",
	   "Kruler",
	   "MessageWin",  -- kalarm.
	   "Sxiv",
	   "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
	   "Wpa_gui",
	   "veromix",
	   "xtightvncviewer"},

	-- Note that the name property shown in xprop might be set slightly after creation of the client
	-- and the name shown there might not match defined rules here.
	name = {
	   "Event Tester",  -- xev.
	},
	role = {
	   "AlarmWindow",  -- Thunderbird's calendar.
	   "ConfigManager",  -- Thunderbird's about:config.
	   "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
	}
   }, properties = { floating = true }},

   -- Add titlebars to normal clients and dialogs
   { rule_any = {type = { "normal", "dialog" }
		}, properties = { titlebars_enabled = true }
   },

   -- Set Firefox to always map on the tag named "2" on screen 1.
   -- { rule = { class = "Firefox" },
   --   properties = { screen = 1, tag = "2" } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
			 -- Set the windows at the slave,
			 -- i.e. put it at the end of others instead of setting it master.
			 -- if not awesome.startup then awful.client.setslave(c) end

			 if awesome.startup
			    and not c.size_hints.user_position
			 and not c.size_hints.program_position then
			    -- Prevent clients from being unreachable after screen count changes.
			    awful.placement.no_offscreen(c)
			 end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
			 -- buttons for the titlebar
			 local buttons = gears.table.join(
			    awful.button({ }, 1, function()
				  c:emit_signal("request::activate", "titlebar", {raise = true})
				  awful.mouse.client.move(c)
			    end),
			    awful.button({ }, 3, function()
				  c:emit_signal("request::activate", "titlebar", {raise = true})
				  awful.mouse.client.resize(c)
			    end)
			 )

			 awful.titlebar(c) : setup {
			    { -- Left
			       awful.titlebar.widget.iconwidget(c),
			       buttons = buttons,
			       layout  = wibox.layout.fixed.horizontal
			    },
			    { -- Middle
			       { -- Title
				  align  = "center",
				  widget = awful.titlebar.widget.titlewidget(c)
			       },
			       buttons = buttons,
			       layout  = wibox.layout.flex.horizontal
			    },
			    { -- Right
			       awful.titlebar.widget.floatingbutton (c),
			       awful.titlebar.widget.maximizedbutton(c),
			       awful.titlebar.widget.stickybutton   (c),
			       awful.titlebar.widget.ontopbutton    (c),
			       awful.titlebar.widget.closebutton    (c),
			       layout = wibox.layout.fixed.horizontal()
			    },
			    layout = wibox.layout.align.horizontal
						   }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
			 c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}