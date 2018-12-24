dofile(LockOn_Options.common_script_path.."devices_defs.lua")

indicator_type = indicator_types.COMMON

purposes                  = {render_purpose.GENERAL,
                                render_purpose.HUD_ONLY_VIEW,
                                render_purpose.SCREENSPACE_OUTSIDE_COCKPIT,
                                render_purpose.SCREENSPACE_INSIDE_COCKPIT}

local x_size         = 1
local y_size         = 1

function update_screenspace_diplacement(zoom_value)
    local default_width  = 0.5 * LockOn_Options.screen.height + (64 * zoom_value)
    if default_width > LockOn_Options.screen.height then
       default_width = LockOn_Options.screen.height
    end
    
    if default_width > 0.5 * LockOn_Options.screen.width then
       default_width = 0.5 * LockOn_Options.screen.width
    end
        
    local default_height = (y_size/x_size) * default_width
    local default_y      = LockOn_Options.screen.height - default_height
    local default_x      = LockOn_Options.screen.width  - default_width
   
    dedicated_viewport           = {default_x,default_y,default_width,default_height}
    dedicated_viewport_arcade = {default_x, 0        ,default_width,default_height}
end

update_screenspace_diplacement(0)    

function set_full_viewport_coverage(viewport)

   dedicated_viewport          = {viewport.x,
                                viewport.y,
                                viewport.width,
                                viewport.height}
   dedicated_viewport_arcade = dedicated_viewport
   purposes                  = {render_purpose.GENERAL,
                                render_purpose.HUD_ONLY_VIEW,
                                render_purpose.SCREENSPACE_OUTSIDE_COCKPIT,
                                render_purpose.SCREENSPACE_INSIDE_COCKPIT} -- set purposes to draw it always 
   render_target_always = true
end

-- try to find assigned viewport
local multimonitor_setup_name =  "Config/MonitorSetup/"..get_multimonitor_preset_name()..".lua"
local env = {}
      env.screen = LockOn_Options.screen
local f = loadfile(multimonitor_setup_name)
if      f     then
      setfenv(f,env)
      pcall(f)
      
      local vp = nil
      vp = env.CMSC_SCREEN
      
      if vp ~= nil then
         dbg_print("ok we have directly assigned viewport to MFCD\n")
         set_full_viewport_coverage(vp)
      end       
end

opacity_sensitive_materials = 
{
	"font_CMS"
}

-------PAGE IDs-------
id_Page =
{
	PAGE_NULL = 0,
	PAGE_OFF  = 1,
	PAGE_MAIN = 2
}

id_pagesubset =
{
	MAIN   = 0
}

page_subsets = {}
page_subsets[id_pagesubset.MAIN]   = LockOn_Options.script_path.."CMSC/indicator/CMSC_page.lua"
  	
----------------------
pages = {}
pages[id_Page.PAGE_MAIN] = {id_pagesubset.MAIN}

init_pageID     = id_Page.PAGE_MAIN
purposes = {render_purpose.GENERAL,
render_purpose.HUD_ONLY_VIEW,
render_purpose.SCREENSPACE_OUTSIDE_COCKPIT,
render_purpose.SCREENSPACE_INSIDE_COCKPIT}

