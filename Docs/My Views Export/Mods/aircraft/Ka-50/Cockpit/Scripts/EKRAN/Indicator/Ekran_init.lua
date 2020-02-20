dofile(LockOn_Options.common_script_path.."devices_defs.lua")

-- add Hellfrog
dofile(LockOn_Options.common_script_path.."ViewportHandling.lua")
-- end add Hellfrog


indicator_type = indicator_types.COMMON

-- add Hellfrog
purposes      = {render_purpose.GENERAL}
try_find_assigned_viewport("EKRAN","MFCD_04") 
-- end add Hellfrog



-------PAGE IDs-------
id_Page =
{
	PAGE_NULL  = 0,
	PAGE_OFF   = 1,
	PAGE_MAIN  = 2
}

id_pagesubset =
{
	MAIN	= 0
}

page_subsets = {}
page_subsets[id_pagesubset.MAIN]   = LockOn_Options.script_path.."EKRAN\\Indicator\\EKRAN.lua"
  	
----------------------
pages = {}
pages[id_Page.PAGE_MAIN] = {id_pagesubset.MAIN}

init_pageID   = id_Page.PAGE_MAIN
use_parser    = false
