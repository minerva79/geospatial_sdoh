# server.R
function(input, output, session) {
  
  # Tab 1 module
  mod_map_server(
    id = "tab1",
    basemap_pa = basemap_pa,
    labels     = labels,
    metric_map = metric_map,
    hosp_acute = hosp_acute,
    hosp_icon  = hosp_icon
  )
  
  # Tab 2 module — pass a reactive ‘visible?’ flag
  tab2_visible <- reactive(identical(input$main_nav, "Postal lookup"))
  
  mod_postal_server(
    id = "tab2",
    subzones    = subzones,
    sg_postal   = sg_postal,
    eshi_sf     = eshi_sf,
    tab_visible = reactive(identical(input$main_nav, "Postal lookup"))
  )
  
  
}
