# ui.R
navbarPage(
  title = "Singapore SDOH Indices",
  id = "main_nav",
  
  tabPanel("Map",            mod_map_ui("tab1")),
  tabPanel("Postal lookup",  mod_postal_ui("tab2")),
  tabPanel(
    title = "About",
    icon  = icon("circle-info"),
    includeMarkdown("www/about.md")
  )
)
