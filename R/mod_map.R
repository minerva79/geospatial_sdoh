# R/mod_map.R

mod_map_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 3,
      br(),
      selectInput(ns("metric"), "Fill by:",
                  choices = c("None", names(metric_map)), selected = "None"),
      radioButtons(ns("legend_dir"), "Legend order:",
                   choices = c("High → Low" = "desc", "Low → High" = "asc"),
                   selected = "desc"),
      checkboxInput(ns("showAcute"), "Show acute hospitals", FALSE),
      if (!has_icon) div(style="color:#a00; font-size:12px; margin-top:6px;",
                         "Note: www/hospital_icon.png not found — using circles.")
    ),
    column(width = 9, leafletOutput(ns("map"), height = 680))
  )
}

mod_map_server <- function(id, basemap_pa, labels, metric_map, hosp_acute, hosp_icon) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    add_ctrl <- function(proxy) {
      overlay <- if (isTRUE(input$showAcute) && nrow(hosp_acute) > 0) "Acute Hospitals" else NULL
      proxy %>% addLayersControl(
        baseGroups = c("Light"),
        overlayGroups = overlay,
        options = layersControlOptions(collapsed = TRUE)
      )
    }
    
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
        setView(lng = 103.82, lat = 1.35, zoom = 11) %>%
        addLayersControl(baseGroups = c("Light"),
                         options = layersControlOptions(collapsed = TRUE)) %>%
        addPolygons(
          data = basemap_pa,
          group = "PA",
          weight = 1, color = "#6b6b6b",
          fillColor = "#e9eef3", fillOpacity = 0.2,   # neutral default
          label = lapply(labels, HTML),
          labelOptions = labelOptions(direction = "auto", opacity = 0.95),
          highlightOptions = highlightOptions(weight = 2, color = "#000000", fillOpacity = 0.85, bringToFront = TRUE)
        )
    })
    
    observeEvent(list(input$metric, input$legend_dir), {
      proxy <- leafletProxy(ns("map")) %>%
        clearGroup("PA") %>%
        clearControls() %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Light")
      
      if (identical(input$metric, "None")) {
        proxy %>%
          addPolygons(
            data = basemap_pa,
            group = "PA",
            weight = 1, color = "#6b6b6b",
            fillColor = "#e9eef3", fillOpacity = 0.2,
            label = lapply(labels, HTML),
            labelOptions = labelOptions(direction = "auto", opacity = 0.95),
            highlightOptions = highlightOptions(weight = 2, color = "#000000", fillOpacity = 0.2, bringToFront = TRUE)
          ) %>% add_ctrl()
        return(invisible())
      }
      
      var_name <- metric_map[[input$metric]]
      vals     <- basemap_pa[[var_name]]
      
      pal_vec <- viridis(256)
      if (identical(input$legend_dir, "desc")) pal_vec <- rev(pal_vec)
      pal <- colorNumeric(pal_vec, domain = vals, na.color = "transparent")
      
      proxy %>%
        addPolygons(
          data = basemap_pa,
          group = "PA",
          weight = 1, color = "#6b6b6b",
          fillColor = unname(pal(vals)),
          fillOpacity = 0.8,
          label = lapply(labels, HTML),
          labelOptions = labelOptions(direction = "auto", opacity = 0.95),
          highlightOptions = highlightOptions(weight = 2, color = "#000000", fillOpacity = 0.9, bringToFront = TRUE)
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = unname(vals),
          title = input$metric,
          labFormat = labelFormat(digits = if (var_name %in% c("SEDI","SAI")) 0 else 2),
          opacity = 0.9
        ) %>% add_ctrl()
    }, ignoreInit = FALSE)
    
    observeEvent(input$showAcute, {
      if (isTRUE(input$showAcute) && nrow(hosp_acute) > 0) {
        proxy <- leafletProxy(ns("map"))
        if (!is.null(hosp_icon)) {
          proxy <- proxy %>%
            addMarkers(
              data = hosp_acute,
              group = "Acute Hospitals",
              icon  = hosp_icon,
              label = lapply(hosp_acute$label_html, HTML),
              options = markerOptions(riseOnHover = TRUE)
            )
        } else {
          proxy <- proxy %>%
            addCircleMarkers(
              data = hosp_acute,
              group = "Acute Hospitals",
              radius = 5, stroke = TRUE, weight = 1,
              fillOpacity = 0.95,
              label = lapply(hosp_acute$label_html, HTML)
            )
        }
        add_ctrl(proxy)
      } else {
        leafletProxy(ns("map")) %>% clearGroup("Acute Hospitals") %>% add_ctrl()
      }
    })
  })
}
