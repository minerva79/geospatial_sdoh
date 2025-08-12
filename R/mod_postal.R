# R/mod_postal.R — ggplot map + single postal marker; ESHI as two stacked key–value tables

mod_postal_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 3,
      br(),
      textInput(ns("postal_in"), "Enter 6-digit postal code:", placeholder = "e.g., 600101"),
      actionButton(ns("btn_search"), "Search"),
      br(), br(),
      helpText("Digits only; spaces/dashes are ignored.")
    ),
    column(
      width = 9,
      plotOutput(ns("map_plot"), height = 680, width = "100%"),
      br(),
      textOutput(ns("postal_status")),
      br(),
      # stacked tables (one under another)
      column(12, tableOutput(ns("eshi_table_core"))),
      br(),
      column(12, tableOutput(ns("eshi_table_access")))
    )
  )
}

# subzones: MULTIPOLYGON sf (WGS84)
# sg_postal: POINT sf with postal column (likely 'postal_code')
# eshi_sf:   POINT sf with postal column (likely 'postal') + metrics
# tab_visible kept for API compatibility, unused for static plot
mod_postal_server <- function(id, subzones, sg_postal, eshi_sf, tab_visible) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---------- detect postal columns & prep padded keys ----------
    cand <- c("postal", "postcode", "postal_code", "post_code", "zip")
    pick_postal_col <- function(df) {
      nm <- names(df); hit <- nm[tolower(nm) %in% cand]
      if (length(hit)) hit[1] else stop("No postal column found.")
    }
    postal_col_sg   <- pick_postal_col(sg_postal)   # e.g., 'postal_code'
    postal_col_eshi <- pick_postal_col(eshi_sf)     # e.g., 'postal'
    
    sg_postal2 <- sg_postal %>%
      dplyr::mutate(.postal_chr = sprintf("%06s", as.character(.data[[postal_col_sg]])))
    eshi2 <- eshi_sf %>%
      dplyr::mutate(.postal_chr = sprintf("%06s", as.character(.data[[postal_col_eshi]])))
    
    # fixed extent
    bb <- sf::st_bbox(subzones)
    
    # ---------- helpers ----------
    # Build a compact key–value table from a 1-row data.frame
    make_kv <- function(df1row, fields, nice = NULL) {
      fields <- intersect(fields, names(df1row))
      if (!length(fields)) return(data.frame(Field = character(), Value = character()))
      vals <- lapply(fields, function(f) df1row[[f]][1])
      vals <- vapply(vals, function(v) {
        if (is.numeric(v)) format(round(as.numeric(v), 2), trim = TRUE) else as.character(v)
      }, FUN.VALUE = character(1))
      labels <- fields
      if (!is.null(nice)) {
        m <- match(fields, names(nice)); labels[!is.na(m)] <- nice[na.omit(m)]
      }
      data.frame(Field = labels, Value = vals, check.names = FALSE)
    }
    
    nice_names <- c(
      postal_col_eshi = "Postal",
      subzone = "Subzone",
      planning_area = "Planning area",
      region = "Region",
      SEDI = "SEDI",
      SAI  = "SAI",
      eshi = "ESHI",
      cdi  = "CDI",
      access = "ACCESS",
      nearest_polyclinic = "Nearest polyclinic",
      distance_to_polyclinic_km = "Dist. to polyclinic (km)",
      nearest_mrt = "Nearest MRT",
      distance_to_mrt_km = "Dist. to MRT (km)",
      nearest_hospital = "Nearest hospital",
      distance_to_hospital_km = "Dist. to hospital (km)",
      public_rental = "Public rental",
      SHI = "SHI"
    )
    
    core_fields   <- c(postal_col_eshi, "subzone", "planning_area", "region", "SEDI", "SAI", "eshi", "cdi", "access")
    access_fields <- c("nearest_polyclinic", "distance_to_polyclinic_km",
                       "nearest_mrt", "distance_to_mrt_km",
                       "nearest_hospital", "distance_to_hospital_km",
                       "public_rental", "SHI")
    
    # ---------- reactive selection ----------
    selected_pt <- reactiveVal(NULL)
    
    # ---------- static map ----------
    output$map_plot <- renderPlot({
      p <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = subzones, fill = "#e9eef3", color = "#6b6b6b", linewidth = 0.2) +
        ggplot2::coord_sf(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"]), expand = FALSE) +
        ggplot2::theme_void()
      pt <- selected_pt()
      if (!is.null(pt) && nrow(pt) > 0) {
        p <- p + ggplot2::geom_sf(data = pt, color = "#D62728", fill = "#D62728", size = 2.8, shape = 21, linewidth = 0.6)
      }
      p
    })
    
    output$postal_status      <- renderText({ "" })
    output$eshi_table_core    <- renderTable({ NULL }, striped = TRUE, bordered = TRUE, rownames = FALSE)
    output$eshi_table_access  <- renderTable({ NULL }, striped = TRUE, bordered = TRUE, rownames = FALSE)
    
    # ---------- search ----------
    observeEvent(input$btn_search, {
      raw  <- input$postal_in
      code <- gsub("\\D", "", raw)
      if (nchar(code) != 6) code <- NA_character_
      
      selected_pt(NULL)
      output$eshi_table_core   <- renderTable({ NULL }, striped = TRUE, bordered = TRUE, rownames = FALSE)
      output$eshi_table_access <- renderTable({ NULL }, striped = TRUE, bordered = TRUE, rownames = FALSE)
      
      if (is.na(code)) {
        output$postal_status <- renderText(sprintf("Postal code '%s' is not valid (must be exactly 6 digits).", raw))
        return(invisible())
      }
      
      pt <- dplyr::filter(sg_postal2, .postal_chr == code)
      if (nrow(pt) == 0) {
        output$postal_status <- renderText(sprintf("Postal code '%s' was not found in the postal dataset.", code))
        return(invisible())
      }
      
      selected_pt(pt[1, , drop = FALSE])  # show the point
      
      eshi_row <- dplyr::filter(eshi2, .postal_chr == code)
      if (nrow(eshi_row) > 0) {
        df <- sf::st_drop_geometry(eshi_row) %>%
          dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))
        
        # make two compact key–value tables (stacked)
        kv_core   <- make_kv(df, core_fields,   nice = nice_names)
        kv_access <- make_kv(df, access_fields, nice = nice_names)
        
        # ensure the postal column label shows as "Postal"
        kv_core$Field[kv_core$Field == postal_col_eshi] <- "Postal"
        
        output$eshi_table_core   <- renderTable(kv_core,   striped = TRUE, bordered = TRUE, rownames = FALSE)
        output$eshi_table_access <- renderTable(kv_access, striped = TRUE, bordered = TRUE, rownames = FALSE)
        
        output$postal_status <- renderText(sprintf("Showing postal code %s. ESHI record found.", code))
      } else {
        output$postal_status <- renderText(sprintf("Showing postal code %s. (No ESHI record found.)", code))
      }
    })
  })
}
