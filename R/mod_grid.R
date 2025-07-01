mod_grid_ui <- function(id) {
  
  # Becomes namespacing function
  ns <- NS(id)
  
  shiny::tagList(
    shiny::sidebarLayout(
      
      shiny::sidebarPanel(
        shiny::numericInput(ns("nrow"), "Number of Rows:", value = 10, min = 1),
        shiny::numericInput(ns("ncol"), "Number of Columns:", value = 10, min = 1),
        shiny::numericInput(ns("xmin"), "xmin:", value = -100),
        shiny::numericInput(ns("xmax"), "xmax:", value = -90),
        shiny::numericInput(ns("ymin"), "ymin:", value = 30),
        shiny::numericInput(ns("ymax"), "ymax:", value = 40),
        shiny::actionButton(ns("update_grid"), "Update Grid"),
        shiny::actionButton(ns("clear"), "Clear Grid"),
        shiny::verbatimTextOutput(ns("bbox"))
      ),
      
      shiny::mainPanel(
        leaflet::leafletOutput(ns("map"), height = "600px")
      )
      
    )
  )
}

mod_grid_server <- function(id) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    drawn_extent <- shiny::reactiveVal(NULL)
    grid_raster <- shiny::reactiveVal(NULL)
    
    # Grid generator
    generate_grid <- function(extent, nrow, ncol) {
      # r <- terra::rast(nrows = nrow, ncols = ncol, extent = extent, vals = NA)
      # r[] <- 1:ncell(r)
      # polys <- as.polygons(r)
      # polys$label <- as.character(values(r)[, 1])
      # polys
      r <- terra::rast(
        nrows = nrow, ncols = ncol, 
        extent = extent
      )
      terra::values(r) <- 1
      r
    }
    
    # User draws grid extent
    shiny::observeEvent(input$map_draw_new_feature, {
      # Clear previous grid
      leaflet::leafletProxy(ns("map")) |> leaflet::clearGroup("drawn")
      
      feature <- input$map_draw_new_feature
      coords <- feature$geometry$coordinates[[1]]
      lngs <- sapply(coords, function(x) x[[1]])
      lats <- sapply(coords, function(x) x[[2]])
      extent <- terra::ext(min(lngs), max(lngs), min(lats), max(lats))
      drawn_extent(extent)
      grid_raster(generate_grid(extent, input$nrow, input$ncol))
      
      xmin <- min(lngs)
      xmax <- max(lngs)
      ymin <- min(lats)
      ymax <- max(lats)
      shiny::updateNumericInput(session, "xmin", value = xmin)
      shiny::updateNumericInput(session, "xmax", value = xmax)
      shiny::updateNumericInput(session, "ymin", value = ymin)
      shiny::updateNumericInput(session, "ymax", value = ymax)
    })
    
    # User edits drawn grid extent
    observeEvent(input$map_draw_edited_features, {
      edited <- input$map_draw_edited_features
      
      if (is.null(edited$features) || length(edited$features) == 0) return()
      
      # Grab the edited geometry (assumes only one shape is editable)
      edited <- input$map_draw_edited_features
      coords <- edited$features[[1]]$geometry$coordinates[[1]]
      
      lngs <- sapply(coords, function(x) x[[1]])
      lats <- sapply(coords, function(x) x[[2]])
      
      extent <- ext(min(lngs), max(lngs), min(lats), max(lats))
      drawn_extent(extent)
      
      # Recalculate the grid using current nrow/ncol
      grid_raster(generate_grid(extent, input$nrow, input$ncol))
      
      xmin <- min(lngs)
      xmax <- max(lngs)
      ymin <- min(lats)
      ymax <- max(lats)
      shiny::updateNumericInput(session, "xmin", value = xmin)
      shiny::updateNumericInput(session, "xmax", value = xmax)
      shiny::updateNumericInput(session, "ymin", value = ymin)
      shiny::updateNumericInput(session, "ymax", value = ymax)
    })
    
    # User manually updates grid
    shiny::observeEvent(input$update_grid, {
      # Use drawn extent if it exists
      ext_val <- drawn_extent()
      
      # If no drawn extent, fall back to manual inputs
      if (is.null(ext_val)) {
        shiny::req(input$xmin, input$xmax, input$ymin, input$ymax)
        ext_val <- terra::ext(input$xmin, input$xmax, input$ymin, input$ymax)
        drawn_extent(ext_val)  # store it for future use
        
        # Add visual bounding box to map
        bbox_poly <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(
          xmin = input$xmin,
          xmax = input$xmax,
          ymin = input$ymin,
          ymax = input$ymax
        ), crs = 4326)))
        
        leaflet::leafletProxy(ns("map")) |>
          leaflet::clearGroup("drawn") |>
          leaflet::addPolygons(
            data = bbox_poly, group = "drawn", 
            color = "red", fill = FALSE, weight = 2
          )
      }
      
      grid_raster(generate_grid(ext_val, input$nrow, input$ncol))
    })
    
    shiny::observeEvent(input$clear, {
      drawn_extent(NULL)
      grid_raster(NULL)
      leaflet::leafletProxy(ns("map")) |> leaflet::clearShapes()
    })
    
    output$bbox <- shiny::renderPrint({
      ext <- drawn_extent()
      if (!is.null(ext)) ext else "Draw a rectangle on the map to define extent."
    })
    
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron", group = "Street") |> 
        leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Topo") |> 
        leaflet::addLayersControl(
          baseGroups = c("Street", "Topo")
        ) |> 
        leaflet.extras::addDrawToolbar(
          targetGroup = "drawn",
          polylineOptions = FALSE,
          polygonOptions = FALSE,
          circleOptions = FALSE,
          markerOptions = FALSE, 
          circleMarkerOptions = FALSE, 
          rectangleOptions = leaflet.extras::drawRectangleOptions(
            shapeOptions = leaflet.extras::drawShapeOptions(
              color = "red", fill = FALSE
            )
          ),
          editOptions = leaflet.extras::editToolbarOptions()
        )
    })
    
    shiny::observe({
      shiny::req(grid_raster())
      grid_lines <- terra::as.lines(grid_raster())
      grid_sf <- sf::st_as_sf(grid_lines)
      leaflet::leafletProxy(ns("map")) |>
        leaflet::clearGroup("grid") |>
        # leaflet::addPolygons(
        #   data = grid_raster(),
        #   fill = FALSE, 
        #   # fillColor = "transparent",
        #   # fillOpacity = 0,
        #   color = "black",
        #   weight = 1,
        #   group = "grid",
        #   label = ~label
        # )
        leaflet::addPolylines(
          data = grid_sf, 
          color = "black", weight = 1, 
          group = "grid"
        )
    })
    
  })
  
}