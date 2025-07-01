# gridtool

This is a small project for a Shiny module to interactively define and 
visualize a spatial raster grid using leaflet, terra, and sf.

This tools allows a user to define a raster extent by either:
- Drawing a rectangle on the map, or
- Manually defining the min/max of long/lat

The number of rows and columns in the raster grid is customizable.

The grid is rendered as an sf object built from terra::as.lines(), which is
rather permanent compared to e.g. polygons.

## Features

- Interactive map-based rectangle drawing to define spatial extent
- Manual override for xmin/xmax/ymin/ymax
- Adjustable number of rows and columns
- Raster grid rendering using `terra::as.lines()` for speed
- Live syncing of extent inputs with drawn or edited rectangles
- Toggle between Topo and Street base maps
- Uses `leaflet.extras::addDrawToolbar()` for draw/edit controls

## How to run

### Clone the repo and run the app:

```r
# Install dependencies (if not already installed)
install.packages(c("shiny", "leaflet", "leaflet.extras", "terra", "sf"))

# From your R console
shiny::runApp()
```

The app is organized using a modular structure:
```
gridtool/
├── app.R           # Launches the app
├── ui.R            # App-level UI
├── server.R        # App-level server
└── R/
    └── mod_grid.R  # Contains grid module UI + server
```