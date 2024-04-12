#' slice_shapefile
#'
#' This is an auxiliary function that allows cutting shapefiles into smaller
#' pieces. You may want to use this if you are trying to use nightlight_plot
#' or nightlight_calculate, but your geographic area is too large and the
#' capacities of your PC do not allow running these functions for such a
#' large region. If that is the case, an error message "Cannot allocate
#' vector of size ..." will appear in nightlight_plot or nightlight_calculate.
#' To continue nevertheless, you can use this function to slice your shapefile
#' into smaller pieces. The function will save the sliced pieces on your drive
#' (the original shapefile will be unaffected), then you can continue working
#' with the sliced shapefiles. You can gather the names of the sliced
#' shapefiles into a vector and input the vector into the "shapefiles"
#' arugment of the other functions. The area names will automatically adjust
#' using indices if you simply input one argument (the name of your region)
#' into "area_names".
#'
#' @param shapefile May not be empty. Provide the filename of your
#' shapefile as a string here.
#' @param shapefile_location May not be empty. Provide the location of the
#' shapefile on your drive as a string here.
#' @param slice_direction May not be empty. Provide the direction of the
#' slicing as a string here. Options are "horizontal" and "vertical".
#' @param number_pieces May not be empty. Provide the number of pieces
#' you want the area to be split up into as an integer here.
#' @param gpkg_layer For .gpkg files, enter the layer here as a string.
#' To find out which layers are included in your .gpkg shapefile, you can
#' use sf::st_layers().
#' @export

slice_shapefile <- function(shapefile,
                            shapefile_location,
                            slice_direction,
                            number_pieces,
                            gpkg_layer = NULL){

  # need the location without "/" at the end. in case user types this
  # with "/", remove that
  if (substr(shapefile_location,
             start = nchar(shapefile_location),
             stop = nchar(shapefile_location)) == "/"){
    shapefile_location <- substr(shapefile_location, 1,
                                 nchar(shapefile_location) - 1)
  }

  if (length(grep(shapefile, pattern = ".gpkg")) != 0){
    shapefile_name <- strsplit(shapefile, ".gpkg")
    shapefile_name_ending <- substr(shapefile,
                                    start = nchar(shapefile) - 4,
                                    stop = (nchar(shapefile)))
    if (!is.null(gpkg_layer)){
      shapefile <- sf::st_read(
        paste0(shapefile_location, "/", shapefile), gpkg_layer)
    } else if (is.null(gpkg_layer)){
      stop(paste0("Please enter the layer of your .gpkg shapefile by ",
                  "hand using the gpkg_layer argument."))
    }
  } else if (length(grep(shapefile, pattern = ".shp")) != 0){
    shapefile_name <- strsplit(shapefile, ".shp")
    shapefile_name_ending <- substr(shapefile,
                                    start = nchar(shapefile) - 3,
                                    stop = (nchar(shapefile)))
    shapefile <- sf::st_read(paste0(shapefile_location, "/", shapefile))
  } else {
    stop(paste0("Unfortunately, the function does not work the format ",
                "of your shapefile."))
  }

  crs_string <- raster::crs(shapefile)
  extent <- raster::extent(shapefile)
  extent_bbox <- sp::bbox(extent)

  xmin = extent_bbox[1,1]
  xmax = extent_bbox[1,2]
  ymin = extent_bbox[2,1]
  ymax = extent_bbox[2,2]

  if (slice_direction == "horizontal"){
    slice_borders <- seq(ymin, ymax, length.out = number_pieces + 1)
    for (i in 2:length(slice_borders)){
      coord_current <- c(xmin, xmax, slice_borders[i-1], slice_borders[i])
      coord_current <- methods::as(raster::extent(coord_current),
                                   "SpatialPolygons")
      raster::crs(coord_current) <- crs_string
      coord_current <- sf::st_as_sf(coord_current)
      shapefile_current <- suppressWarnings(sf::st_intersection(sf::st_geometry(shapefile), coord_current))
      #shapefile_current <- sp::SpatialPolygonsDataFrame(
       # shapefile_current, data.frame(N = "1", row.names = c("1 1")))
      #shapefile_current <- sf::st_as_sf(shapefile_current)
      assign(paste0(shapefile_name, "_", i-1), shapefile_current)
    }

  } else if (slice_direction == "vertical"){
    slice_borders <- seq(xmin, xmax, length.out = number_pieces + 1)
    for (i in 2:length(slice_borders)){
      coord_current <- c(slice_borders[i-1], slice_borders[i], ymin, ymax)
      coord_current <- methods::as(raster::extent(coord_current),
                                   "SpatialPolygons")
      raster::crs(coord_current) <- crs_string
      coord_current <- sf::st_as_sf(coord_current)
      shapefile_current <- suppressWarnings(sf::st_intersection(shapefile, coord_current))
      #shapefile_current <- sp::SpatialPolygonsDataFrame(
        #shapefile_current, data.frame(N = "1", row.names = c("1 1")))
      #shapefile_current <- sf::st_as_sf(shapefile_current)
      assign(paste0(shapefile_name, "_", i-1), shapefile_current)
    }
  }

  # save the sliced shapefiles 
  
  for (j in 1:number_pieces){
    current_shapefile <- get(paste0(shapefile_name, "_", j))
    sf::st_write(current_shapefile, paste0(shapefile_location, "/",
                                           shapefile_name, "_", j, ".gpkg"), driver = "GPKG")
  }
}
