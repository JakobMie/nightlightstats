slice_shapefile <- function(shapefile,
                            shapefile_location,
                            slice_direction,
                            number_pieces){

  # need the location without "/" at the end. in case user types this with "/", remove that
  if (substr(shapefile_location,
             start = nchar(shapefile_location),
             stop = nchar(shapefile_location)) == "/"){
    shapefile_location <- substr(shapefile_location, 1, nchar(shapefile_location) - 1)
  }


  if (length(grep(shapefile, pattern = ".rds")) != 0){
    shapefile_name <- strsplit(shapefile, ".rds")
    shapefile_name_ending <- substr(shapefile,
                                    start = nchar(shapefile) - 3,
                                    stop = (nchar(shapefile)))
    shapefile <- readRDS(paste0(shapefile_location, "/", shapefile))
    if (class(shapefile)[1] == "sf"){
      stop("Unfortunately, the function does not work with sf.rds shapefiles.")
    }
  } else if (length(grep(shapefile, pattern = ".shp")) != 0){
    shapefile_name <- strsplit(shapefile, ".shp")
    shapefile_name_ending <- substr(shapefile,
                                    start = nchar(shapefile) - 3,
                                    stop = (nchar(shapefile)))
    shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", shapefile))
  } else if (length(grep(shapefile, pattern = ".kml")) != 0){
    shapefile_name <- strsplit(shapefile, ".kml")
    shapefile_name_ending <- substr(shapefile,
                                    start = nchar(shapefile) - 3,
                                    stop = (nchar(shapefile)))
    shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", shapefile))
  } else if (length(grep(shapefile, pattern = ".gpkg")) != 0){
    shapefile_name <- strsplit(shapefile, ".gpkg")
    shapefile_name_ending <- substr(shapefile,
                                    start = nchar(shapefile) - 4,
                                    stop = (nchar(shapefile)))
    shapefile <- suppressWarnings(rgdal::readOGR(paste0(shapefile_location, "/", shapefile)))
    # suppress warnings because gpkg has multiple adm layers. for split, just read the first one = adm0
  } else {
    stop("Unfortunately, the function does not work the format of your shapefile.")
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
      coord_current <- as(raster::extent(coord_current), "SpatialPolygons")
      raster::crs(coord_current) <- crs_string
      shapefile_current <- rgeos::gIntersection(shapefile, coord_current , byid = TRUE)
      shapefile_current <- sp::SpatialPolygonsDataFrame(shapefile_current,
                                                        data.frame(N = "1", row.names = c("1 1")))
      assign(paste0(shapefile_name, "_", i-1), shapefile_current)
    }

  } else if (slice_direction == "vertical"){
    slice_borders <- seq(xmin, xmax, length.out = number_pieces + 1)
    for (i in 2:length(slice_borders)){
      coord_current <- c(slice_borders[i-1], slice_borders[i], ymin, ymax)
      coord_current <- as(raster::extent(coord_current), "SpatialPolygons")
      raster::crs(coord_current) <- crs_string
      shapefile_current <- rgeos::gIntersection(shapefile, coord_current , byid = TRUE)
      shapefile_current <- sp::SpatialPolygonsDataFrame(shapefile_current,
                                                        data.frame(N = "1", row.names = c("1 1")))
      assign(paste0(shapefile_name, "_", i-1), shapefile_current)
    }
  }

  # save the sliced shapefiles as .rds shapefiles

  for (j in 1:number_pieces){
    current_shapefile <- get(paste0(shapefile_name, "_", j))
    readr::write_rds(current_shapefile, path = paste0(shapefile_location, "/", shapefile_name, "_", j, ".rds"), compress = c("none"))
  }
}
