help_shapefile <-  help_shapefiles[i]
shapefile <-  shapefiles[i]
# these are either provided by the user and hence activated at this point
# or shapefile will be NULL and help_shapefile will be NA and shapefile will be detected or downloaded later

# if shapefile available at this point, its type is detected and it is read with the according function
if (length(grep(help_shapefile, pattern = "sp.rds")) != 0){
  shapefile <- readRDS(shapefile)
}
if (length(grep(help_shapefile, pattern = "sf.rds")) != 0){
  print("Unfortunately, the function does not work with sf.rds shapefiles. If no other option is available to you, you can try using the min/max x- and y-coordinates of your shapefile in the function input instead.")
}
if (length(grep(help_shapefile, pattern = ".shp")) != 0 |
    length(grep(help_shapefile, pattern = ".kml")) != 0){
  shapefile <- rgdal::readOGR(shapefile)
}
if (length(grep(help_shapefile, pattern = ".gpkg")) != 0){
  layers <- rgdal::ogrListLayers(help_shapefile)
  layer <- length(layers) - admlevel
  shapefile <- rgdal::readOGR(help_shapefile, layers[layer])
}

if(!is.null(user_coordinates)){
  extent <- raster::extent(user_coordinates)
  extent_bbox <- sp::bbox(extent)
  shapefile <- as(extent, "SpatialPolygons")
  raster::crs(shapefile) <- "+init=epsg:4326"
  shapefile <- sp::SpatialPolygonsDataFrame(shapefile, data.frame(N = c("1"), row.names = c("1")))
} # creates a rectangular shapefile if user provides coordinates

ISO3s <- suppressWarnings(countrycode::countrycode(area_names,
                                                   origin = "country.name",
                                                   destination = "iso3c",
                                                   custom_match = c('Akrotiri and Dhekelia' = 'XAD',
                                                                    'Caspian Sea' = 'XCA',
                                                                    'Clipperton Island' = 'XCL',
                                                                    'Kosovo' = 'XKO',
                                                                    'Micronesia' = 'FSM',
                                                                    'Paracel Islands' = 'XPI',
                                                                    'Saint-Martin' = 'MAF',
                                                                    'Saint Martin' = 'MAF',
                                                                    'Spratly Islands' = 'XSP'))) # these countries have GADM shapefiles but are not recognized by countrycode
for (z in 1:length(ISO3s)){
  if (is.na(ISO3s[z])){
    print(paste0("There is no ISO3 countrycode for ", area_names[z], ", hence download from GADM will not work. Either your shapefile is not a country or, if it is a country, the countryname was not recognized correctly."))
  }
}

area_name <- area_names[i]
ISO3 <- ISO3s[i]

# check if shapefile is already downloaded
# check for every format. will preferably find
# sp.rds, then .shp, .kml, .gpkg in this order

# first: check for GADM shapefiles which are identified by ISO3 and admlevel

# sp.rds
if (is.null(shapefile)){
  help_shapefile <- list.files(shapefile_location, pattern = "sp.rds")
  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = admlevel)]
  if (length(help_shapefile) != 0){
    shapefile <- readRDS(paste0(shapefile_location, "/", help_shapefile))
  }
}
# .shp
if (is.null(shapefile)){
  help_shapefile <- list.files(shapefile_location, pattern = ".shp")
  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = admlevel)]
  if (length(help_shapefile) != 0){
    shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", help_shapefile))
  }
}
# .shp but still zipped as .zip
if (is.null(shapefile)){
  help_shapefile <- list.files(shapefile_location, pattern = "shp.zip")
  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
  if (length(help_shapefile) != 0){
    help_shapefile <- utils::unzip(zipfile = paste0(shapefile_location, "/", help_shapefile), exdir = shapefile_location)
    help_shapefile <- list.files(shapefile_location, pattern = ".shp")
    help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
    help_shapefile <- help_shapefile[grep(help_shapefile, pattern = admlevel)]
    shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", help_shapefile))
    zipfile <- list.files(shapefile_location, pattern = "shp.zip")
    zipfile <- zipfile[grep(zipfile, pattern = ISO3)]
    unlink(zipfile)
  }
}
# .kml
if (is.null(shapefile)){
  help_shapefile <- list.files(shapefile_location, pattern = ".kml")
  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = admlevel)]
  if (length(help_shapefile) != 0){
    shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", help_shapefile))
  }
}
# .kml but still zipped as .kmz
if (is.null(shapefile)){
  help_shapefile <- list.files(shapefile_location, pattern = ".kmz")
  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
  if (length(help_shapefile) != 0){
    help_shapefile <- unzip(zipfile = paste0(help_shapefile, "/", help_shapefile), exdir = shapefile_location)
    shapefile <- rgdal::readOGR(help_shapefile)
    zipfile <- list.files(shapefile_location, pattern = ".kmz")
    zipfile <- zipfile[grep(zipfile, pattern = ISO3)]
    unlink(zipfile)
  }
}
# .gpkg
if (is.null(shapefile)){
  help_shapefile <- list.files(shapefile_location, pattern = ".gpkg")
  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
  if (length(help_shapefile) != 0){
    layers <- rgdal::ogrListLayers(paste0(shapefile_location, "/", help_shapefile))
    layer <- length(layers) - admlevel
    shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", help_shapefile), layers[layer])
  }
}
# .gpkg but still zipped as .zip
if (is.null(shapefile)){
  help_shapefile <- list.files(shapefile_location, pattern = "gpkg.zip")
  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
  if (length(help_shapefile) != 0){
    help_shapefile <- utils::unzip(zipfile = paste0(shapefile_location, "/", help_shapefile), exdir = shapefile_location)
    help_shapefile <- list.files(shapefile_location, pattern = ".gpkg")
    help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
    help_shapefile <- help_shapefile[-grep(help_shapefile, pattern = ".zip")]
    layers <- rgdal::ogrListLayers(paste0(shapefile_location, "/", help_shapefile))
    layer <- length(layers) - admlevel
    shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", help_shapefile), layers[layer])
    zipfile <- list.files(shapefile_location, pattern = "gpkg.zip")
    zipfile <- zipfile[grep(zipfile, pattern = ISO3)]
    unlink(zipfile)
  }
}

# then: check for shapefiles that are identified by the name given by the user in area_names

# sp.rds
if (is.null(shapefile)){
  help_shapefile <- list.files(shapefile_location, pattern = "sp.rds")
  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
  if (length(help_shapefile) != 0){
    shapefile <- readRDS(paste0(shapefile_location, "/", help_shapefile))
  }
}
# .shp
if (is.null(shapefile)){
  help_shapefile <- list.files(shapefile_location, pattern = ".shp")
  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
  if (length(help_shapefile) != 0){
    shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", help_shapefile))
  }
}
# .shp but still zipped as .zip
if (is.null(shapefile)){
  help_shapefile <- list.files(shapefile_location, pattern = "shp.zip")
  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
  if (length(help_shapefile) != 0){
    help_shapefile <- utils::unzip(zipfile = paste0(shapefile_location, "/", help_shapefile), exdir = shapefile_location)
    help_shapefile <- list.files(shapefile_location, pattern = ".shp")
    help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
    shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", help_shapefile))
    zipfile <- list.files(shapefile_location, pattern = "shp.zip")
    zipfile <- zipfile[grep(zipfile, pattern = area_name)]
    unlink(zipfile)
  }
}
# .kml
if (is.null(shapefile)){
  help_shapefile <- list.files(shapefile_location, pattern = ".kml")
  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
  if (length(help_shapefile) != 0){
    shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", help_shapefile))
  }
}
# .kml but still zipped as .kmz
if (is.null(shapefile)){
  help_shapefile <- list.files(shapefile_location, pattern = ".kmz")
  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
  if (length(help_shapefile) != 0){
    help_shapefile <- utils::unzip(zipfile = paste0(shapefile_location, "/", help_shapefile), exdir = shapefile_location)
    shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", help_shapefile))
    zipfile <- list.files(".", pattern = ".kmz")
    zipfile <- zipfile[grep(zipfile, pattern = area_name)]
    unlink(zipfile)
  }
}
# .gpkg
if (is.null(shapefile)){
  help_shapefile <- list.files(shapefile_location, pattern = ".gpkg")
  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
  if (length(help_shapefile) != 0){
    layers <- rgdal::ogrListLayers(paste0(shapefile_location, "/", help_shapefile))
    layer <- length(layers) - admlevel
    shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", help_shapefile), layers[layer])
  }
}
# .gpkg but still zipped as .zip
if (is.null(shapefile)){
  help_shapefile <- list.files(shapefile_location, pattern = "gpkg.zip")
  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
  if (length(help_shapefile) != 0){
    help_shapefile <- utils::unzip(zipfile = paste0(shapefile_location, "/", help_shapefile), exdir = shapefile_location)
    help_shapefile <- list.files(shapefile_location, pattern = ".gpkg")
    help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
    help_shapefile <- help_shapefile[-grep(help_shapefile, pattern = ".zip")]
    layers <- rgdal::ogrListLayers(paste0(shapefile_location, "/", help_shapefile))
    layer <- length(layers) - admlevel
    shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", help_shapefile), layers[layer])
    zipfile <- list.files(shapefile_location, pattern = "gpkg.zip")
    zipfile <- zipfile[grep(zipfile, pattern = area_name)]
    unlink(zipfile)
  }
}

# if shapefile is not downloaded yet: download from GADM
# sp.rds
if (is.null(shapefile) & download_shape == "sp.rds"){
  stumpurl <- "https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_"
  utils::download.file(paste0(stumpurl, ISO3, "_", admlevel, "_sp.rds"),
                       destfile = paste0(shapefile_location, "/", "gadm36_", ISO3, "_", admlevel, "_sp.rds"), mode = "wb")
  help_shapefile <- paste0(shapefile_location, "/", "gadm36_", ISO3, "_", admlevel, "_sp.rds")
  shapefile <- readRDS(help_shapefile)
}
# .shp
if (is.null(shapefile) & download_shape == ".shp"){
  stumpurl <- "https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_"
  utils::download.file(paste0(stumpurl, ISO3, "_shp.zip"),
                       destfile = paste0(shapefile_location, "/", "gadm36_", ISO3, "_shp.zip"), mode = "wb")
  help_shapefile <- paste0(shapefile_location, "/", "gadm36_", ISO3, "_shp.zip")
  help_shapefile <- utils::unzip(zipfile = help_shapefile, exdir = shapefile_location)
  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ".shp")]
  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = admlevel)]
  shapefile <- rgdal::readOGR(help_shapefile)
  unlink(paste0(shapefile_location, "/", "gadm36_", ISO3, "_shp.zip"))
}
# .kml
if (is.null(shapefile) & download_shape == ".kml"){
  stumpurl <- "https://biogeo.ucdavis.edu/data/gadm3.6/kmz/gadm36_"
  utils::download.file(paste0(stumpurl, ISO3, "_", admlevel, ".kmz"),
                       destfile = paste0(shapefile_location, "/",  "gadm36_", ISO3, "_", admlevel, ".kmz"), mode = "wb")
  help_shapefile <- paste0(shapefile_location, "/", "gadm36_", ISO3, "_", admlevel, ".kmz")
  help_shapefile <- utils::unzip(zipfile = help_shapefile, exdir = shapefile_location)
  shapefile <- rgdal::readOGR(help_shapefile)
  unlink(paste0(shapefile_location, "/", "gadm36_", ISO3, "_", admlevel, ".kmz"))
}
# .gpkg
if (is.null(shapefile) & download_shape == ".gpkg"){
  stumpurl <- "https://biogeo.ucdavis.edu/data/gadm3.6/gpkg/gadm36_"
  utils::download.file(paste0(stumpurl, ISO3, "_gpkg.zip"),
                       destfile = paste0(shapefile_location, "/",  "gadm36_", ISO3, "_gpkg.zip"), mode = "wb")
  help_shapefile <- paste0(shapefile_location, "/", "gadm36_", ISO3, "_gpkg.zip")
  help_shapefile <- utils::unzip(zipfile = help_shapefile, exdir = shapefile_location)
  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ".gpkg")]
  layers <- rgdal::ogrListLayers(help_shapefile)
  layer <- length(layers) - admlevel
  shapefile <- rgdal::readOGR(help_shapefile, layers[layer])
  unlink(paste0(shapefile_location, "/", "gadm36_", ISO3, "_gpkg.zip"))
}

if (is.null(user_coordinates)){
  extent <- raster::extent(shapefile)
  extent_bbox <- sp::bbox(extent)
}

# the following is only for the case that coordinates of the shapefile are not in longlat format - in that case transform it into longlat
shapefileprojection <- suppressWarnings(raster::crs(shapefile))
shapefileprojection <- as.character(shapefileprojection)
if (length(shapefileprojection[grep(shapefileprojection, pattern = "longlat")]) == 0){
  shapefile <- suppressWarnings(sp::spTransform(shapefile, CRSobj = "+init=epsg:4326"))
  extent <- raster::extent(shapefile)
  extent_bbox <- sp::bbox(extent)
}

# search for tiles on which the shapefile is located
if (lightdata_time == "monthly"){
  tilenumbers <- c()
  if (extent_bbox[2,2] > 0 & c(extent_bbox[1,1] < -60 | extent_bbox[1,2] < -60)){
    tilenumbers <- append(tilenumbers, "1")
  }
  if (extent_bbox[2,2] > 0 & c(c(extent_bbox[1,1] > -60 & extent_bbox[1,1] < 60) | c(extent_bbox[1,2] > -60 & extent_bbox[1,2] < 60))){
    tilenumbers <- append(tilenumbers, "2")
  }
  if (extent_bbox[2,2] > 0 & c(extent_bbox[1,1] > 60 | extent_bbox[1,2] > 60)){
    tilenumbers <- append(tilenumbers, "3")
  }
  if (extent_bbox[2,1] < 0 & c(extent_bbox[1,1] < -60 | extent_bbox[1,2] < -60)){
    tilenumbers <- append(tilenumbers, "4")
  }
  if (extent_bbox[2,1] < 0 & c(c(extent_bbox[1,1] > -60 & extent_bbox[1,1] < 60) | c(extent_bbox[1,2] > -60 & extent_bbox[1,2] < 60))){
    tilenumbers <- append(tilenumbers, "5")
  }
  if (extent_bbox[2,1] < 0 & c(extent_bbox[1,1] > 60 | extent_bbox[1,2] > 60)){
    tilenumbers <- append(tilenumbers, "6")
  }
  if (length(tilenumbers) > 1){
    overlapping_tile <- TRUE
  } else if (length(tilenumbers == 1)){
    overlapping_tile <- FALSE
  }
} else if (lightdata_time == "yearly"){
  tilenumbers <- c("")
  overlapping_tile <- FALSE
}
