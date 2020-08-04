nightlight_calculate <- function(area_names,
                                 time,
                                 light_location,
                                 shapefile_location = NULL,
                                 admlevel = 0,
                                 shapefiles = NULL,
                                 download_shape = "sp.rds",
                                 dmsp_oldversion = FALSE,
                                 single_dataframes = FALSE,
                                 functions_calculate = NULL,
                                 rectangle_calculate = FALSE,
                                 rawdata = FALSE,
                                 cut_low = NULL,
                                 cut_high = NULL,
                                 cut_quality = 0,
                                 user_coordinates = NULL){

  rawdata_lights <- NULL # "nulling out" i.e. setting the variables to NULL first and then using them later, only to bind variables so there is no "binding for global variables" problem later

  if (is.null(functions_calculate)){
    functions_calculate <- c("sum", "min", "mean", "max")
  }

  if (is.null(shapefile_location) & is.null(user_coordinates)){
    stop("Please input geographical information, either by providing shapefiles (do not forget to use shapefile_location to specifiy the location on your drive) or a set of coordinates.")
  }

  time <- as.character(time) # need this as character to build strings later
  if (length(time) == 1){
    time_from <- time
    time_to <- time
  } else if (length(time) == 2){
    time_from <- time[1]
    time_to <- time[2]
  }

  lightdata_time <- "monthly" # set default to monthly

  if (nchar(time_from) == 4 & nchar(time_to) == 4){
    lightdata_time <- "yearly"
  } # change to yearly if year is given (only possibility for 4 characters)

  if (length(area_names == 1) & length(shapefiles) > 1){
    area_names <- rep(area_names, length = length(shapefiles))
    for (l in 1:length(shapefiles)){
      area_names[l] <- paste0(area_names[l], "_", l)
    }
  } # if multiple shapefiles but only one area name provided: give that name to all shapefiles

  if (!is.null(shapefiles)){
    shapefiles <- paste0(shapefile_location, "/", shapefiles)
    help_shapefiles <- shapefiles
  } else if (is.null(shapefiles)){
    help_shapefiles <- NA
  } # help_shapefiles is a duplicate of shapefiles for later if provided by the user, otherwise NA

  # create the time sequence to perform the loop for each period between "from" and "to"
  # + keep monthly data standardized in yearmonth format
  if (lightdata_time == "monthly"){
    time_from <- zoo::as.yearmon(time_from)
    time_to <- zoo::as.yearmon(time_to)
    time_from <- zoo::as.Date.yearmon(time_from)
    time_to <- zoo::as.Date.yearmon(time_to)
    sequence <- seq(time_from, time_to, by = "mon")
    sequence <- zoo::as.yearmon(sequence)
  } else if (lightdata_time == "yearly"){
    sequence <- as.character(seq(time_from, time_to, by = 1))
  }


  for (i in 1:length(area_names)){

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


    if (rectangle_calculate == TRUE & is.null(user_coordinates)){
      crs <- suppressWarnings(raster::crs(shapefile))
      shapefile <- as(extent, "SpatialPolygons")
      raster::crs(shapefile) <- crs
      shapefile <- sp::SpatialPolygonsDataFrame(shapefile, data.frame(N = c("1"), row.names = c("1")))
    } # transform non-enclosed shapefile into a rectangle if user sets rectangle_calculate to TRUE

    area <- (suppressWarnings(raster::area(shapefile))) / (1000^2) # area is output in sq meters; convert to sq kilometers
    # at this point we have either an enclosed shapefile or a rectangle. hence, area can be calculated
    # for lower admlevels, area will output as many values as there are regions of the lowest admlevel

    if (lightdata_time == "monthly"){
      all_aggregated <- data.frame(area_name = "",
                                   iso3 = "",
                                   area_km2 = 0,
                                   yearmonth = "",
                                   mean_obs = 0)
    } else if (lightdata_time == "yearly"){
      all_aggregated <- data.frame(area_name = "",
                                   iso3 = "",
                                   area_km2 = 0,
                                   year = 0,
                                   mean_obs = 0)
    }

    if (!is.null(shapefile$NAME_1)){
      all_aggregated$NAME_1 <- ""
    }

    if (!is.null(shapefile$NAME_2)){
      all_aggregated$NAME_2 <- ""
    }

    if (!is.null(shapefile$NAME_3)){
      all_aggregated$NAME_3 <- ""
    }

    if (!is.null(shapefile$NAME_4)){
      all_aggregated$NAME_4 <- ""
    }

    if (!is.null(shapefile$NAME_5)){
      all_aggregated$NAME_5 <- ""
    }

    for (k in 1:length(functions_calculate)){
      current_function_name <- functions_calculate[k]
      name_df <- data.frame(current_function_name = 0)
      colnames(name_df)[colnames(name_df) == "current_function_name"] <- current_function_name
      all_aggregated <- cbind(all_aggregated, name_df)
    }

    all_aggregated <- all_aggregated[-c(1),]

    for (j in 1:length(sequence)){

      # build a string out of date and search for lightdata that matches this date
      if (lightdata_time == "monthly"){

        year <- as.character(data.table::year(sequence[j]))
        numericyear <- as.numeric(year)
        month <- as.character(data.table::month(sequence[j]))

        if (nchar(month) == 1){
          month <- paste0("0", month)
        } # month needs to be in 2-digit format for following lightfile-search string

        yearmonth <- paste0(year, month)

        if (month == "01" | month == "03" | month == "05" | month == "07" | month == "08" | month == "10" | month == "12"){
          numberdays <- c("31")
        } else if (month == "04" | month == "06" | month == "09" | month == "11"){
          numberdays <- c("30")
        } else if (month == "02" & numericyear %% 4 == 0 & numericyear %% 100 != 0){
          numberdays <- c("29")
        } else if (month == "02" & numericyear %% 400 == 0){
          numberdays <- c("29")
        } else {
          numberdays <- c("28")
        }
        yearmonthspan <- paste0(yearmonth, "01-", yearmonth, numberdays)

      } else if (lightdata_time == "yearly"){
        year <- sequence[j]
      }

      for (t in 1:length(tilenumbers)){

        tilenumber <- tilenumbers[t]

        if (tilenumber == "1"){
          tilestump <- "75N180W"
        } else if (tilenumber == "2"){
          tilestump <- "75N060W"
        } else if (tilenumber == "3"){
          tilestump <- "75N060E"
        } else if (tilenumber == "4"){
          tilestump <- "00N180W"
        } else if (tilenumber == "5"){
          tilestump <- "00N060W"
        } else if (tilenumber == "6"){
          tilestump <- "00N060E"
        }

        if (lightdata_time == "monthly"){
          list_light <-  list.files(paste0(light_location), pattern = "avg_rade9")
          lightfile <- list_light[grep(".tif", list_light)]
          lightfile <- lightfile[grep(yearmonthspan, lightfile)]
          lightfile <- lightfile[grep(tilestump, lightfile)]
          lightfile <- paste0(light_location, "/", lightfile)
          lightfile <- raster::raster(lightfile)
          lightfile <- raster::crop(lightfile, extent)
          if (t == 1){
            lightdata <- lightfile
          }
          if (overlapping_tile == TRUE & t > 1){
            lightdata <- raster::merge(lightdata, lightfile)
          }

          list_quality <- list.files(paste0(light_location), pattern = "cf_cvg")
          qualityfile <- list_quality[grep(".tif", list_quality)]
          qualityfile <- qualityfile[grep(yearmonthspan, qualityfile)]
          qualityfile <- qualityfile[grep(tilestump, qualityfile)]
          qualityfile <- paste0(light_location, "/", qualityfile)
          qualityfile <- raster::raster(qualityfile)
          qualityfile <- raster::crop(qualityfile, extent)
          if (t == 1){
            qualitydata <- qualityfile
          }
          if (overlapping_tile == TRUE & t > 1){
            qualitydata <- raster::merge(qualitydata, qualityfile)
          }
          rm(lightfile)
          rm(qualityfile)

        } else if (lightdata_time == "yearly"){

          if (year == "1992" |
              year == "1993"){
            dmsp_stump <- "F10"
          } else if (year == "1994"){
            if (dmsp_oldversion == TRUE){
              dmsp_stump <- "F10"
            } else if (dmsp_oldversion == FALSE){
              dmsp_stump <- "F12"
            }
          } else if (year == "1995" |
                     year == "1996"){
            dmsp_stump <- "F12"
          } else if (year == "1997" |
                     year == "1998" |
                     year == "1999"){
            if (dmsp_oldversion == TRUE){
              dmsp_stump <- "F12"
            } else if (dmsp_oldversion == FALSE){
              dmsp_stump <- "F14"
            }
          } else if (year == "2000" |
                     year == "2001" |
                     year == "2002" |
                     year == "2003"){
            if (dmsp_oldversion == TRUE){
              dmsp_stump <- "F14"
            } else if (dmsp_oldversion == FALSE){
              dmsp_stump <- "F15"
            }
          } else if (year == "2004" |
                     year == "2005" |
                     year == "2006" |
                     year == "2007"){
            if (dmsp_oldversion == TRUE){
              dmsp_stump <- "F15"
            } else if (dmsp_oldversion == FALSE){
              dmsp_stump <- "F16"
            }
          } else if (year == "2008" |
                     year == "2009"){
            dmsp_stump <- "F16"
          } else if (year == "2010" |
                     year == "2011" |
                     year == "2012" |
                     year == "2013"){
            dmsp_stump <- "F18"
          }

          list_light <-  list.files(light_location, pattern = "avg_vis.tif")
          list_light <-  list_light[grep("stable", list_light)]
          list_light <-  list_light[grep(dmsp_stump, list_light)]
          lightfile <- list_light[grep(year, list_light)]
          lightdata <- paste0(light_location, "/", lightfile)
          lightdata <- raster::raster(lightdata)
          lightdata <- raster::crop(lightdata, extent)

          list_quality <- list.files(light_location, pattern = "cf_cvg.tif")
          list_quality <- list_quality[grep(dmsp_stump, list_quality)]
          qualityfile <- list_quality[grep(year, list_quality)]
          qualitydata <- paste0(light_location, "/", qualityfile)
          qualitydata <- raster::raster(qualitydata)
          qualitydata <- raster::crop(qualitydata, extent)
        }
      }

      lightdata[qualitydata <= cut_quality] <- NA


      if (!is.null(cut_low)){
        lightdata[lightdata < cut_low] <- NA
        qualitydata[lightdata < cut_low] <- NA
      }
      if (!is.null(cut_high)){
        lightdata[lightdata > cut_high] <- NA
        qualitydata[lightdata > cut_high] <- NA
      }

      if (lightdata_time == "monthly"){
        aggregated <- data.frame(area_name = "",
                                 iso3 = "",
                                 area_km2 = 0,
                                 yearmonth = "")
      } else if (lightdata_time == "yearly"){
        aggregated <- data.frame(area_name = "",
                                 iso3 = "",
                                 area_km2 = 0,
                                 year = 0)
      }

      aggregated <- aggregated[-c(1),]

      # set the number of rows for the output dataframe according to the number of the lowest-order subregional division (5 divisions are the most finely divided divisions)
      if (!is.null(shapefile$NAME_5)){
        aggregated[nrow(aggregated) + length(shapefile$NAME_5),] <- NA
      } else if (is.null(shapefile$NAME_5) &
                 !is.null(shapefile$NAME_4)){
        aggregated[nrow(aggregated) + length(shapefile$NAME_4),] <- NA
      } else if (is.null(shapefile$NAME_5) &
                 is.null(shapefile$NAME_4) &
                 !is.null(shapefile$NAME_3)){
        aggregated[nrow(aggregated) + length(shapefile$NAME_3),] <- NA
      } else if (is.null(shapefile$NAME_5) &
                 is.null(shapefile$NAME_4) &
                 is.null(shapefile$NAME_3) &
                 !is.null(shapefile$NAME_2)){
        aggregated[nrow(aggregated) + length(shapefile$NAME_2),] <- NA
      } else if (is.null(shapefile$NAME_5) &
                 is.null(shapefile$NAME_4) &
                 is.null(shapefile$NAME_3) &
                 is.null(shapefile$NAME_2) &
                 !is.null(shapefile$NAME_1)){
        aggregated[nrow(aggregated) + length(shapefile$NAME_1),] <- NA
      } else if (is.null(shapefile$NAME_5) &
                 is.null(shapefile$NAME_4) &
                 is.null(shapefile$NAME_3) &
                 is.null(shapefile$NAME_2) &
                 is.null(shapefile$NAME_1)){
        aggregated[nrow(aggregated) + 1,] <- NA
      }

      if (!is.null(shapefile$NAME_5)){
        aggregated$NAME_5 <- shapefile$NAME_5
      }

      if (!is.null(shapefile$NAME_4)){
        aggregated$NAME_4 <- shapefile$NAME_4
      }

      if (!is.null(shapefile$NAME_3)){
        aggregated$NAME_3 <- shapefile$NAME_3
      }

      if (!is.null(shapefile$NAME_2)){
        aggregated$NAME_2 <- shapefile$NAME_2
      }

      if (!is.null(shapefile$NAME_1)){
        aggregated$NAME_1 <- shapefile$NAME_1
      }

      aggregated$area_name <- area_name
      aggregated$iso3 <- ISO3
      aggregated$area_km2 <- area
      if (lightdata_time == "monthly"){
        aggregated$yearmonth <- paste0(year, "-", month)
      } else if (lightdata_time == "yearly"){
        aggregated$year <- as.numeric(year)
      }

      aggregated$mean_obs <- suppressWarnings(raster::extract(qualitydata, shapefile, fun = mean, na.rm = TRUE))

      for (k in 1:length(functions_calculate)){
        current_function_name <- functions_calculate[k]
        function_df <- data.frame(current_function_name = suppressWarnings(raster::extract(lightdata, shapefile, fun = get(functions_calculate[[k]]), na.rm = TRUE)))
        colnames(function_df)[colnames(function_df) == "current_function_name"] <- functions_calculate[k]
        aggregated <- cbind(aggregated, function_df)
      }

      all_aggregated <- rbind(all_aggregated, aggregated)

      if (rawdata == TRUE){
        rawdata_values <- data.frame(suppressWarnings(raster::extract(lightdata, shapefile, cellnumbers = TRUE)))
        rawdata_values <- cbind(rawdata_values, raster::coordinates(lightdata)[rawdata_values[,1],])
        rawdata_lights <<- rawdata_values # load into global environment with name rawdata_lights
      }

      if (single_dataframes == TRUE){
        single_environment_name <- paste0(area_name, "_lights")
        assign(single_environment_name, all_aggregated, envir = .GlobalEnv)
      } # if user sets this to TRUE, the output will not only be an aggregated dataframe for all countries, but also an additional one for each country

      if (i == 1){
        lights <- all_aggregated
        # build dataframe called lights with all areas, start with first area, which means i = 1
      } else if (i > 1){
        lights <- rbind(lights, aggregated)
        # bind to the lights dataframe if area loop is at least at the second iteration
      }

    }

    rm(area_name)
    rm(ISO3)
    shapefile <- NULL
    help_shapefile <- NULL

  }

  lights <<- lights # load aggregated dataframe with all regions into global environment

}
