nightlight_calculate <- function(area_names,
                                 time,
                                 light_location,
                                 shapefile_location = NULL,
                                 shapefiles = NULL,
                                 download_shape = "sp.rds",
                                 admlevel = 0,
                                 single_dataframes = FALSE,
                                 functions_calculate = NULL,
                                 rectangle_calculate = NULL,
                                 rawdata = FALSE,
                                 cut_low = NULL,
                                 cut_high = NULL,
                                 cut_quality = 0,
                                 user_coordinates = NULL,
                                 corrected_lights = FALSE,
                                 harmonized_lights = FALSE){

  if (is.null(functions_calculate)){
    functions_calculate <- c("sum", "min", "mean", "max")
  }

  if (is.null(shapefile_location) & is.null(user_coordinates)){
    stop("Please input geographical information, either by providing shapefiles (do not forget to use shapefile_location to specifiy the location on your drive) or a set of coordinates.")
  }


  # begin sourcefile "setup"


  # need the locations without "/" at the end. in case user types this with "/", remove that
  if (substr(light_location,
             start = nchar(light_location),
             stop = nchar(light_location)) == "/"){
    light_location <- substr(light_location, 1, nchar(light_location) - 1)
  }

  if(!is.null(shapefile_location)){
    if (substr(shapefile_location,
               start = nchar(shapefile_location),
               stop = nchar(shapefile_location)) == "/"){
      shapefile_location <- substr(shapefile_location, 1, nchar(shapefile_location) - 1)
    }
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

  if (length(area_names) == 1 & length(shapefiles) > 1){
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


  # end sourcefile "setup"


  for (i in 1:length(area_names)){


    # begin sourcefile "get_shapefile"


    help_shapefile <- help_shapefiles[i]
    shapefile <- shapefiles[i]
    # these are either provided by the user and hence activated at this point
    # or shapefile will be NULL and help_shapefile will be NA and shapefile will be detected or downloaded later

    # if shapefile available at this point, its type is detected and it is read with the according function

    if (!is.na(help_shapefile)){
      if (length(grep(help_shapefile, pattern = ".rds")) != 0){
        shapefile <- readRDS(shapefile)
        if (class(shapefile)[1] == "sf"){
          stop("Unfortunately, the function does not work with sf.rds shapefiles. If no other option is available to you, you can try using the min/max x- and y-coordinates of your shapefile in the function input instead.")
        }
      } else if (length(grep(help_shapefile, pattern = ".shp")) != 0 |
                 length(grep(help_shapefile, pattern = ".kml")) != 0){
        shapefile <- rgdal::readOGR(shapefile)
      } else if (length(grep(help_shapefile, pattern = ".gpkg")) != 0){
        layers <- rgdal::ogrListLayers(help_shapefile)
        shapefile <- rgdal::readOGR(help_shapefile, layers[admlevel + 1])
      } else {
        stop("Unfortunately, the function does not work the format of your shapefile. If no other option is available to you, you can try using the min/max x- and y-coordinates of your shapefile in the function input instead.")
      }
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

    area_name <- area_names[i]
    ISO3 <- ISO3s[i]

    if (is.na(ISO3)){
      print(paste0("There is no iso3c countrycode for ", area_name, ", hence download from GADM will not work. Either your shapefile is not a country or, if it is a country, the countryname was not recognized correctly."))
    }

    # check if shapefile is already downloaded
    # check for every format. will preferably find
    # sp.rds, then .shp, .kml, .gpkg in this order

    # first: check for GADM shapefiles which are identified by ISO3 and admlevel
    if (!is.na(ISO3)){ # need this if condition because otherwise some files are detected by ISO3 = NA. for some reason only .gkpg files
      # sp.rds
      if (is.null(shapefile)){
        help_shapefile <- list.files(shapefile_location, pattern = ".rds")
        help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
        help_shapefile <- help_shapefile[grep(help_shapefile, pattern = admlevel)]
        if (length(help_shapefile) == 1){
          shapefile <- readRDS(paste0(shapefile_location, "/", help_shapefile))
          if (class(shapefile)[1] == "sf"){
            stop("Unfortunately, the function does not work with sf.rds shapefiles. If no other option is available to you, you can try using the min/max x- and y-coordinates of your shapefile in the function input instead.")
          }
        } else if (length(help_shapefile) > 1){
          stop(paste0("Please enter the filename of your .rds shapefile for ", area_name, " by hand using the shapefiles argument. It could not be loaded automatically.") )
        }
      }
      # .shp
      if (is.null(shapefile)){
        help_shapefile <- list.files(shapefile_location, pattern = ".shp")
        help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
        help_shapefile <- help_shapefile[grep(help_shapefile, pattern = admlevel)]
        if (length(help_shapefile) == 1){
          shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", help_shapefile))
        } else if (length(help_shapefile) > 1){
          stop(paste0("Please enter the filename of your .shp shapefile for ", area_name, " by hand using the shapefiles argument. It could not be loaded automatically.") )
        }
      }
      # .shp but still zipped as .zip
      if (is.null(shapefile)){
        help_shapefile <- list.files(shapefile_location, pattern = "shp.zip")
        help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
        if (length(help_shapefile) == 1){
          help_shapefile <- utils::unzip(zipfile = paste0(shapefile_location, "/", help_shapefile), exdir = shapefile_location)
          help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ".shp")]
          help_shapefile <- help_shapefile[grep(help_shapefile, pattern = admlevel)]
          shapefile <- rgdal::readOGR(help_shapefile)
          zipfile <- list.files(shapefile_location, pattern = "shp.zip")
          zipfile <- zipfile[grep(zipfile, pattern = ISO3)]
          unlink(paste0(shapefile_location, "/", zipfile), recursive = TRUE)
        } else if (length(help_shapefile) > 1){
          stop(paste0("Please enter the filename of your .shp shapefile for ", area_name, " by hand using the shapefiles argument. It could not be loaded automatically.") )
        }
      }
      # .kml
      if (is.null(shapefile)){
        help_shapefile <- list.files(shapefile_location, pattern = ".kml")
        help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
        help_shapefile <- help_shapefile[grep(help_shapefile, pattern = admlevel)]
        if (length(help_shapefile) == 1){
          shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", help_shapefile))
        } else if (length(help_shapefile) > 1){
          stop(paste0("Please enter the filename of your .kml shapefile for ", area_name, " by hand using the shapefiles argument. It could not be loaded automatically.") )
        }
      }
      # .kml but still zipped as .kmz
      if (is.null(shapefile)){
        help_shapefile <- list.files(shapefile_location, pattern = ".kmz")
        help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
        if (length(help_shapefile) == 1){
          help_shapefile <- utils::unzip(zipfile = paste0(shapefile_location, "/", help_shapefile), exdir = shapefile_location)
          shapefile <- rgdal::readOGR(help_shapefile)
          zipfile <- list.files(shapefile_location, pattern = ".kmz")
          zipfile <- zipfile[grep(zipfile, pattern = ISO3)]
          unlink(paste0(shapefile_location, "/", zipfile), recursive = TRUE)
        } else if (length(help_shapefile) > 1){
          stop(paste0("Please enter the filename of your .kml shapefile for ", area_name, " by hand using the shapefiles argument. It could not be loaded automatically.") )
        }
      }
      # .gpkg
      if (is.null(shapefile)){
        help_shapefile <- list.files(shapefile_location, pattern = ".gpkg")
        help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
        if (length(help_shapefile) == 1){
          layers <- rgdal::ogrListLayers(paste0(shapefile_location, "/", help_shapefile))
          shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", help_shapefile), layers[admlevel + 1])
        } else if (length(help_shapefile) > 1){
          stop(paste0("Please enter the filename of your .gpkg shapefile for ", area_name, " by hand using the shapefiles argument. It could not be loaded automatically.") )
        }
      }
      # .gpkg but still zipped as .zip
      if (is.null(shapefile)){
        help_shapefile <- list.files(shapefile_location, pattern = "gpkg.zip")
        help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
        if (length(help_shapefile) == 1){
          help_shapefile <- utils::unzip(zipfile = paste0(shapefile_location, "/", help_shapefile), exdir = shapefile_location)
          help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ".gpkg")]
          layers <- rgdal::ogrListLayers(help_shapefile)
          shapefile <- rgdal::readOGR(help_shapefile, layers[admlevel + 1])
          zipfile <- list.files(shapefile_location, pattern = "gpkg.zip")
          zipfile <- zipfile[grep(zipfile, pattern = ISO3)]
          unlink(paste0(shapefile_location, "/", zipfile), recursive = TRUE)
        } else if (length(help_shapefile) > 1){
          stop(paste0("Please enter the filename of your .gpkg shapefile for ", area_name, " by hand using the shapefiles argument. It could not be loaded automatically.") )
        }
      }
    }

    # then: check for shapefiles that are identified by the name given by the user in area_names
    # sp.rds
    if (is.null(shapefile)){
      help_shapefile <- list.files(shapefile_location, pattern = ".rds")
      help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
      if (length(help_shapefile) == 1){
        shapefile <- readRDS(paste0(shapefile_location, "/", help_shapefile))
        if (class(shapefile)[1] == "sf"){
          stop("Unfortunately, the function does not work with sf.rds shapefiles. If no other option is available to you, you can try using the min/max x- and y-coordinates of your shapefile in the function input instead.")
        }
      } else if (length(help_shapefile) > 1){
        stop(paste0("Please enter the filename of your .rds shapefile for ", area_name, " by hand using the shapefiles argument. It could not be loaded automatically.") )
      }
    }
    # .shp
    if (is.null(shapefile)){
      help_shapefile <- list.files(shapefile_location, pattern = ".shp")
      help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
      if (length(help_shapefile) == 1){
        shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", help_shapefile))
      } else if (length(help_shapefile) > 1){
        stop(paste0("Please enter the filename of your .shp shapefile for ", area_name, " by hand using the shapefiles argument. It could not be loaded automatically.") )
      }
    }
    # .shp but still zipped as .zip
    if (is.null(shapefile)){
      help_shapefile <- list.files(shapefile_location, pattern = "shp.zip")
      help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
      if (length(help_shapefile) == 1){
        help_shapefile <- utils::unzip(zipfile = paste0(shapefile_location, "/", help_shapefile), exdir = shapefile_location)
        help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ".shp")]
        if (length(help_shapefile) == 1){
          shapefile <- rgdal::readOGR(help_shapefile)
          zipfile <- list.files(shapefile_location, pattern = "shp.zip")
          zipfile <- zipfile[grep(zipfile, pattern = area_name)]
          unlink(paste0(shapefile_location, "/", zipfile), recursive = TRUE)
        } else if (length(help_shapefile) > 1){
          stop(paste0("Please enter the filename of your .gpkg shapefile for ", area_name, " by hand using the shapefiles argument. It could not be loaded automatically.") )
        }
      } else if (length(help_shapefile) > 1){
        stop(paste0("Please enter the filename of your .gpkg shapefile for ", area_name, " by hand using the shapefiles argument. It could not be loaded automatically.") )
      } # the outer error message refers to non-unique shapefile zip names, the inner one to non-unique shapefile names when unpacked (for gadm shapefiles above, this is unique because they always have the admlevel in the name as well. but if the code searches for adm level here, shapefiles with no admlevel e.g. cities are excluded)
    }
    # .kml
    if (is.null(shapefile)){
      help_shapefile <- list.files(shapefile_location, pattern = ".kml")
      help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
      if (length(help_shapefile) == 1){
        shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", help_shapefile))
      } else if (length(help_shapefile) > 1){
        stop(paste0("Please enter the filename of your .kml shapefile for ", area_name, " by hand using the shapefiles argument. It could not be loaded automatically.") )
      }
    }
    # .kml but still zipped as .kmz
    if (is.null(shapefile)){
      help_shapefile <- list.files(shapefile_location, pattern = ".kmz")
      help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
      if (length(help_shapefile) == 1){
        help_shapefile <- utils::unzip(zipfile = paste0(shapefile_location, "/", help_shapefile), exdir = shapefile_location)
        shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", help_shapefile))
        zipfile <- list.files(shapefile_location, pattern = ".kmz")
        zipfile <- zipfile[grep(zipfile, pattern = area_name)]
        unlink(paste0(shapefile_location, "/", zipfile), recursive = TRUE)
      } else if (length(help_shapefile) > 1){
        stop(paste0("Please enter the filename of your .kml shapefile for ", area_name, " by hand using the shapefiles argument. It could not be loaded automatically.") )
      } # for kmz there is only one shapefile per zipfile so only 1 error message is necessary
    }
    # .gpkg
    if (is.null(shapefile)){
      help_shapefile <- list.files(shapefile_location, pattern = ".gpkg")
      help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
      if (length(help_shapefile) == 1){
        layers <- rgdal::ogrListLayers(paste0(shapefile_location, "/", help_shapefile))
        shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", help_shapefile), layers[admlevel + 1])
      } else if (length(help_shapefile) > 1){
        stop(paste0("Please enter the filename of your .gpkg shapefile for ", area_name, " by hand using the shapefiles argument. It could not be loaded automatically.") )
      }
    }
    # .gpkg but still zipped as .zip
    if (is.null(shapefile)){
      help_shapefile <- list.files(shapefile_location, pattern = "gpkg.zip")
      help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
      if (length(help_shapefile) == 1){
        help_shapefile <- utils::unzip(zipfile = paste0(shapefile_location, "/", help_shapefile), exdir = shapefile_location)
        help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ".gpkg")]
        layers <- rgdal::ogrListLayers(help_shapefile)
        shapefile <- rgdal::readOGR(help_shapefile, layers[admlevel + 1])
        zipfile <- list.files(shapefile_location, pattern = "gpkg.zip")
        zipfile <- zipfile[grep(zipfile, pattern = area_name)]
        unlink(paste0(shapefile_location, "/", zipfile), recursive = TRUE)
      } else if (length(help_shapefile) > 1){
        stop(paste0("Please enter the filename of your .gpkg shapefile for ", area_name, " by hand using the shapefiles argument. It could not be loaded automatically.") )
      }
    }

    # if shapefile is not downloaded yet: download from GADM
    stumpurl1 <- "https://biogeo.ucdavis.edu/data/gadm3.6/"
    stumpurl2 <- "/gadm36_"
    # sp.rds
    if (is.null(shapefile) & download_shape == "sp.rds"){
      utils::download.file(paste0(stumpurl1, "Rsp", stumpurl2, ISO3, "_", admlevel, "_sp.rds"),
                           destfile = paste0(shapefile_location, "/", "gadm36_", ISO3, "_", admlevel, "_sp.rds"), mode = "wb")
      help_shapefile <- paste0(shapefile_location, "/", "gadm36_", ISO3, "_", admlevel, "_sp.rds")
      shapefile <- readRDS(help_shapefile)
    }
    # .shp
    if (is.null(shapefile) & download_shape == ".shp"){
      utils::download.file(paste0(stumpurl1, "shp", stumpurl2, ISO3, "_shp.zip"),
                           destfile = paste0(shapefile_location, "/", "gadm36_", ISO3, "_shp.zip"), mode = "wb")
      help_shapefile <- paste0(shapefile_location, "/", "gadm36_", ISO3, "_shp.zip")
      help_shapefile <- utils::unzip(zipfile = help_shapefile, exdir = shapefile_location)
      help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ".shp")]
      help_shapefile <- help_shapefile[grep(help_shapefile, pattern = admlevel)]
      shapefile <- rgdal::readOGR(help_shapefile)
      unlink(paste0(shapefile_location, "/", "gadm36_", ISO3, "_shp.zip"), recursive = TRUE)
    }
    # .kml
    if (is.null(shapefile) & download_shape == ".kml"){
      utils::download.file(paste0(stumpurl1, "kmz", stumpurl2, ISO3, "_", admlevel, ".kmz"),
                           destfile = paste0(shapefile_location, "/",  "gadm36_", ISO3, "_", admlevel, ".kmz"), mode = "wb")
      help_shapefile <- paste0(shapefile_location, "/", "gadm36_", ISO3, "_", admlevel, ".kmz")
      help_shapefile <- utils::unzip(zipfile = help_shapefile, exdir = shapefile_location)
      shapefile <- rgdal::readOGR(help_shapefile)
      unlink(paste0(shapefile_location, "/", "gadm36_", ISO3, "_", admlevel, ".kmz"), recursive = TRUE)
    }
    # .gpkg
    if (is.null(shapefile) & download_shape == ".gpkg"){
      utils::download.file(paste0(stumpurl1, "gpkg", stumpurl2 , ISO3, "_gpkg.zip"),
                           destfile = paste0(shapefile_location, "/",  "gadm36_", ISO3, "_gpkg.zip"), mode = "wb")
      help_shapefile <- paste0(shapefile_location, "/", "gadm36_", ISO3, "_gpkg.zip")
      help_shapefile <- utils::unzip(zipfile = help_shapefile, exdir = shapefile_location)
      help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ".gpkg")]
      layers <- rgdal::ogrListLayers(help_shapefile)
      shapefile <- rgdal::readOGR(help_shapefile, layers[admlevel + 1])
      unlink(paste0(shapefile_location, "/", "gadm36_", ISO3, "_gpkg.zip"), recursive = TRUE)
    }

    # the following is only for the case that coordinates of the shapefile are not in longlat format - in that case transform it into longlat
    shapefileprojection <- suppressWarnings(raster::crs(shapefile))
    shapefileprojection <- as.character(shapefileprojection)
    if (length(shapefileprojection[grep(shapefileprojection, pattern = "longlat")]) == 0){
      shapefile <- suppressWarnings(sp::spTransform(shapefile, CRSobj = "+init=epsg:4326"))
    }

    if (is.null(user_coordinates)){
      extent <- raster::extent(shapefile)
      extent_bbox <- sp::bbox(extent)
    }

    xmin = extent_bbox[1,1]
    xmax = extent_bbox[1,2]
    ymin = extent_bbox[2,1]
    ymax = extent_bbox[2,2]

    # search for tiles on which the shapefile is located
    if (lightdata_time == "monthly"){
      tilenumbers <- c()
      if (ymax > 0 & c(xmin < -60 | xmax < -60)){
        tilenumbers <- append(tilenumbers, "1")
      }
      if (ymax > 0 & c(c(xmin > -60 & xmin < 60) | c(xmax > -60 & xmax < 60))){
        tilenumbers <- append(tilenumbers, "2")
      }
      if (ymax > 0 & c(xmin > 60 | xmax > 60)){
        tilenumbers <- append(tilenumbers, "3")
      }
      if (ymin < 0 & c(xmin < -60 | xmax < -60)){
        tilenumbers <- append(tilenumbers, "4")
      }
      if (ymin < 0 & c(c(xmin > -60 & xmin < 60) | c(xmax > -60 & xmax < 60))){
        tilenumbers <- append(tilenumbers, "5")
      }
      if (ymin < 0 & c(xmin > 60 | xmax > 60)){
        tilenumbers <- append(tilenumbers, "6")
      }
    }


    # end sourcefile "get_shapefile"


    # in case user did not set rectangle to TRUE: check for enclosed area and set automatically
    if (is.null(rectangle_calculate)){
      if (admlevel != 0){
        rectangle_calculate <- FALSE
      } else if (admlevel == 0){
        if (class(shapefile) != "SpatialPolygonsDataFrame"){
          rectangle_calculate <-  TRUE
        } else if (class(shapefile) == "SpatialPolygonsDataFrame"){
          help_area <- suppressWarnings(raster::area(shapefile))
          if (length(help_area) == 1){
            if (!is.na(help_area)){
              rectangle_calculate <-  FALSE
            } else if (is.na(help_area)){
              rectangle_calculate <-  TRUE
            }
          } else if (length(help_area) != 1){
            rectangle_calculate <-  TRUE
          }
        }
      }
    }

    if (rectangle_calculate == TRUE & is.null(user_coordinates)){
      crs <- suppressWarnings(raster::crs(shapefile))
      shapefile <- as(extent, "SpatialPolygons")
      raster::crs(shapefile) <- crs
      shapefile <- sp::SpatialPolygonsDataFrame(shapefile, data.frame(N = c("1"), row.names = c("1")))
      print(paste0("The shapefile for ", area_name , " features a non-enclosed area (or you have activated rectangle_calculate manually). The calculations will be performed on a rectangular version of the shapefile defined by its minimum and maximum extent."))
    } # transform non-enclosed shapefile into a rectangle if rectangle_calculate is TRUE

    area <- (suppressWarnings(raster::area(shapefile))) / (1000^2) # area is output in sq meters; convert to sq kilometers
    # at this point we have either an enclosed shapefile or a rectangle. hence, area can be calculated
    # for lower admlevels, area will output as many values as there are regions of the lowest admlevel

    if (lightdata_time == "monthly"){
      all_aggregated <- data.frame(area_name = "",
                                   iso3c = "",
                                   area_km2 = 0,
                                   yearmonth = "")
    } else if (lightdata_time == "yearly"){
      all_aggregated <- data.frame(area_name = "",
                                   iso3c = "",
                                   area_km2 = 0,
                                   year = 0)
    }

    if (harmonized_lights == FALSE){
      all_aggregated$mean_obs = 0
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


      # begin sourcefile "get_lightfile"


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

          list_light <-  list.files(paste0(light_location), pattern = "avg_rade9")
          lightfile <- list_light[grep(".tif", list_light)]
          lightfile <- lightfile[grep(yearmonthspan, lightfile)]
          lightfile <- lightfile[grep(tilestump, lightfile)]
          if (corrected_lights == FALSE){
            lightfile <- lightfile[grep("vcmcfg", lightfile)]
          } else if (corrected_lights == TRUE){
            lightfile <- lightfile[grep("vcmslcfg", lightfile)]
          }
          if (length(lightfile) == 1){
            lightfile <- paste0(light_location, "/", lightfile)
            lightfile <- raster::raster(lightfile)
            lightfile <- raster::crop(lightfile, extent)
            if (t == 1){
              lightdata <- lightfile
            } else if (t > 1){
              lightdata <- raster::merge(lightdata, lightfile)
            }
          } else {
            stop(paste0("The light file for ", month, "/", year, " could not be found in your light location."))
          }

          list_quality <- list.files(paste0(light_location), pattern = "cf_cvg")
          qualityfile <- list_quality[grep(".tif", list_quality)]
          qualityfile <- qualityfile[grep(yearmonthspan, qualityfile)]
          qualityfile <- qualityfile[grep(tilestump, qualityfile)]
          if (corrected_lights == FALSE){
            qualityfile <- qualityfile[grep("vcmcfg", qualityfile)]
          } else if (corrected_lights == TRUE){
            qualityfile <- qualityfile[grep("vcmslcfg", qualityfile)]
          }
          if (length(qualityfile) == 1){
            qualityfile <- paste0(light_location, "/", qualityfile)
            qualityfile <- raster::raster(qualityfile)
            qualityfile <- raster::crop(qualityfile, extent)
            if (t == 1){
              qualitydata <- qualityfile
            } else if (t > 1){
              qualitydata <- raster::merge(qualitydata, qualityfile)
            }
            rm(lightfile)
            rm(qualityfile)
          } else {
            stop(paste0("The quality file for ", month, "/", year, " could not be found in your light location."))
          }

        } # end tilenumbers loop


      } else if (lightdata_time == "yearly"){

        year <- sequence[j]

        if (corrected_lights == FALSE & harmonized_lights == FALSE){
          # select consistent dmsp version according to the start/ end of the time sequence.
          # otherwise choose the newest dmsp version for each year
          # check if the required files exist
          F10_years = as.character(seq(1992, 1994, by = 1))
          F12_years = as.character(seq(1994, 1999, by = 1))
          F14_years = as.character(seq(1997, 2003, by = 1))
          F15_years = as.character(seq(2000, 2007, by = 1))
          F16_years = as.character(seq(2004, 2009, by = 1))
          F18_years = as.character(seq(2010, 2013, by = 1))

          dmsp_stump <- NULL

          # not else if conditions but if conditions, so the
          # newer consistent version will overwrite the older one
          # if for the selected years there are 2 consistent versions
          if (sequence[1] %in% F10_years & tail(sequence, 1) %in% F10_years){
            dmsp_stump <- "F10"
            dmsp_consistent <- TRUE
          }
          if (sequence[1] %in% F12_years & tail(sequence, 1) %in% F12_years){
            dmsp_stump <- "F12"
            dmsp_consistent <- TRUE
          }
          if (sequence[1] %in% F14_years & tail(sequence, 1) %in% F14_years){
            dmsp_stump <- "F14"
            dmsp_consistent <- TRUE
          }
          if (sequence[1] %in% F15_years & tail(sequence, 1) %in% F15_years){
            dmsp_stump <- "F15"
            dmsp_consistent <- TRUE
          }
          if (sequence[1] %in% F16_years & tail(sequence, 1) %in% F16_years){
            dmsp_stump <- "F16"
            dmsp_consistent <- TRUE
          }
          if (sequence[1] %in% F18_years & tail(sequence, 1) %in% F18_years){
            dmsp_stump <- "F18"
            dmsp_consistent <- TRUE
          }
          if (is.null(dmsp_stump)){
            if (year == "1992" |
                year == "1993"){
              dmsp_stump <- "F10"
              dmsp_consistent <- FALSE
            } else if (year == "1994" |
                       year == "1995" |
                       year == "1996"){
              dmsp_stump <- "F12"
              dmsp_consistent <- FALSE
            } else if (year == "1997" |
                       year == "1998" |
                       year == "1999"){
              dmsp_stump <- "F14"
              dmsp_consistent <- FALSE
            } else if (year == "2000" |
                       year == "2001" |
                       year == "2002" |
                       year == "2003"){
              dmsp_stump <- "F15"
              dmsp_consistent <- FALSE
            } else if (year == "2004" |
                       year == "2005" |
                       year == "2006" |
                       year == "2007" |
                       year == "2008" |
                       year == "2009"){
              dmsp_stump <- "F16"
              dmsp_consistent <- FALSE
            } else if (year == "2010" |
                       year == "2011" |
                       year == "2012" |
                       year == "2013"){
              dmsp_stump <- "F18"
              dmsp_consistent <- FALSE
            }
          }

          list_light <- list.files(light_location, pattern = "avg_vis.tif")
          lightdata <- list_light[grep("stable", list_light)]
          lightdata <- lightdata[grep(dmsp_stump, lightdata)]
          lightdata <- lightdata[grep(year, lightdata)]
          if (length(lightdata) == 1){
            lightdata <- paste0(light_location, "/", lightdata)
            lightdata <- raster::raster(lightdata)
            lightdata <- raster::crop(lightdata, extent)
          } else { # if user does not have this version of the light file, search if there is
            # another one, only defined by year. since there are max. 2 versions per year,
            # this should find the other one if it is there. if not, the error message will occur
            lightdata <- list_light[grep("stable", list_light)]
            lightdata <- lightdata[grep(year, lightdata)]
            if (length(lightdata) == 1){
              lightdata <- paste0(light_location, "/", lightdata)
              lightdata <- raster::raster(lightdata)
              lightdata <- raster::crop(lightdata, extent)
            } else {
              stop(paste0("The light file for ", year, " could not be found in your light location."))
            }
          }

          list_quality <- list.files(light_location, pattern = "cf_cvg.tif")
          qualitydata <- list_quality[grep(dmsp_stump, list_quality)]
          qualitydata <- qualitydata[grep(year, qualitydata)]
          if (length(qualitydata) == 1){
            qualitydata <- paste0(light_location, "/", qualitydata)
            qualitydata <- raster::raster(qualitydata)
            qualitydata <- raster::crop(qualitydata, extent)
          } else { # same as above with the light file
            qualitydata <- list_quality[grep(year, list_quality)]
            if (length(qualitydata) == 1){
              qualitydata <- paste0(light_location, "/", qualitydata)
              qualitydata <- raster::raster(qualitydata)
              qualitydata <- raster::crop(qualitydata, extent)
            } else {
              stop(paste0("The quality file for ", year, " could not be found in your light location."))
            }
          }

        } else if (corrected_lights == TRUE & harmonized_lights == FALSE){

          if (year == "1996"){
            dmsp_stump <- "F12_19960316-19970212"
          } else if (year == "1999"){
            dmsp_stump <- "F12_19990119-19991211"
          } else if (year == "2000"){
            dmsp_stump <- "F12-F15_20000103-20001229"
          } else if (year == "2003"){
            dmsp_stump <- "F14-F15_20021230-20031127"
          } else if (year == "2004"){
            dmsp_stump <- "F14_20040118-20041216"
          } else if (year == "2006"){
            dmsp_stump <- "F16_20051128-20061224"
          } else if (year == "2010"){
            dmsp_stump <- "F16_20100111-20101209"
          } else {
            next # sequence fills all years, so skip to next year if current one
            # is not covered by corrected DMSP
          }

          list_light <- list.files(light_location, pattern = "avg_vis.tif")
          lightdata <- list_light[grep("rad_v4", list_light)]
          lightdata <- lightdata[grep(dmsp_stump, lightdata)]
          if (length(lightdata) == 1){
            lightdata <- paste0(light_location, "/", lightdata)
            lightdata <- raster::raster(lightdata)
            lightdata <- raster::crop(lightdata, extent)
          } else {
            stop(paste0("The light file for ", year, " could not be found in your light location."))
          }

          list_quality <- list.files(light_location, pattern = "cf_cvg.tif")
          qualitydata <- list_quality[grep("rad_v4", list_quality)]
          qualitydata <- qualitydata[grep(dmsp_stump, qualitydata)]
          if (length(qualitydata) == 1){
            qualitydata <- paste0(light_location, "/", qualitydata)
            qualitydata <- raster::raster(qualitydata)
            qualitydata <- raster::crop(qualitydata, extent)
          } else {
            stop(paste0("The quality file for ", year, " could not be found in your light location."))
          }

        } else if (corrected_lights == TRUE & harmonized_lights == TRUE){
          stop("Please choose either harmonized, corrected or standard yearly lights.")

        } else if (corrected_lights == FALSE & harmonized_lights == TRUE){
          qualitydata <- NULL
          list_light <- list.files(light_location, pattern = "Harmonized_DN_NTL")
          lightdata <- list_light[grep(year, list_light)]
          numericyear <- as.numeric(year)
          if (numericyear >= 1992 & numericyear < 2014){
            lightdata <- lightdata[grep("calDMSP.tif", lightdata)]
          } else if (numericyear >= 2014){
            lightdata <- lightdata[grep("simVIIRS.tif", lightdata)]
          }
          if (length(lightdata) == 1){
            lightdata <- paste0(light_location, "/", lightdata)
            lightdata <- raster::raster(lightdata)
            lightdata <- raster::crop(lightdata, extent)
          } else {
            stop(paste0("The light file for ", year, " could not be found in your light location."))
          }

        } # end corrected_lights if condition

      } # end lightdata_time if condition


      # end sourcefile "get_lightfile"


      if (rawdata == TRUE){
        lightdata_uncut <- lightdata
        if (harmonized_lights == FALSE){
          qualitydata_uncut <- qualitydata
        }
      }

      lightdata[qualitydata <= cut_quality] <- NA
      if (harmonized_lights == FALSE){
        qualitydata[qualitydata <= cut_quality] <- NA # also set this to NA so later the mean of observations per pixel is calculated only for pixels above the cut_quality threshold
      }

      if (!is.null(cut_low)){
        lightdata[lightdata < cut_low] <- NA
        if (harmonized_lights == FALSE){
          qualitydata[lightdata < cut_low] <- NA
        }
      }

      if (!is.null(cut_high)){
        lightdata[lightdata > cut_high] <- NA
        if (harmonized_lights == FALSE){
          qualitydata[lightdata > cut_high] <- NA
        }
      }

      if (lightdata_time == "monthly"){
        aggregated <- data.frame(area_name = "",
                                 iso3c = "",
                                 area_km2 = 0,
                                 yearmonth = "")
      } else if (lightdata_time == "yearly"){
        aggregated <- data.frame(area_name = "",
                                 iso3c = "",
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
      aggregated$iso3c <- ISO3
      aggregated$area_km2 <- area
      if (lightdata_time == "monthly"){
        aggregated$yearmonth <- paste0(year, "-", month)
      } else if (lightdata_time == "yearly"){
        aggregated$year <- as.numeric(year)
      }

      if (harmonized_lights == FALSE){
        aggregated$mean_obs <- suppressWarnings(raster::extract(qualitydata, shapefile, fun = mean, na.rm = TRUE))
      }

      for (k in 1:length(functions_calculate)){
        current_function_name <- functions_calculate[k]
        function_df <- data.frame(current_function_name = suppressWarnings(raster::extract(lightdata, shapefile, fun = get(functions_calculate[[k]]), na.rm = TRUE)))
        colnames(function_df)[colnames(function_df) == "current_function_name"] <- functions_calculate[k]
        aggregated <- cbind(aggregated, function_df)
      }

      all_aggregated <- rbind(all_aggregated, aggregated)

      if (rawdata == TRUE){
        rawdata_values <- data.frame(suppressWarnings(raster::extract(lightdata_uncut, shapefile, cellnumbers = TRUE)))
        rawdata_values <- cbind(rawdata_values, raster::coordinates(lightdata_uncut)[rawdata_values[,1],])
        colnames(rawdata_values)[colnames(rawdata_values) == "cell"] <- "cellnumber"
        colnames(rawdata_values)[colnames(rawdata_values) == "value"] <- "lightvalue"
        if (harmonized_lights == FALSE){
          rawdata_quality <- data.frame(suppressWarnings(raster::extract(qualitydata_uncut, shapefile, cellnumbers = TRUE)))
          rawdata_values <- cbind(rawdata_values, rawdata_quality$value)
          colnames(rawdata_values)[colnames(rawdata_values) == "rawdata_quality$value"] <- "number_obs"
          rawdata_values <- rawdata_values[,c("cellnumber", "lightvalue", "number_obs", "x", "y")]
        } else if (harmonized_lights == TRUE){
          rawdata_values <- rawdata_values[,c("cellnumber", "lightvalue", "x", "y")]
        }
        if (lightdata_time == "monthly"){
          rawdata_name <- paste0("rawlights_", area_name, "_", year, "-", month)
        } else if (lightdata_time == "yearly"){
          rawdata_name <- paste0("rawlights_", area_name, "_", year)
        }
        assign(rawdata_name, rawdata_values, envir = .GlobalEnv)
      }

      if (single_dataframes == TRUE){
        single_df_name <- paste0(area_name, "_lights")
        assign(single_df_name, all_aggregated, envir = .GlobalEnv)
      } # if user sets this to TRUE, the output will not only be an aggregated dataframe for all countries, but also an additional one for each country

      if (i == 1){
        lights <- all_aggregated
        # build dataframe called lights with all areas, start with first area, which means i = 1
      } else if (i > 1){
        lights <- rbind(lights, aggregated)
        # bind to the lights dataframe if area loop is at least at the second iteration
      }

    } # end sequence loop

    rm(area_name)
    rm(ISO3)
    shapefile <- NULL
    help_shapefile <- NULL

  } # end area loop

  lights <<- lights # load aggregated dataframe with all regions into global environment

  if (lightdata_time == "yearly" & corrected_lights == FALSE & harmonized_lights == FALSE){
    if (dmsp_consistent == TRUE | length(sequence) == 1){
      print(paste0("The consistent DMSP version selected for your timespan is ", dmsp_stump, "."))
    } else if (dmsp_consistent == FALSE & length(sequence) > 1){
      print("It was not possible to select a consistent DMSP version for your timespan. For each year, the newest DMSP version was chosen.")
    }
  }

} # end function
