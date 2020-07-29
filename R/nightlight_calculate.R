nightlight_calculate <- function(area_names,
                                 time_from,
                                 time_to,
                                 light_location,
                                 output_location = ".",
                                 admlevel = 0,
                                 shapefiles = NULL,
                                 download_shape = "sp.rds",
                                 functions_calculate = NULL,
                                 function_names = NULL,
                                 rectangle_calculate = FALSE,
                                 rawdata = FALSE,
                                 cut_low = NULL,
                                 cut_high = NULL,
                                 xmin = NULL,
                                 xmax = NULL,
                                 ymin = NULL,
                                 ymax = NULL){


  if (is.null(functions_calculate)){
    functions_calculate = c(sum, min, mean, max)
    function_names = c("lightsum", "lightmin", "lightmean", "lightmax")
  }

  user_coordinates <- c(xmin, xmax, ymin, ymax)

  time_from <- as.character(time_from) # need this as character to build strings later
  time_to <- as.character(time_to) # need this as character to build strings later

  lightdata_time = "monthly" # set default to monthly

  if (nchar(time_from) == 4 & nchar(time_to) == 4){
    lightdata_time = "yearly"
  } # change to yearly if year is given (only possibility for 4 characters)

  if (lightdata_time == "monthly"){
    if (nchar(time_from) != 7 | nchar(time_to) != 7){
      print('Please enter monthly dates in format yyyy-mm as a string to ensure that the function will read the date correctly. It works for other entries in English, but cannot be ensured to make this transfer across languages.')}
  }

  if (length(area_names == 1) & length(shapefiles) > 1){
    area_names = rep(area_names, length = length(shapefiles))
    for (l in 1:length(shapefiles)){
      area_names[l] <- paste0(area_names[l], "_", l)
    }
  } # if multiple shapefiles but only one area name provided: give that name to all shapefiles

  if (!is.null(shapefiles)){
    user_shapefiles <- shapefiles
  } else if (is.null(shapefiles)){
    user_shapefiles <- NA
  } # user_shapefiles is a duplicate of shapefiles if provided by the user, otherwise NA

  for (i in 1:length(area_names)){

    user_shapefile = user_shapefiles[i]
    shapefile = shapefiles[i]
    # these are either provided by the user and hence activated at this point
    # or shapefile will be NULL and user_shapefile will be NA and shapefile will be detected or downloaded later

    # if shapefile available at this point, its type is detected and it is read with the according function
    if (length(grep(user_shapefile, pattern = "sp.rds")) != 0){
      shapefile <- readRDS(shapefile)
    }
    if (length(grep(user_shapefile, pattern = "sf.rds")) != 0){
      print("Unfortunately, the function does not work with sf.rds shapefiles. If no other option is available to you, you can try using the min/max x- and y-coordinates of your shapefile in the function input instead.")
    }
    if (length(grep(user_shapefile, pattern = ".shp")) != 0 |
        length(grep(user_shapefile, pattern = ".kml")) != 0){
      shapefile <- rgdal::readOGR(shapefile)
    }
    if (length(grep(user_shapefile, pattern = ".gpkg")) != 0){
      layers <- rgdal::ogrListLayers(user_shapefile)
      layer = length(layers) - admlevel
      shapefile <- rgdal::readOGR(user_shapefile, layers[layer])
    }

    if(!is.null(user_coordinates)){
      extent <- raster::extent(user_coordinates)
      extent_bbox <- sp::bbox(extent)
      shapefile <- as(extent, "SpatialPolygons")
      raster::crs(shapefile) <- "+init=epsg:4326"
      shapefile <- sp::SpatialPolygonsDataFrame(shapefile, data.frame(N = c("1"), row.names = c("1")))
    } # creates a rectangular shapefile if user provides coordinates

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
      sequence <- as.character(seq(time_from,time_to,by = 1))
    }

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
      user_shapefile = list.files(".", pattern = "sp.rds")
      user_shapefile = user_shapefile[grep(user_shapefile, pattern = ISO3)]
      user_shapefile = user_shapefile[grep(user_shapefile, pattern = admlevel)]
      if (length(user_shapefile) != 0){
        shapefile <- readRDS(user_shapefile)
      }
    }
    # .shp
    if (is.null(shapefile)){
      user_shapefile = list.files(".", pattern = ".shp")
      user_shapefile = user_shapefile[grep(user_shapefile, pattern = ISO3)]
      user_shapefile = user_shapefile[grep(user_shapefile, pattern = admlevel)]
      if (length(user_shapefile) != 0){
        shapefile <- rgdal::readOGR(user_shapefile)
      }
    }
    # .shp but still zipped as .zip
    if (is.null(shapefile)){
      user_shapefile = list.files(".", pattern = "shp.zip")
      user_shapefile = user_shapefile[grep(user_shapefile, pattern = ISO3)]
      if (length(user_shapefile) != 0){
        user_shapefile = utils::unzip(zipfile = user_shapefile)
        user_shapefile = list.files(".", pattern = ".shp")
        user_shapefile = user_shapefile[grep(user_shapefile, pattern = ISO3)]
        user_shapefile = user_shapefile[grep(user_shapefile, pattern = admlevel)]
        shapefile <- rgdal::readOGR(user_shapefile)
        zipfile <- list.files(".", pattern = "shp.zip")
        zipfile <- zipfile[grep(zipfile, pattern = ISO3)]
        unlink(zipfile)
      }
    }
    # .kml
    if (is.null(shapefile)){
      user_shapefile = list.files(".", pattern = ".kml")
      user_shapefile = user_shapefile[grep(user_shapefile, pattern = ISO3)]
      user_shapefile = user_shapefile[grep(user_shapefile, pattern = admlevel)]
      if (length(user_shapefile) != 0){
        shapefile <- rgdal::readOGR(user_shapefile)
      }
    }
    # .kml but still zipped as .kmz
    if (is.null(shapefile)){
      user_shapefile = list.files(".", pattern = ".kmz")
      user_shapefile = user_shapefile[grep(user_shapefile, pattern = ISO3)]
      if (length(user_shapefile) != 0){
        user_shapefile = unzip(zipfile = user_shapefile)
        shapefile <- rgdal::readOGR(user_shapefile)
        zipfile = list.files(".", pattern = ".kmz")
        zipfile = zipfile[grep(zipfile, pattern = ISO3)]
        unlink(zipfile)
      }
    }
    # .gpkg
    if (is.null(shapefile)){
      user_shapefile = list.files(".", pattern = ".gpkg")
      user_shapefile = user_shapefile[grep(user_shapefile, pattern = ISO3)]
      if (length(user_shapefile) != 0){
        layers <- rgdal::ogrListLayers(user_shapefile)
        layer = length(layers) - admlevel
        shapefile <- rgdal::readOGR(user_shapefile, layers[layer])
      }
    }
    # .gpkg but still zipped as .zip
    if (is.null(shapefile)){
      user_shapefile = list.files(".", pattern = "gpkg.zip")
      user_shapefile = user_shapefile[grep(user_shapefile, pattern = ISO3)]
      if (length(user_shapefile) != 0){
        user_shapefile = utils::unzip(zipfile = user_shapefile)
        user_shapefile = list.files(".", pattern = ".gpkg")
        user_shapefile = user_shapefile[grep(user_shapefile, pattern = ISO3)]
        user_shapefile = user_shapefile[-grep(user_shapefile, pattern = ".zip")]
        layers <- rgdal::ogrListLayers(user_shapefile)
        layer = length(layers) - admlevel
        shapefile <- rgdal::readOGR(user_shapefile, layers[layer])
        zipfile = list.files(".", pattern = "gpkg.zip")
        zipfile = zipfile[grep(zipfile, pattern = ISO3)]
        unlink(zipfile)
      }
    }

    # then: check for shapefiles that are identified by the name given by the user in area_names

    # sp.rds
    if (is.null(shapefile)){
      user_shapefile = list.files(".", pattern = "sp.rds")
      user_shapefile = user_shapefile[grep(user_shapefile, pattern = area_name)]
      if (length(user_shapefile) != 0){
        shapefile <- readRDS(user_shapefile)
      }
    }
    # .shp
    if (is.null(shapefile)){
      user_shapefile = list.files(".", pattern = ".shp")
      user_shapefile = user_shapefile[grep(user_shapefile, pattern = area_name)]
      if (length(user_shapefile) != 0){
        shapefile <- rgdal::readOGR(user_shapefile)
      }
    }
    # .shp but still zipped as .zip
    if (is.null(shapefile)){
      user_shapefile = list.files(".", pattern = "shp.zip")
      user_shapefile = user_shapefile[grep(user_shapefile, pattern = area_name)]
      if (length(user_shapefile) != 0){
        user_shapefile = utils::unzip(zipfile = user_shapefile)
        user_shapefile = list.files(".", pattern = ".shp")
        user_shapefile = user_shapefile[grep(user_shapefile, pattern = area_name)]
        shapefile <- rgdal::readOGR(user_shapefile)
        zipfile <- list.files(".", pattern = "shp.zip")
        zipfile <- zipfile[grep(zipfile, pattern = area_name)]
        unlink(zipfile)
      }
    }
    # .kml
    if (is.null(shapefile)){
      user_shapefile = list.files(".", pattern = ".kml")
      user_shapefile = user_shapefile[grep(user_shapefile, pattern = area_name)]
      if (length(user_shapefile) != 0){
        shapefile <- rgdal::readOGR(user_shapefile)
      }
    }
    # .kml but still zipped as .kmz
    if (is.null(shapefile)){
      user_shapefile = list.files(".", pattern = ".kmz")
      user_shapefile = user_shapefile[grep(user_shapefile, pattern = area_name)]
      if (length(user_shapefile) != 0){
        user_shapefile = unzip(zipfile = user_shapefile)
        shapefile <- rgdal::readOGR(user_shapefile)
        zipfile = list.files(".", pattern = ".kmz")
        zipfile = zipfile[grep(zipfile, pattern = area_name)]
        unlink(zipfile)
      }
    }
    # .gpkg
    if (is.null(shapefile)){
      user_shapefile = list.files(".", pattern = ".gpkg")
      user_shapefile = user_shapefile[grep(user_shapefile, pattern = area_name)]
      if (length(user_shapefile) != 0){
        layers <- rgdal::ogrListLayers(user_shapefile)
        layer = length(layers) - admlevel
        shapefile <- rgdal::readOGR(user_shapefile, layers[layer])
      }
    }
    # .gpkg but still zipped as .zip
    if (is.null(shapefile)){
      user_shapefile = list.files(".", pattern = "gpkg.zip")
      user_shapefile = user_shapefile[grep(user_shapefile, pattern = area_name)]
      if (length(user_shapefile) != 0){
        user_shapefile = utils::unzip(zipfile = user_shapefile)
        user_shapefile = list.files(".", pattern = ".gpkg")
        user_shapefile = user_shapefile[grep(user_shapefile, pattern = area_name)]
        user_shapefile = user_shapefile[-grep(user_shapefile, pattern = ".zip")]
        layers <- rgdal::ogrListLayers(user_shapefile)
        layer = length(layers) - admlevel
        shapefile <- rgdal::readOGR(user_shapefile, layers[layer])
        zipfile = list.files(".", pattern = "gpkg.zip")
        zipfile = zipfile[grep(zipfile, pattern = area_name)]
        unlink(zipfile)
      }
    }

    # if shapefile is not downloaded yet: download from GADM
    # sp.rds
    if (is.null(shapefile) & download_shape == "sp.rds"){
      stumpurl <- "https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_"
      utils::download.file(paste0(stumpurl, ISO3, "_", admlevel, "_sp.rds"),
                           destfile = paste0(".", "/", "gadm36_", ISO3, "_", admlevel, "_sp.rds"), mode = "wb")
      user_shapefile <- paste0(".", "/", "gadm36_", ISO3, "_", admlevel, "_sp.rds")
      shapefile <- readRDS(user_shapefile)
    }
    # .shp
    if (is.null(shapefile) & download_shape == ".shp"){
      stumpurl <- "https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_"
      utils::download.file(paste0(stumpurl, ISO3, "_shp.zip"),
                           destfile = paste0(".", "/", "gadm36_", ISO3, "_shp.zip"), mode = "wb")
      user_shapefile <- paste0(".", "/", "gadm36_", ISO3, "_shp.zip")
      user_shapefile = utils::unzip(zipfile = user_shapefile)
      user_shapefile = user_shapefile[grep(user_shapefile, pattern = ".shp")]
      user_shapefile = user_shapefile[grep(user_shapefile, pattern = admlevel)]
      shapefile <- rgdal::readOGR(user_shapefile)
      unlink(paste0(".", "/", "gadm36_", ISO3, "_shp.zip"))
    }
    # .kml
    if (is.null(shapefile) & download_shape == ".kml"){
      stumpurl <- "https://biogeo.ucdavis.edu/data/gadm3.6/kmz/gadm36_"
      utils::download.file(paste0(stumpurl, ISO3, "_", admlevel, ".kmz"),
                           destfile = paste0(".", "/",  "gadm36_", ISO3, "_", admlevel, ".kmz"), mode = "wb")
      user_shapefile <- paste0(".", "/", "gadm36_", ISO3, "_", admlevel, ".kmz")
      user_shapefile = utils::unzip(zipfile = user_shapefile)
      shapefile <- rgdal::readOGR(user_shapefile)
      unlink(paste0(".", "/", "gadm36_", ISO3, "_", admlevel, ".kmz"))
    }
    # .gpkg
    if (is.null(shapefile) & download_shape == ".gpkg"){
      stumpurl <- "https://biogeo.ucdavis.edu/data/gadm3.6/gpkg/gadm36_"
      utils::download.file(paste0(stumpurl, ISO3, "_gpkg.zip"),
                           destfile = paste0(".", "/",  "gadm36_", ISO3, "_gpkg.zip"), mode = "wb")
      user_shapefile <- paste0(".", "/", "gadm36_", ISO3, "_gpkg.zip")
      user_shapefile = utils::unzip(zipfile = user_shapefile)
      user_shapefile = user_shapefile[grep(user_shapefile, pattern = ".gpkg")]
      layers <- rgdal::ogrListLayers(user_shapefile)
      layer = length(layers) - admlevel
      shapefile <- rgdal::readOGR(user_shapefile, layers[layer])
      unlink(paste0(".", "/", "gadm36_", ISO3, "_gpkg.zip"))
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


    if (rectangle_calculate == TRUE & is.null(user_coordinates)){
      crs <- suppressWarnings(raster::crs(shapefile))
      shapefile <- as(extent, "SpatialPolygons")
      raster::crs(shapefile) <- crs
      shapefile <- sp::SpatialPolygonsDataFrame(shapefile, data.frame(N = c("1"), row.names = c("1")))
    } # transform non-enclosed shapefile into a rectangle

    area <- (suppressWarnings(raster::area(shapefile))) / (1000^2) # area is output in sq meters; convert to sq kilometers
    # at this point we have either an enclosed shapefile or a rectangle. hence, area can be calculated
    # for lower admlevels, area will output as many values as there are regions of the lowest admlevel

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
        overlapping_tile = TRUE
      } else if (length(tilenumbers == 1)){
        overlapping_tile = FALSE
      }
    } else if (lightdata_time == "yearly"){
      tilenumbers <- c("")
      overlapping_tile = FALSE
    }

    if (lightdata_time == "monthly"){
      all_aggregated <- data.frame(area_name = "",
                                   iso3 = "",
                                   area_km2 = 0,
                                   yearmonth = "")
    } else if (lightdata_time == "yearly"){
      all_aggregated <- data.frame(area_name = "",
                                   iso3 = "",
                                   area_km2 = 0,
                                   year = 0)
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

    for (k in 1:length(function_names)){
      current_function_name <- function_names[k]
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
          list = list.files(paste0(light_location), pattern = "avg_rade9")
          lightfile <- list[grep(yearmonthspan, list)]
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
        } else if (lightdata_time == "yearly"){
          list = list.files(light_location, pattern = "avg_vis")
          list = list[grep("stable", list)]
          lightfile <- list[grep(year, list)]
          lightdata <- paste0(light_location, "/", lightfile)
          lightdata <- raster::raster(lightdata)
          lightdata <- raster::crop(lightdata, extent)
        }

        if (!is.null(cut_low)){
          lightdata[lightdata < cut_low] <- NA
        }
        if (!is.null(cut_high)){
          lightdata[lightdata > cut_high] <- NA
        }

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

      # set the number of rows according to the number of the lowest-order subregional division (5 divisions are the most finely divided divisions)
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
        aggregated$year <- as.numeric(year) # sidenote: the variable numericyear is only defined for monthly data, hence it is not used here
      }

      for (k in 1:length(functions_calculate)){
        current_function_name <- function_names[k]
        current_function <- functions_calculate[k]
        function_df <- data.frame(current_function_name = suppressWarnings(raster::extract(lightdata, shapefile, fun = functions_calculate[[k]], na.rm = TRUE)))
        colnames(function_df)[colnames(function_df) == "current_function_name"] <- current_function_name
        aggregated <- cbind(aggregated, function_df)
      }

      all_aggregated <- rbind(all_aggregated, aggregated)

      if (rawdata == TRUE){
        rawdata_values <- data.frame(suppressWarnings(raster::extract(lightdata, shapefile, cellnumbers = TRUE)))
        rawdata_values <- cbind(rawdata_values, raster::coordinates(lightdata)[rawdata_values[,1],])
        save(rawdata_values, file = paste0(output_location, "/", area_name, "_", sequence[j], "_adm", admlevel, "_rawdata", ".RData"))
      }

    }

    if (lightdata_time == "monthly"){
      all_aggregated$time <- zoo::as.yearmon(all_aggregated$yearmonth)
    }

    lights <- all_aggregated # rename to nightlights to make naming more intuitive

    if (rectangle_calculate == FALSE){
      save(lights, file = paste0(output_location, "/", area_name, "_", sequence[1], "-", utils::tail(sequence, n = 1), "_adm", admlevel, ".RData"))
    } else if (rectangle_calculate == TRUE){
      save(lights, file = paste0(output_location, "/", area_name, "_", sequence[1], "-", utils::tail(sequence, n = 1), "_adm", admlevel, "_rectangle", ".RData"))
    }

    rm(area_name)
    rm(ISO3)
    shapefile <- NULL
    user_shapefile <- NULL

  }
}
