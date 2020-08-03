nightlight_plot <- function(area_names,
                            time,
                            light_location,
                            shapefile_location = NULL,
                            admlevel = 0,
                            shapefiles = NULL,
                            download_shape = "sp.rds",
                            saveraster = FALSE,
                            light_colours = c("white", "darkblue", "gray"),
                            shapefile_colour = "black",
                            fixed_scale_low = NULL,
                            fixed_scale_high = NULL,
                            user_projection = NULL,
                            user_coordinates = NULL){

  if (is.null(shapefile_location) & is.null(user_coordinates)){
    print("Please input geographical information, either by providing shapefiles (do not forget to use shapefile_location to specifiy the location on your drive) or a set of coordinates.")
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

  colour_from <- light_colours[1]
  colour_to <- light_colours[2]
  na_colour <- light_colours[3]

  if (length(area_names == 1) & length(shapefiles) > 1){
    area_names <- rep(area_names, length = length(shapefiles))
    for (l in 1:length(shapefiles)){
      area_names[l] <- paste0(area_names[l], "_", l)
    }
  } # if multiple shapefiles but only one area name provided: give that name to all shapefiles

  if (!is.null(shapefiles)){
    shapefiles <- paste0(shapefile_location, "/", shapefiles)
    user_shapefiles <- shapefiles
  } else if (is.null(shapefiles)){
    user_shapefiles <- NA
  } # user_shapefiles is a duplicate of shapefiles if provided by the user, otherwise NA

  for (i in 1:length(area_names)){

    user_shapefile <- user_shapefiles[i]
    shapefile <- shapefiles[i]
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
      layer <- length(layers) - admlevel
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
      user_shapefile <- list.files(shapefile_location, pattern = "sp.rds")
      user_shapefile <- user_shapefile[grep(user_shapefile, pattern = ISO3)]
      user_shapefile <- user_shapefile[grep(user_shapefile, pattern = admlevel)]
      if (length(user_shapefile) != 0){
        shapefile <- readRDS(paste0(shapefile_location, "/", user_shapefile))
      }
    }
    # .shp
    if (is.null(shapefile)){
      user_shapefile <- list.files(shapefile_location, pattern = ".shp")
      user_shapefile <- user_shapefile[grep(user_shapefile, pattern = ISO3)]
      user_shapefile <- user_shapefile[grep(user_shapefile, pattern = admlevel)]
      if (length(user_shapefile) != 0){
        shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", user_shapefile))
      }
    }
    # .shp but still zipped as .zip
    if (is.null(shapefile)){
      user_shapefile <- list.files(shapefile_location, pattern = "shp.zip")
      user_shapefile <- user_shapefile[grep(user_shapefile, pattern = ISO3)]
      if (length(user_shapefile) != 0){
        user_shapefile <- utils::unzip(zipfile = paste0(shapefile_location, "/", user_shapefile), exdir = shapefile_location)
        user_shapefile <- list.files(shapefile_location, pattern = ".shp")
        user_shapefile <- user_shapefile[grep(user_shapefile, pattern = ISO3)]
        user_shapefile <- user_shapefile[grep(user_shapefile, pattern = admlevel)]
        shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", user_shapefile))
        zipfile <- list.files(shapefile_location, pattern = "shp.zip")
        zipfile <- zipfile[grep(zipfile, pattern = ISO3)]
        unlink(zipfile)
      }
    }
    # .kml
    if (is.null(shapefile)){
      user_shapefile <- list.files(shapefile_location, pattern = ".kml")
      user_shapefile <- user_shapefile[grep(user_shapefile, pattern = ISO3)]
      user_shapefile <- user_shapefile[grep(user_shapefile, pattern = admlevel)]
      if (length(user_shapefile) != 0){
        shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", user_shapefile))
      }
    }
    # .kml but still zipped as .kmz
    if (is.null(shapefile)){
      user_shapefile <- list.files(shapefile_location, pattern = ".kmz")
      user_shapefile <- user_shapefile[grep(user_shapefile, pattern = ISO3)]
      if (length(user_shapefile) != 0){
        user_shapefile <- unzip(zipfile = paste0(shapefile_location, "/", user_shapefile), exdir = shapefile_location)
        shapefile <- rgdal::readOGR(user_shapefile)
        zipfile <- list.files(shapefile_location, pattern = ".kmz")
        zipfile <- zipfile[grep(zipfile, pattern = ISO3)]
        unlink(zipfile)
      }
    }
    # .gpkg
    if (is.null(shapefile)){
      user_shapefile <- list.files(shapefile_location, pattern = ".gpkg")
      user_shapefile <- user_shapefile[grep(user_shapefile, pattern = ISO3)]
      if (length(user_shapefile) != 0){
        layers <- rgdal::ogrListLayers(paste0(shapefile_location, "/", user_shapefile))
        layer <- length(layers) - admlevel
        shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", user_shapefile), layers[layer])
      }
    }
    # .gpkg but still zipped as .zip
    if (is.null(shapefile)){
      user_shapefile <- list.files(shapefile_location, pattern = "gpkg.zip")
      user_shapefile <- user_shapefile[grep(user_shapefile, pattern = ISO3)]
      if (length(user_shapefile) != 0){
        user_shapefile <- utils::unzip(zipfile = paste0(shapefile_location, "/", user_shapefile), exdir = shapefile_location)
        user_shapefile <- list.files(shapefile_location, pattern = ".gpkg")
        user_shapefile <- user_shapefile[grep(user_shapefile, pattern = ISO3)]
        user_shapefile <- user_shapefile[-grep(user_shapefile, pattern = ".zip")]
        layers <- rgdal::ogrListLayers(paste0(shapefile_location, "/", user_shapefile))
        layer <- length(layers) - admlevel
        shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", user_shapefile), layers[layer])
        zipfile <- list.files(shapefile_location, pattern = "gpkg.zip")
        zipfile <- zipfile[grep(zipfile, pattern = ISO3)]
        unlink(zipfile)
      }
    }

    # then: check for shapefiles that are identified by the name given by the user in area_names

    # sp.rds
    if (is.null(shapefile)){
      user_shapefile <- list.files(shapefile_location, pattern = "sp.rds")
      user_shapefile <- user_shapefile[grep(user_shapefile, pattern = area_name)]
      if (length(user_shapefile) != 0){
        shapefile <- readRDS(paste0(shapefile_location, "/", user_shapefile))
      }
    }
    # .shp
    if (is.null(shapefile)){
      user_shapefile <- list.files(shapefile_location, pattern = ".shp")
      user_shapefile <- user_shapefile[grep(user_shapefile, pattern = area_name)]
      if (length(user_shapefile) != 0){
        shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", user_shapefile))
      }
    }
    # .shp but still zipped as .zip
    if (is.null(shapefile)){
      user_shapefile <- list.files(shapefile_location, pattern = "shp.zip")
      user_shapefile <- user_shapefile[grep(user_shapefile, pattern = area_name)]
      if (length(user_shapefile) != 0){
        user_shapefile <- utils::unzip(zipfile = paste0(shapefile_location, "/", user_shapefile), exdir = shapefile_location)
        user_shapefile <- list.files(shapefile_location, pattern = ".shp")
        user_shapefile <- user_shapefile[grep(user_shapefile, pattern = area_name)]
        shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", user_shapefile))
        zipfile <- list.files(shapefile_location, pattern = "shp.zip")
        zipfile <- zipfile[grep(zipfile, pattern = area_name)]
        unlink(zipfile)
      }
    }
    # .kml
    if (is.null(shapefile)){
      user_shapefile <- list.files(shapefile_location, pattern = ".kml")
      user_shapefile <- user_shapefile[grep(user_shapefile, pattern = area_name)]
      if (length(user_shapefile) != 0){
        shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", user_shapefile))
      }
    }
    # .kml but still zipped as .kmz
    if (is.null(shapefile)){
      user_shapefile <- list.files(shapefile_location, pattern = ".kmz")
      user_shapefile <- user_shapefile[grep(user_shapefile, pattern = area_name)]
      if (length(user_shapefile) != 0){
        user_shapefile <- utils::unzip(zipfile = paste0(shapefile_location, "/", user_shapefile), exdir = shapefile_location)
        shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", user_shapefile))
        zipfile <- list.files(".", pattern = ".kmz")
        zipfile <- zipfile[grep(zipfile, pattern = area_name)]
        unlink(zipfile)
      }
    }
    # .gpkg
    if (is.null(shapefile)){
      user_shapefile <- list.files(shapefile_location, pattern = ".gpkg")
      user_shapefile <- user_shapefile[grep(user_shapefile, pattern = area_name)]
      if (length(user_shapefile) != 0){
        layers <- rgdal::ogrListLayers(paste0(shapefile_location, "/", user_shapefile))
        layer <- length(layers) - admlevel
        shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", user_shapefile), layers[layer])
      }
    }
    # .gpkg but still zipped as .zip
    if (is.null(shapefile)){
      user_shapefile <- list.files(shapefile_location, pattern = "gpkg.zip")
      user_shapefile <- user_shapefile[grep(user_shapefile, pattern = area_name)]
      if (length(user_shapefile) != 0){
        user_shapefile <- utils::unzip(zipfile = paste0(shapefile_location, "/", user_shapefile), exdir = shapefile_location)
        user_shapefile <- list.files(shapefile_location, pattern = ".gpkg")
        user_shapefile <- user_shapefile[grep(user_shapefile, pattern = area_name)]
        user_shapefile <- user_shapefile[-grep(user_shapefile, pattern = ".zip")]
        layers <- rgdal::ogrListLayers(paste0(shapefile_location, "/", user_shapefile))
        layer <- length(layers) - admlevel
        shapefile <- rgdal::readOGR(paste0(shapefile_location, "/", user_shapefile), layers[layer])
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
      user_shapefile <- paste0(shapefile_location, "/", "gadm36_", ISO3, "_", admlevel, "_sp.rds")
      shapefile <- readRDS(user_shapefile)
    }
    # .shp
    if (is.null(shapefile) & download_shape == ".shp"){
      stumpurl <- "https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_"
      utils::download.file(paste0(stumpurl, ISO3, "_shp.zip"),
                           destfile = paste0(shapefile_location, "/", "gadm36_", ISO3, "_shp.zip"), mode = "wb")
      user_shapefile <- paste0(shapefile_location, "/", "gadm36_", ISO3, "_shp.zip")
      user_shapefile <- utils::unzip(zipfile = user_shapefile, exdir = shapefile_location)
      user_shapefile <- user_shapefile[grep(user_shapefile, pattern = ".shp")]
      user_shapefile <- user_shapefile[grep(user_shapefile, pattern = admlevel)]
      shapefile <- rgdal::readOGR(user_shapefile)
      unlink(paste0(shapefile_location, "/", "gadm36_", ISO3, "_shp.zip"))
    }
    # .kml
    if (is.null(shapefile) & download_shape == ".kml"){
      stumpurl <- "https://biogeo.ucdavis.edu/data/gadm3.6/kmz/gadm36_"
      utils::download.file(paste0(stumpurl, ISO3, "_", admlevel, ".kmz"),
                           destfile = paste0(shapefile_location, "/",  "gadm36_", ISO3, "_", admlevel, ".kmz"), mode = "wb")
      user_shapefile <- paste0(shapefile_location, "/", "gadm36_", ISO3, "_", admlevel, ".kmz")
      user_shapefile <- utils::unzip(zipfile = user_shapefile, exdir = shapefile_location)
      shapefile <- rgdal::readOGR(user_shapefile)
      unlink(paste0(shapefile_location, "/", "gadm36_", ISO3, "_", admlevel, ".kmz"))
    }
    # .gpkg
    if (is.null(shapefile) & download_shape == ".gpkg"){
      stumpurl <- "https://biogeo.ucdavis.edu/data/gadm3.6/gpkg/gadm36_"
      utils::download.file(paste0(stumpurl, ISO3, "_gpkg.zip"),
                           destfile = paste0(shapefile_location, "/",  "gadm36_", ISO3, "_gpkg.zip"), mode = "wb")
      user_shapefile <- paste0(shapefile_location, "/", "gadm36_", ISO3, "_gpkg.zip")
      user_shapefile <- utils::unzip(zipfile = user_shapefile, exdir = shapefile_location)
      user_shapefile <- user_shapefile[grep(user_shapefile, pattern = ".gpkg")]
      layers <- rgdal::ogrListLayers(user_shapefile)
      layer <- length(layers) - admlevel
      shapefile <- rgdal::readOGR(user_shapefile, layers[layer])
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


    for (j in 1:length(sequence)){

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
          list <- list.files(paste0(light_location), pattern = "avg_rade9")
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
          list <- list.files(light_location, pattern = "avg_vis")
          list <- list[grep("stable", list)]
          lightfile <- list[grep(year, list)]
          lightdata <- paste0(light_location, "/", lightfile)
          lightdata <- raster::raster(lightdata)
          lightdata <- raster::crop(lightdata, extent)
        }
      }

      if (saveraster == TRUE){
        raster_lights <<- lightdata # load into global environment with name raster_lights
      }

      lightdata <- raster::rasterToPoints(lightdata)
      df <- data.frame(lightdata)
      colnames(df) <- c("lon", "lat", "light")

      if (is.null(fixed_scale_low)){
        fixed_scale_low_plot <- min(df$light, na.rm = TRUE)
      } else if (!is.null(fixed_scale_low)){
        fixed_scale_low_plot <- fixed_scale_low
      }

      if (is.null(fixed_scale_high)){
        fixed_scale_high_plot <- max(df$light, na.rm = TRUE)
      } else if (!is.null(fixed_scale_high)){
        fixed_scale_high_plot <- fixed_scale_high
      }

      if (lightdata_time == "monthly"){
        title <- paste0("Nighttime Lights ", area_name, " (", month, "/", year, ")")
      } else if (lightdata_time == "yearly"){
        title <- paste0("Nighttime Lights ", area_name, " (", year, ")")
      }

      if (is.null(user_projection)){
      lights_plot <<- ggplot2::ggplot() +
          ggplot2::geom_tile(data = df, mapping = ggplot2::aes(x = lon, y = lat, fill = light), alpha = 0.8) +
          ggplot2::scale_fill_gradientn(limits = c(fixed_scale_low_plot, fixed_scale_high_plot),
                                        colors = plotrix::smoothColors(colour_from, 100, colour_to), na.value = na_colour) +
          ggplot2::coord_quickmap() +
          ggplot2::geom_path(data = shapefile, mapping = ggplot2::aes(x = long, y = lat, group = group), colour = shapefile_colour) +
          ggplot2::labs(title = title) +
          ggplot2::scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::theme_bw()+
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                         panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         axis.title.x = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank(),
                         axis.title.y = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_blank(),
                         axis.ticks.y = ggplot2::element_blank())
      } else if (!is.null(user_projection)){
        lights_plot <<- ggplot2::ggplot() +
          ggplot2::geom_tile(data = df, mapping = ggplot2::aes(x = lon, y = lat, fill = light), alpha = 0.8) +
          ggplot2::scale_fill_gradientn(limits = c(fixed_scale_low_plot, fixed_scale_high_plot), colors = plotrix::smoothColors(colour_from, 100, colour_to), na.value = na_colour) +
          ggplot2::coord_map(projection = user_projection) +
          ggplot2::geom_path(data = shapefile, mapping = ggplot2::aes(x = long, y = lat, group = group), colour = shapefile_colour) +
          ggplot2::labs(title = title) +
          ggplot2::scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::theme_bw()+
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                         panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         axis.title.x = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank(),
                         axis.title.y = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_blank(),
                         axis.ticks.y = ggplot2::element_blank())
      }
    }

    rm(area_name)
    rm(ISO3)
    shapefile <- NULL
    tilenumber <- NULL

  }
}

