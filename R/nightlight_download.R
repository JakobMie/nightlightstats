nightlight_download <- function(area_names = "world",
                                time,
                                light_location,
                                shapefile_location = NULL,
                                admlevel = 0,
                                shapefiles = NULL,
                                download_shape = "sp.rds",
                                dmsp_oldversion = FALSE,
                                user_coordinates = NULL){

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


  if (lightdata_time == "yearly"){

    stump1 <- "https://www.ngdc.noaa.gov/eog/data/web_data/v4composites/"
    stump3 <- ".v4.tar"

    for (j in 1:length(sequence)){

      year <- sequence[j]

      if (year == "1992" |
          year == "1993"){

        stump2 <- paste0("F10", year)

      } else if (year == "1994" |
                 year == "1995" |
                 year == "1996"){

        stump2 <- ifelse(year == "1994" &
                           dmsp_oldversion == TRUE,
                         paste0("F10", year),
                         paste0("F12", year))

      } else if (year == "1997" |
                 year == "1998" |
                 year == "1999"){

        stump2 <- ifelse((year == "1997" |
                            year == "1998" |
                            year == "1999") &
                           dmsp_oldversion == TRUE,
                         paste0("F12", year),
                         paste0("F14", year))

      } else if (year == "2000" |
                 year == "2001" |
                 year == "2002" |
                 year == "2003"){

        stump2 <- ifelse((year == "2000" |
                            year == "2001" |
                            year == "2002" |
                            year == "2003") &
                           dmsp_oldversion == TRUE,
                         paste0("F14", year),
                         paste0("F15", year))

      } else if (year == "2004" |
                 year == "2005" |
                 year == "2006" |
                 year == "2007" |
                 year == "2008" |
                 year == "2009"){

        stump2 <- ifelse((year == "2004" |
                            year == "2005" |
                            year == "2006" |
                            year == "2007") &
                           dmsp_oldversion == TRUE,
                         paste0("F15", year),
                         paste0("F16", year))

      } else if (year == "2010"){

        stump2 <- paste0("F18", year)

      } else if (year == "2011" |
                 year == "2012" |
                 year == "2013"){

        stump2 <- paste0("F18", year)

      }

      if (year == "2010"){
        stump4 <- ".v4d"
      } else if (year == "2011" |
                 year == "2012" |
                 year == "2013"){
        stump4 <- ".v4c"
      } else {
        stump4 <- ".v4b"
      }

      # test whether lightfile is already downloaded
      lightfile_test <- paste0(light_location, "/", stump2, stump4, "_web.stable_lights.avg_vis.tif")

      if(!file.exists(lightfile_test)){

        utils::download.file(url = paste0(stump1, stump2, stump3), destfile = paste0(light_location, "/", stump2, ".v4.tar"), mode = "wb")
        utils::untar(paste0(light_location, "/", stump2, ".v4.tar"),
                     exdir = light_location)

        lightfile <- list.files(paste0(light_location))
        lightfile <- lightfile[grep(lightfile, pattern = "stable")]
        lightfile <- lightfile[grep(lightfile, pattern = ".gz")]
        lightfile <- lightfile[grep(lightfile, pattern = year)]
        R.utils::gunzip(filename = paste0(light_location, "/", lightfile),
                        destname = paste0(light_location, "/", stump2, stump4, "_web.stable_lights.avg_vis.tif"))

        qualityfile <- list.files(paste0(light_location))
        qualityfile <- qualityfile[grep(qualityfile, pattern = "cf_cvg")]
        qualityfile <- qualityfile[grep(qualityfile, pattern = ".gz")]
        qualityfile <- qualityfile[grep(qualityfile, pattern = year)]
        R.utils::gunzip(filename = paste0(light_location, "/", qualityfile),
                        destname = paste0(light_location, "/", stump2, stump4, "_web.cf_cvg.tif"))

        remove_files <- list.files(paste0(light_location))
        remove_files <- remove_files[-grep(remove_files, pattern = "_web.stable_lights.avg_vis.tif")]
        remove_files <- remove_files[-grep(remove_files, pattern = "_web.cf_cvg.tif")]
        remove_files <- remove_files[grep(remove_files, pattern = year)]
        remove_files <- remove_files[grep(remove_files, pattern = stump4)]
        remove_files <- c(remove_files, paste0(stump2, ".v4.tar"))
        unlink(paste0(light_location, "/", remove_files), recursive = TRUE)

      } else if (file.exists(lightfile_test)){
        print(paste0("Light file for ", year, " is already downloaded."))
      }

    }

  } else if (lightdata_time == "monthly"){

    stumpurl = "https://eogdata.mines.edu/pages/download_dnb_composites_iframe.html"

    # Scrape the links directly from the overview page
    overview_page <- xml2::read_html(stumpurl)
    links <- rvest::html_attr(rvest::html_nodes(overview_page, "a"), "href")
    rm(overview_page)

    if ((!is.null(user_coordinates) & area_names == "world") |
        (!is.null(shapefiles) & area_names == "world")){
      area_names <-  c("")
    } # in case someone gives coordinates or shapefiles but leaves area_names at world,
    # this gives the area an empty name
    # so the "world" download will not be activated

    if (area_names == "world"){

      for (j in 1:length(sequence)){

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

        links_current <- links[grep(links, pattern = yearmonthspan)]

        tilenumbers <- c("1", "2", "3", "4", "5", "6")

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

          links_current_tile <- links_current[grep(links_current, pattern = tilestump)]
          links_current_tile <- links_current_tile[grep(links_current_tile, pattern = "vcmcfg")]

          lightfile_test <- strsplit(links_current_tile, "/")
          lightfile_test <- lightfile_test[[1]][11]
          lightfile_test <- strsplit(lightfile_test, ".tgz")
          lightfile_test1 <- paste0(light_location, "/", lightfile_test, ".avg_rade9.tif")
          lightfile_test2 <- paste0(light_location, "/", lightfile_test, ".avg_rade9h.tif")

          if(!(file.exists(lightfile_test1) | file.exists(lightfile_test2))){
            utils::download.file(links_current_tile,
                                 destfile = paste0(light_location, "/", yearmonth, ".tgz"), mode = "wb")

            R.utils::gunzip(filename = paste0(light_location, "/", yearmonth, ".tgz"),
                            destname = paste0(light_location, "/", yearmonth, "_unzipped.tar"))

            utils::untar(paste0(light_location, "/", yearmonth, "_unzipped.tar"),
                         exdir = paste0(light_location))

            unlink(paste0(light_location, "/", yearmonth, "_unzipped.tar"), recursive = TRUE)
          } else if (file.exists(lightfile_test1) | file.exists(lightfile_test2)){
            print(paste0("Lightfile for tile ", tilenumber, ", ", year, "/", month, " is already downloaded."))
          }

        }
      }


    } else if (area_names != "world"){

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


        for (t in 1:length(tilenumbers)){

          tilenumber <- tilenumbers[t]

          for (j in 1:length(sequence)){
            # build a string out of date and search for lightdata that matches this date
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

            links_current <- links[grep(links, pattern = yearmonthspan)]

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

            links_current <- links_current[grep(links_current, pattern = tilestump)]
            links_current <- links_current[grep(links_current, pattern = "vcmcfg")]

            # test whether lightfile is already downloaded
            lightfile_test <- strsplit(links_current, "/")
            lightfile_test <- lightfile_test[[1]][11]
            lightfile_test <- strsplit(lightfile_test, ".tgz")
            lightfile_test1 <- paste0(light_location, "/", lightfile_test, ".avg_rade9.tif")
            lightfile_test2 <- paste0(light_location, "/", lightfile_test, ".avg_rade9h.tif")

            if(!(file.exists(lightfile_test1) | file.exists(lightfile_test2))){
              utils::download.file(links_current,
                                   destfile = paste0(light_location, "/", yearmonth, ".tgz"), mode = "wb")

              R.utils::gunzip(filename = paste0(light_location, "/", yearmonth, ".tgz"),
                              destname = paste0(light_location, "/", yearmonth, "_unzipped.tar"))

              utils::untar(paste0(light_location, "/", yearmonth, "_unzipped.tar"),
                           exdir = paste0(light_location))

              unlink(paste0(light_location, "/", yearmonth, "_unzipped.tar"), recursive = TRUE)
            } else if (file.exists(lightfile_test1) | file.exists(lightfile_test2)){
              print(paste0("Light file for ", area_name, ", ", year, "/", month, " is already downloaded."))
            }
          }
        }
      }
    }
  }
}
