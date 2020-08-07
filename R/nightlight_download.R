nightlight_download <- function(area_names = "world",
                                time,
                                light_location,
                                shapefile_location = NULL,
                                shapefiles = NULL,
                                download_shape = "sp.rds",
                                admlevel = 0,
                                user_coordinates = NULL){


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


  if (lightdata_time == "yearly"){

    stump1 <- "https://www.ngdc.noaa.gov/eog/data/web_data/v4composites/"
    stump3 <- ".v4.tar"

    for (j in 1:length(sequence)){

      year <- sequence[j]

      if (year == "1992" |
          year == "1993"){

        stump2_all <- paste0("F10", year)

      } else if (year == "1994"){

        stump2_all <- c(paste0("F10", year),
                    paste0("F12", year))

      } else if (year == "1995" |
                 year == "1996"){

        stump2_all <- paste0("F12", year)

      } else if (year == "1997" |
                 year == "1998" |
                 year == "1999"){

        stump2_all <- c(paste0("F12", year),
                    paste0("F14", year))

      } else if (year == "2000" |
                 year == "2001" |
                 year == "2002" |
                 year == "2003"){

        stump2_all <- c(paste0("F14", year),
                    paste0("F15", year))

      } else if (year == "2004" |
                 year == "2005" |
                 year == "2006" |
                 year == "2007"){

        stump2_all <- c(paste0("F15", year),
                    paste0("F16", year))

      } else if (year == "2008" |
                 year == "2009"){

        stump2_all <- paste0("F16", year)

      } else if (year == "2010" |
                 year == "2011" |
                 year == "2012" |
                 year == "2013"){

        stump2_all <- paste0("F18", year)

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
      # for years with multiple versions, do this for all versions
      lightfile_test_all <- paste0(light_location, "/", stump2_all, stump4, "_web.stable_lights.avg_vis.tif")

      for (g in 1:length(lightfile_test_all)){

        lightfile_test <- lightfile_test_all[g]
        stump2 <- stump2_all[g]

        if(!file.exists(lightfile_test)){

          utils::download.file(url = paste0(stump1, stump2, stump3), destfile = paste0(light_location, "/", stump2, ".v4.tar"), mode = "wb")
          utils::untar(paste0(light_location, "/", stump2, ".v4.tar"),
                       exdir = paste0(light_location, "/"))

          lightfile <- list.files(path = light_location)
          lightfile <- lightfile[grep(lightfile, pattern = "stable")]
          lightfile <- lightfile[grep(lightfile, pattern = ".gz")]
          lightfile <- lightfile[grep(lightfile, pattern = year)]
          R.utils::gunzip(filename = paste0(light_location, "/", lightfile),
                          destname = paste0(light_location, "/", stump2, stump4, "_web.stable_lights.avg_vis.tif"))

          qualityfile <- list.files(light_location)
          qualityfile <- qualityfile[grep(qualityfile, pattern = "cf_cvg")]
          qualityfile <- qualityfile[grep(qualityfile, pattern = ".gz")]
          qualityfile <- qualityfile[grep(qualityfile, pattern = year)]
          R.utils::gunzip(filename = paste0(light_location, "/", qualityfile),
                          destname = paste0(light_location, "/", stump2, stump4, "_web.cf_cvg.tif"))

          remove_files <- list.files(light_location)
          remove_files <- remove_files[grep(remove_files, pattern = stump2)]
          remove_files <- remove_files[-grep(remove_files, pattern = "_web.stable_lights.avg_vis.tif")]
          remove_files <- remove_files[-grep(remove_files, pattern = "_web.cf_cvg.tif")]
          unlink(paste0(light_location, "/", remove_files), recursive = TRUE)

        } else if (file.exists(lightfile_test)){
          print(paste0("Light file for ", year, " is already downloaded."))
        }

      } # end test loop

    } # end sequence loop

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
                         exdir = paste0(light_location, "/"))

            unlink(paste0(light_location, "/", yearmonth, "_unzipped.tar"), recursive = TRUE)
          } else if (file.exists(lightfile_test1) | file.exists(lightfile_test2)){
            print(paste0("Lightfile for tile ", tilenumber, ", ", year, "/", month, " is already downloaded."))
          }

        } # end tilenumbers loop
      } # end sequence loop


    } else if (area_names != "world"){

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
          print(paste0("There is no ISO3 countrycode for ", area_name, ", hence download from GADM will not work. Either your shapefile is not a country or, if it is a country, the countryname was not recognized correctly."))
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


        # end sourcefile "get_shapefile"


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
                           exdir = paste0(light_location, "/"))

              unlink(paste0(light_location, "/", yearmonth, "_unzipped.tar"), recursive = TRUE)
            } else if (file.exists(lightfile_test1) | file.exists(lightfile_test2)){
              print(paste0("Light file for ", area_name, ", ", year, "/", month, " is already downloaded."))
            }

          } # end sequence loop
        } # end tilenumbers loop
      } # end area loop
    } # end area_names != world if condition
  } # end monthly if condition
} # end function
