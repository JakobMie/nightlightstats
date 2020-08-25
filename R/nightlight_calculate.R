#' nightlight_calculate
#'
#' Perform calculations on nightlights of a region in a given time interval
#' (defaults are sum of light values, min, mean and max, plus mean_obs, which
#' indicates the mean number of observations (with respect to pixels) that went
#' into the aggregated image for the time period in a given area).
#'
#' @param area_names May not be empty. String (vector) with the name(s) of your
#' region(s). If you provide a country name, the country shapefile will be
#' downloaded automatically. Shapefiles in your shapefile location that have
#' the area_names or (in case they exist) their iso3c countrycodes in their
#' filename will be detected automatically. If you provide own shapefiles in
#' the shapefiles argument, put the names in order of the shapefile order. If
#' you only put one name for multiple shapefiles, all shapefiles will be
#' processed with that name.
#' @param time May not be empty. Vector of strings with the start/end dates in
#' the format "2012-04" (monthly data) or "1992" (yearly data). If only one
#' time period is desired, then simply input one date.
#' @param light_location May not be empty. Provide the location of the
#' lightfiles on your drive as a string. Note about the light files: for the
#' DMSP images, some years have 2 versions available. Generally, if possible,
#' a consistent version for your timespan will be chosen. Otherwise the newest
#' version for each year will be selected. If 2 consistent versions are
#' available for your timespan, the newer one will be selected.
#' @param shapefile_location May be empty. Provide the location of the
#' shapefiles on your drive as a string in the case that you want to use
#' shapefiles and not a set of coordinates.
#' @param shapefiles May be empty. You can provide own shapefiles here
#' (input are the filenames) if no automatic download is desired. If there is
#' a shapefile in your shapefile location that has either the name of the
#' region which you enter in area_names or the iso3c countrycode
#' (if it is a country) in its filename, it will be detected automatically and
#' you do not have to use this argument.
#' @param download_shape Default is ".gpkg". Change to ".rds", ".shp" or
#' ".kml" if you want to download a different shapefile format from GADM. Will
#' only download if no own shapefiles are provided in the shapefiles argument
#' or automatically detected in the shapefile location.
#' @param gpkg_layer May be empty. You might need this argument if the code
#' does not detect the correct layer of a .gpkg file automatically. This can
#' happen if the layers of your .gpkg shapefile do not include an admlevel in
#' their names. In that case, enter the layer here as a string. Note that this
#' only works for one area at a time. To find out which layers are included in
#' your .gpkg shapefile, you can use rgdal::ogrListLayers().
#' @param admlevel Default is 0. Change this when working with different
#' administrative levels. Important for nightlight_calculate: if your shapefile
#' is not from GADM and features adm levels larger than 0, you must rename the
#' column names to NAME_X, X being the admlevel. You can do this by reading
#' the shapefile e.g. with readRDS() or rgdal::readOGR(), renaming the
#' columns of the SpatialPolygonsDataFrame accordingly and then saving the
#' shapefile again. Only with this naming will the calculation with lower
#' admlevels work if the shapefile is not from GADM.
#' @param single_dataframes Default is FALSE. If set to TRUE, this will not
#' only output a dataframe with all areas into the global environment, but also
#' a dataframe for each area specified in area_names.
#' @param functions_calculate Defaults (activated if this is NULL) are sum of
#' light values, min, mean, and max. These calculations will be performed on
#' the night light data for your given time and region. Functions need to
#' support the possibility to set na.rm as an argument. See the documentation
#' of raster::extent(), into which the functions are fed, for further details
#' if you encounter problems. The functions have to be given in a vector of
#' strings, since they are called with get(). The name of the functions will
#' be the column name in the final dataframe output as well.
#' @param rectangle_calculate Default is NULL, which leads to an automatic
#' detection whether your shapefile has to be transformed to a rectangle or not.
#' You can set this to TRUE if you encounter problems with your shapefile -
#' the code will calculate the values for the smallest rectangle that reaches
#' over the full extent of the shapefile. The code will set this automatically
#' to TRUE if a shapefile is not of class SpatialPolygons or if it is of class
#' SpatialPolygons but does not allow feature a unique enclosed area.
#' You can also set this manually to FALSE if for some reason the code wrongly
#' identifies your area as non-enclosed.
#' @param rawdata Default is FALSE. If set to TRUE, an additional file with the
#' raw night light data, i.e. the light values and their coordinates, will be
#' produced for each time period.
#' @param cut_low May be empty. If a value is provided, light values below
#' cut_low will be set to NA before performing calculations.
#' @param cut_high May be empty. If a value is provided, light values above
#' cut_high will be set to NA before performing calculations.
#' @param cut_quality Default is 0. This will set all light values to NA
#' for which the number of observations that went into the calculation of a
#' pixel is smaller than or equal to cut_quality. Hence, with the default
#' value you will have all pixels set to NA for which 0 observations were
#' made. You can set this to a higher number if you want to drop all pixels
#' below a given number of observations.
#' @param user_coordinates May be empty. Inputs are decimal numbers (longlat).
#' Can be specified if you want to calculate night lights for a region
#' defined by specific coordinates. Input order is xmin, xmax, ymin, ymax.
#' @param corrected_lights Default is FALSE. If set to TRUE, the
#' radiance-calibrated version of the DMSP data or the straylight-corrected
#' version of the VIIRS data will be used. Note that you have to download these
#' versions first, since they are different images from the standard ones.
#' @param harmonized_lights Default is FALSE. If set to TRUE, the harmonized
#' DMSP-VIIRS yearly dataset by Li et al. (2020) will be used. Note that you
#' have to download these versions first, since they are different images from
#' the standard ones.
#' @export

nightlight_calculate <- function(area_names,
                                 time,
                                 light_location,
                                 shapefile_location = NULL,
                                 shapefiles = NULL,
                                 download_shape = ".gpkg",
                                 gpkg_layer = NULL,
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

  help_shapefiles <- area_name <- lightdata_time <- tilenumbers <- ISO3 <-
    ISO3s <- dmsp_consistent <- dmsp_stump <- skip_period <- extent <- year <-
    month <- NULL # set the variables to NULL first to bind variables so
  # there is no "binding for global variables" problem later

  if (is.null(functions_calculate)){
    functions_calculate <- c("sum", "min", "mean", "max")
  }

  if (is.null(shapefile_location) & is.null(user_coordinates)){
    stop(paste0("Please input geographical information, either by providing ",
               "shapefiles (do not forget to use shapefile_location to ",
               "specify the location on your drive) or a set of coordinates."))
  }

  source_setup(light_location = light_location,
        shapefile_location = shapefile_location,
        time = time, area_names = area_names,
        shapefiles = shapefiles)


  for (i in 1:length(area_names)){

    area_name <- area_names[i]
    ISO3 <- ISO3s[i]

    if (is.na(ISO3)){
      print(paste0("An iso3c countrycode for ", area_name, "could not be ",
                   "found, hence the download from GADM will not work. Either ",
                   "your shapefile is not a country or, if it is a country, ",
                   "the countryname was not recognized correctly."))
    }

    get_shapefile(i = i,
                  area_name = area_name,
                  ISO3 = ISO3,
                  help_shapefiles = help_shapefiles,
                  shapefiles = shapefiles,
                  admlevel = admlevel,
                  gpkg_layer = gpkg_layer,
                  user_coordinates = user_coordinates,
                  shapefile_location = shapefile_location,
                  download_shape = download_shape,
                  lightdata_time = lightdata_time)

    # in case user did not set rectangle to TRUE: check for enclosed area
    # and set automatically
    if (is.null(rectangle_calculate)){
      rectangle_manual_setting <- FALSE
      if (admlevel != 0){
        rectangle_calculate <- FALSE # for lower admlevels, the code below
        # will detect rectangle_calculate = TRUE because there are multiple
        # areas. however, a lower-admlevel shapefile basically has to be
        # enclosed, so set to FALSE. if desired, the user can still override it
      } else if (admlevel == 0){
        if (class(shapefile) != "SpatialPolygonsDataFrame"){
          # spatial lines and points will have to be rectangularized
          # there are some other uncommon formats that could maybe be enclosed.
          # user will get a message about the rectangularization and can set
          # FALSE manually.
          rectangle_calculate <- TRUE
        } else if (class(shapefile) == "SpatialPolygonsDataFrame"){
          help_area <- suppressWarnings(raster::area(shapefile))
          # there are some shapefiles that have a lot of features and
          # calculate many areas even though it is only one geographical object,
          # e.g. a map with city buildings. this will have to be
          # rectangularized
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
    } else if (!is.null(rectangle_calculate)){
      rectangle_manual_setting <- TRUE
    }

    # transform non-enclosed shapefile into a rectangle if
    # rectangle_calculate is TRUE
    if (rectangle_calculate == TRUE &
        rectangle_manual_setting == FALSE &
        is.null(user_coordinates)){
      crs <- suppressWarnings(raster::crs(shapefile))
      shapefile <- methods::as(extent, "SpatialPolygons")
      raster::crs(shapefile) <- crs
      shapefile <- sp::SpatialPolygonsDataFrame(shapefile,
                                                data.frame(N = c("1"),
                                                           row.names = c("1")))
      print(paste0("The shapefile for ", area_name , " features a ",
      "non-enclosed area. The calculations will be performed on a ",
      "rectangular version of the shapefile defined by its minimum and ",
      "maximum extent."))
    }

    # at this point we have either an enclosed shapefile or a rectangle.
    # hence, an area can be calculated. for lower admlevels, area will output
    # as many values as there are regions of the lowest admlevel
    area <- (suppressWarnings(raster::area(shapefile))) / (1000^2)
    # area is output in sq meters; convert to sq kilometers


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
      colnames(name_df)[colnames(name_df) ==
                          "current_function_name"] <- current_function_name
      all_aggregated <- cbind(all_aggregated, name_df)
    }

    all_aggregated <- all_aggregated[-c(1),]

    for (j in 1:length(sequence)){

      get_lightfile(j = j,
                    light_location = light_location,
                    lightdata_time = lightdata_time,
                    sequence = sequence,
                    tilenumbers = tilenumbers,
                    corrected_lights = corrected_lights,
                    harmonized_lights = harmonized_lights,
                    extent = extent)

      if (skip_period == TRUE){
        next
      }

      if (rawdata == TRUE){
        lightdata_uncut <- lightdata
        if (harmonized_lights == FALSE){
          qualitydata_uncut <- qualitydata
        }
      }

      if (harmonized_lights == FALSE){
        lightdata[qualitydata <= cut_quality] <- NA
        qualitydata[qualitydata <= cut_quality] <- NA
        # also set quality to NA so later the mean of observations per pixel is
        # calculated only for pixels above the cut_quality threshold
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

      # set the number of rows for the output dataframe according to the number
      # of the lowest-order subregional division (5 divisions are the most
      # finely divided divisions)
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
        aggregated$mean_obs <- suppressWarnings(raster::extract(qualitydata,
                                                                shapefile,
                                                                fun = mean,
                                                                na.rm = TRUE))
      }

      for (k in 1:length(functions_calculate)){
        current_function_name <- functions_calculate[k]
        function_df <- data.frame(
          current_function_name =
            suppressWarnings(raster::extract(lightdata,
                                             shapefile,
                                             fun = get(
                                               functions_calculate[[k]]),
                                             na.rm = TRUE)))
        colnames(function_df)[colnames(
          function_df) == "current_function_name"] <- functions_calculate[k]
        aggregated <- cbind(aggregated, function_df)
      }

      all_aggregated <- rbind(all_aggregated, aggregated)

      if (rawdata == TRUE){
        rawdata_values <- data.frame(suppressWarnings(
          raster::extract(
            lightdata_uncut,
            shapefile,
            cellnumbers = TRUE)))
        rawdata_values <- cbind(
          rawdata_values,
          raster::coordinates(lightdata_uncut)[rawdata_values[,1],]
          )
        colnames(rawdata_values)[colnames(rawdata_values) ==
                                   "cell"] <- "cellnumber"
        colnames(rawdata_values)[colnames(rawdata_values) ==
                                   "value"] <- "lightvalue"
        if (harmonized_lights == FALSE){
          rawdata_quality <- data.frame(
            suppressWarnings(raster::extract(
              qualitydata_uncut, shapefile, cellnumbers = TRUE))
            )
          rawdata_values <- cbind(rawdata_values, rawdata_quality$value)
          colnames(rawdata_values)[colnames(rawdata_values) ==
                                     "rawdata_quality$value"] <- "number_obs"
          rawdata_values <- rawdata_values[,c("cellnumber",
                                              "lightvalue",
                                              "number_obs",
                                              "x", "y")]
        } else if (harmonized_lights == TRUE){
          rawdata_values <- rawdata_values[,c("cellnumber",
                                              "lightvalue",
                                              "x", "y")]
        }
        if (lightdata_time == "monthly"){
          rawdata_name <- paste0("rawlights_",
                                 area_name, "_",
                                 year, "-",
                                 month)
        } else if (lightdata_time == "yearly"){
          rawdata_name <- paste0("rawlights_",
                                 area_name, "_",
                                 year)
        }
        assign(rawdata_name, rawdata_values, envir = .GlobalEnv)
      }

      if (single_dataframes == TRUE){
        # if user sets this to TRUE, the output will not only be an
        # aggregated dataframe for all countries, but also an additional one
        # for each country
        single_df_name <- paste0(area_name, "_lights")
        assign(single_df_name, all_aggregated, envir = .GlobalEnv)
      }

      if (i == 1){
        lights <- all_aggregated
        # build dataframe called lights with all areas, start with
        # first area, which means i = 1
      } else if (i > 1){
        lights <- rbind(lights, aggregated)
        # bind to the lights dataframe if area loop is
        # at least at the second iteration
      }

    } # end sequence loop

    rm(area_name)
    rm(ISO3)
    shapefile <- NULL

  } # end area loop

  lights <<- lights # load aggregated dataframe with all regions
  # into global environment

  if (lightdata_time == "yearly" &
      corrected_lights == FALSE &
      harmonized_lights == FALSE){
    if (dmsp_consistent == TRUE | length(sequence) == 1){
      print(paste0("The consistent DMSP version selected for your timespan is ",
                   dmsp_stump, "."))
    } else if (dmsp_consistent == FALSE & length(sequence) > 1){
      print(paste0("It was not possible to select a consistent DMSP version ",
      "for your timespan. For each year, the newest DMSP version was chosen."))
    }
  }

} # end function
