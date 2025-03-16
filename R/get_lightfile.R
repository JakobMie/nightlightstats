get_lightfile <- function(j,
                          light_location,
                          lightdata_time,
                          sequence,
                          tilenumbers,
                          corrected_lights,
                          harmonized_lights,
                          extent){
  skip_period <- FALSE

  if (lightdata_time == "monthly"){

    year <- format(sequence[j], "%Y")
    numericyear <- as.numeric(year)
    month <- format(sequence[j], "%m")
    
    if (nchar(month) == 1){
      month <- paste0("0", month)
    } # month needs to be in 2-digit format for following
    # lightfile-search string

    yearmonth <- paste0(year, month)

    if (month == "01" | month == "03" | month == "05" |
        month == "07" | month == "08" | month == "10" |
        month == "12"){
      numberdays <- c("31")
    } else if (month == "04" | month == "06" |
               month == "09" | month == "11"){
      numberdays <- c("30")
    } else if (month == "02" &
               numericyear %% 4 == 0 &
               numericyear %% 100 != 0){
      numberdays <- c("29")
    } else if (month == "02" &
               numericyear %% 400 == 0){
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
        stop(paste0("The light file for ", month, "/", year,
                    " could not be found in your light location."))
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
      } else {
        stop(paste0("The quality file for ", month, "/", year,
                    " could not be found in your light location."))
      }
      rm(lightfile)
      rm(qualityfile)
    } # end tilenumbers loop


  } else if (lightdata_time == "yearly"){

    year <- sequence[j]

    if (corrected_lights == FALSE & harmonized_lights == FALSE){
      # select consistent dmsp version according to the start/ end
      # of the time sequence.
      # if this is not possible, choose the newest dmsp version for each year.
      # always check if the required files exist.
      F10_years = as.character(seq(1992, 1994, by = 1))
      F12_years = as.character(seq(1994, 1999, by = 1))
      F14_years = as.character(seq(1997, 2003, by = 1))
      F15_years = as.character(seq(2000, 2007, by = 1))
      F16_years = as.character(seq(2004, 2009, by = 1))
      F18_years = as.character(seq(2010, 2013, by = 1))

      dmsp_stump <- NULL

      # now follow not "else if "conditions but "if" conditions, so the
      # newer consistent version will overwrite the older one
      # if for the selected years there are 2 consistent versions
      if (sequence[1] %in% F10_years &
          utils::tail(sequence, 1) %in% F10_years){
        dmsp_stump <- "F10"
        dmsp_consistent <- TRUE
      }
      if (sequence[1] %in% F12_years &
          utils::tail(sequence, 1) %in% F12_years){
        dmsp_stump <- "F12"
        dmsp_consistent <- TRUE
      }
      if (sequence[1] %in% F14_years &
          utils::tail(sequence, 1) %in% F14_years){
        dmsp_stump <- "F14"
        dmsp_consistent <- TRUE
      }
      if (sequence[1] %in% F15_years &
          utils::tail(sequence, 1) %in% F15_years){
        dmsp_stump <- "F15"
        dmsp_consistent <- TRUE
      }
      if (sequence[1] %in% F16_years &
          utils::tail(sequence, 1) %in% F16_years){
        dmsp_stump <- "F16"
        dmsp_consistent <- TRUE
      }
      if (sequence[1] %in% F18_years &
          utils::tail(sequence, 1) %in% F18_years){
        dmsp_stump <- "F18"
        dmsp_consistent <- TRUE
      }
      # if at this point no consistent version was found, select
      # the newest version for each year
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
      } else { # if user does not have this DMSP version of the light file,
        # search if there is another one, only defined by year. since there are
        # max. 2 versions per year, this should find the other one if it is
        # there. if not, the error message will occur
        lightdata <- list_light[grep("stable", list_light)]
        lightdata <- lightdata[grep(year, lightdata)]
        if (length(lightdata) == 1){
          lightdata <- paste0(light_location, "/", lightdata)
          dmsp_consistent <- FALSE
          if (grepl("F10", lightdata) == TRUE){
            dmsp_stump <- "F10"
          } else if (grepl("F12", lightdata) == TRUE){
            dmsp_stump <- "F12"
          } else if (grepl("F14", lightdata) == TRUE){
            dmsp_stump <- "F14"
          } else if (grepl("F15", lightdata) == TRUE){
            dmsp_stump <- "F15"
          } else if (grepl("F16", lightdata) == TRUE){
            dmsp_stump <- "F16"
          } else if (grepl("F18", lightdata) == TRUE){
            dmsp_stump <- "F18"
          }
          lightdata <- raster::raster(lightdata)
          lightdata <- raster::crop(lightdata, extent)

        } else {
          stop(paste0("The light file for ", year, " could not be found in ",
                      "your light location."))
        }
      }

      list_quality <- list.files(light_location, pattern = "cf_cvg.tif")
      qualitydata <- list_quality[grep(dmsp_stump, list_quality)]
      # here only search for quality file that matches the dmsp stump,
      # i.e. matches the dmsp version selected above. otherwise throw an error
      qualitydata <- qualitydata[grep(year, qualitydata)]
      if (length(qualitydata) == 1){
        qualitydata <- paste0(light_location, "/", qualitydata)
        qualitydata <- raster::raster(qualitydata)
        qualitydata <- raster::crop(qualitydata, extent)
      } else {
        stop(paste0("The quality file for ", year, ", version ", dmsp_stump,
                    " could not be found in your light location."))
      }

    } else if (corrected_lights == TRUE & harmonized_lights == FALSE){
      # select consistent dmsp version according to the start/ end
      # of the time sequence.
      # if this is not possible, choose the newest dmsp version for each year.
      # always check if the required files exist.
      F10_years = as.character(seq(1992, 1994, by = 1))
      F12_years = as.character(seq(1994, 1999, by = 1))
      F14_years = as.character(seq(1997, 2003, by = 1))
      F15_years = as.character(seq(2000, 2007, by = 1))
      F16_years = as.character(seq(2004, 2009, by = 1))
      F18_years = as.character(seq(2010, 2013, by = 1))
      
      dmsp_stump <- NULL
      
      # now follow not "else if "conditions but "if" conditions, so the
      # newer consistent version will overwrite the older one
      # if for the selected years there are 2 consistent versions
      if (sequence[1] %in% F10_years &
          utils::tail(sequence, 1) %in% F10_years){
        dmsp_stump <- "F10"
        dmsp_consistent <- TRUE
      }
      if (sequence[1] %in% F12_years &
          utils::tail(sequence, 1) %in% F12_years){
        dmsp_stump <- "F12"
        dmsp_consistent <- TRUE
      }
      if (sequence[1] %in% F14_years &
          utils::tail(sequence, 1) %in% F14_years){
        dmsp_stump <- "F14"
        dmsp_consistent <- TRUE
      }
      if (sequence[1] %in% F15_years &
          utils::tail(sequence, 1) %in% F15_years){
        dmsp_stump <- "F15"
        dmsp_consistent <- TRUE
      }
      if (sequence[1] %in% F16_years &
          utils::tail(sequence, 1) %in% F16_years){
        dmsp_stump <- "F16"
        dmsp_consistent <- TRUE
      }
      if (sequence[1] %in% F18_years &
          utils::tail(sequence, 1) %in% F18_years){
        dmsp_stump <- "F18"
        dmsp_consistent <- TRUE
      }
      # if at this point no consistent version was found, select
      # the newest version for each year
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
      
      list_light <- list.files(light_location, pattern = "_Corrected.tif")
      lightdata <- list_light[grep(dmsp_stump, list_light)]
      lightdata <- lightdata[grep(year, lightdata)]
      if (length(lightdata) == 1){
        lightdata <- paste0(light_location, "/", lightdata)
        lightdata <- raster::raster(lightdata)
        lightdata <- raster::crop(lightdata, extent)
      } else { # if user does not have this DMSP version of the light file,
        # search if there is another one, only defined by year. since there are
        # max. 2 versions per year, this should find the other one if it is
        # there. if not, the error message will occur
        light_list_alternative <- list_light[grep(year, list_light)]
        if (length(lightdata) == 1){
          lightdata <- paste0(light_location, "/", light_list_alternative)
          dmsp_consistent <- FALSE
          if (grepl("F10", lightdata) == TRUE){
            dmsp_stump <- "F10"
          } else if (grepl("F12", lightdata) == TRUE){
            dmsp_stump <- "F12"
          } else if (grepl("F14", lightdata) == TRUE){
            dmsp_stump <- "F14"
          } else if (grepl("F15", lightdata) == TRUE){
            dmsp_stump <- "F15"
          } else if (grepl("F16", lightdata) == TRUE){
            dmsp_stump <- "F16"
          } else if (grepl("F18", lightdata) == TRUE){
            dmsp_stump <- "F18"
          }
          lightdata <- raster::raster(lightdata)
          lightdata <- raster::crop(lightdata, extent)
          
        } else {
          stop(paste0("The light file for ", year, " could not be found in ",
                      "your light location."))
        }
      }
      
    } else if (corrected_lights == TRUE & harmonized_lights == TRUE){
      stop(paste0("Please choose either standard, ",
                  "corrected or harmonized yearly lights."))

    } else if (corrected_lights == FALSE & harmonized_lights == TRUE){
      qualitydata <- NULL # no quality files available for harmonized version
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
        stop(paste0("The light file for ", year,
                    " could not be found in your light location."))
      }

    } # end corrected_lights if condition

  } # end lightdata_time if condition

  # assign the variables that are needed outside of this helper
  # function to the parent frame so they are available one level higher

  assign("skip_period", skip_period, parent.frame())
  assign("year", year, envir = parent.frame())
  if ((lightdata_time == "monthly")){
    assign("month", month, envir = parent.frame())
  }
  assign("lightdata", lightdata, envir = parent.frame())
  if (harmonized_lights == FALSE){
    assign("qualitydata", qualitydata, envir = parent.frame())
  }
  if (lightdata_time == "yearly" &
      corrected_lights == FALSE &
      harmonized_lights == FALSE){
    assign("dmsp_consistent", dmsp_consistent, envir = parent.frame())
    assign("dmsp_stump", dmsp_stump, envir = parent.frame())
  }

}
