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

    if (year == "1992" | "1993"){
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
    list_quality <- list_quality[grep("stable", list_quality)]
    list_quality <- list_quality[grep(dmsp_stump, list_quality)]
    qualityfile <- list_quality[grep(year, list_quality)]
    qualitydata <- paste0(light_location, "/", qualityfile)
    qualitydata <- raster::raster(qualitydata)
    qualitydata <- raster::crop(qualitydata, extent)
  }
}

lightdata[qualitydata <= cut_quality] <- NA
