#' nightlight_download
#'
#' You can download DMSP nightlight data with this function.
#' Note: the yearly DMSP data are of lower resolution than the monthly VIIRS data and takes
#' up less space (1 year image for the whole world = 1/16 space of a monthly
#' image for the whole world). Hence, yearly DMSP data will be fine on your normal
#' drive (all years together ca. 45 GB incl. quality indicator files), but
#' working with monthly VIIRS data likely requires an external drive (about 1.5+
#' TB for all files incl. quality indicator files). For some years,
#' there are 2 DMSP versions available. The code downloads both. There are
#' alternative data sources available, see the corrected_lights and
#' harmonized_lights arguments for these.
#'
#' @param area_names Default is "world", in which case data for the whole world
#' will be downloaded (no difference for yearly data DMSP because there the whole
#' world is one file; important for monthly data). This is a string (vector) with the
#' name(s) of your region(s). If you provide a country name, the country
#' shapefile will be downloaded automatically. Shapefiles in your shapefile
#' location that have the area_names or (in case they exist) their iso3c
#' countrycodes in their filename will be detected automatically. If you provide
#' own shapefiles in the shapefiles argument, put the names in order of the
#' shapefile order. If you only put one name for multiple shapefiles, all
#' shapefiles will be processed with that name.
#' @param time May not be empty. Vector of strings with the start/end dates in
#' the format "2012-04" (monthly data) or "1992" (yearly data). If only one
#' time period is desired, then simply input one date.
#' @param light_location May not be empty. Provide the location where you store
#' your light files on your drive as a string.
#' @param shapefile_location May be empty. Provide the location of the
#' shapefiles on your drive as a string in the case that you want to use
#' shapefiles and not a set of coordinates.
#' @param shapefiles May be empty. You can provide own shapefiles here (input
#' are the filenames) if no automatic download is desired. If there is a
#' shapefile in your shapfile location that has either the name of the region
#' which you enter in area_names or the iso3c countrycode (if it is a
#' country) in its filename, it will be detected automatically and you do not
#' have to use this argument.
#' @param download_shape Default is ".gpkg". Change to ".rds", ".shp" or
#' ".kml" if you want to download a different shapefile format from GADM. Will
#' only download if no own shapefiles are provided in the shapefiles argument
#' or automatically detected in the shapefile location.
#' @param gpkg_layer May be empty. You might need this argument if the code
#' does not detect the correct layer of a .gpkg file automatically. This can
#' happen if the layers of your .gpkg shapefile do not include an admlevel in
#' their names. In that case, enter the layer here as a string. Note that this
#' only works for one area at a time. To find out which layers are included
#' in your .gpkg shapefile, you can use rgdal::ogrListLayers().
#' @param admlevel Default is 0. Change this when working with different
#' administrative levels.
#' @param user_coordinates May be empty. Inputs are decimal numbers (longlat).
#' Can be specified if you want to download night lights for a region
#' defined by specific coordinates. Input order is xmin, xmax, ymin, ymax.
#' @param corrected_lights Default is FALSE. If set to TRUE, the
#' radiance-calibrated version of the DMSP data will be downloaded.
#' @param harmonized_lights Default is FALSE. If set to TRUE, the harmonized
#' DMSP-VIIRS yearly dataset by Li et al. (2020) will be used. Note that you
#' have to download these versions first, since they are different images from
#' the standard ones.
#' @export

nightlight_download <- function(area_names = "world",
                                time,
                                light_location,
                                shapefile_location = NULL,
                                shapefiles = NULL,
                                download_shape = ".gpkg",
                                gpkg_layer = NULL,
                                admlevel = 0,
                                user_coordinates = NULL,
                                corrected_lights = FALSE,
                                harmonized_lights = FALSE){
  
  lightdata_time <- help_shapefiles <- ISO3s <- NULL # set the variables to
  # NULL first to bind variables so there is no "binding for global variables"
  # problem later
  
  source_setup(light_location = light_location,
               shapefile_location = shapefile_location,
               time = time, area_names = area_names,
               shapefiles = shapefiles)
  
  if (lightdata_time == "yearly"){
    
    if (corrected_lights == FALSE & harmonized_lights == FALSE){
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
        lightfile_test_all <- paste0(light_location, "/",
                                     stump2_all, stump4,
                                     "_web.stable_lights.avg_vis.tif")
        qualityfile_test_all <- paste0(light_location, "/",
                                       stump2_all, stump4, "_web.cf_cvg.tif")
        
        for (g in 1:length(lightfile_test_all)){
          
          lightfile_test <- lightfile_test_all[g]
          qualityfile_test <- qualityfile_test_all[g]
          stump2 <- stump2_all[g]
          
          if(!file.exists(lightfile_test) | !file.exists(qualityfile_test)){
            
            utils::download.file(url = paste0(stump1,
                                              stump2,
                                              stump3),
                                 destfile = paste0(light_location, "/",
                                                   stump2, ".v4.tar"),
                                 mode = "wb")
            utils::untar(paste0(light_location, "/", stump2, ".v4.tar"),
                         exdir = paste0(light_location, "/"))
            
            if (!file.exists(lightfile_test)){
              lightfile <- list.files(light_location)
              lightfile <- lightfile[grep(lightfile, pattern = "stable")]
              lightfile <- lightfile[grep(lightfile, pattern = ".gz")]
              lightfile <- lightfile[grep(lightfile, pattern = year)]
              R.utils::gunzip(filename = paste0(light_location, "/",
                                                lightfile),
                              destname = paste0(
                                light_location, "/",
                                stump2, stump4,
                                "_web.stable_lights.avg_vis.tif"))
            }
            
            if (!file.exists(qualityfile_test)){
              qualityfile <- list.files(light_location)
              qualityfile <- qualityfile[grep(qualityfile, pattern = "cf_cvg")]
              qualityfile <- qualityfile[grep(qualityfile, pattern = ".gz")]
              qualityfile <- qualityfile[grep(qualityfile, pattern = year)]
              R.utils::gunzip(filename = paste0(light_location, "/",
                                                qualityfile),
                              destname = paste0(light_location, "/",
                                                stump2, stump4,
                                                "_web.cf_cvg.tif"))
            }
            
            remove_files <- list.files(light_location)
            remove_files <- remove_files[grep(remove_files,
                                              pattern = stump2)]
            remove_files <- remove_files[-grep(
              remove_files, pattern = "_web.stable_lights.avg_vis.tif")]
            remove_files <- remove_files[-grep(
              remove_files, pattern = "_web.cf_cvg.tif")]
            unlink(paste0(light_location, "/",
                          remove_files), recursive = TRUE)
            
          } else if (file.exists(lightfile_test) &
                     file.exists(qualityfile_test)){
            print(paste0("The light file and quality file for ", year,
                         " are already downloaded."))
          }
          
        } # end test loop
        
      } # end sequence loop
      
    } else if (corrected_lights == TRUE & harmonized_lights == FALSE){
      
      stump1 <- "https://eogdata.mines.edu/wwwdata/dmsp/rad_cal/"
      stump3 <- "_rad_v4"
      
      for (j in 1:length(sequence)){
        year <- sequence[j]
        
        if (year == "1996"){
          stump2 <- "F12_19960316-19970212"
        } else if (year == "1999"){
          stump2 <- "F12_19990119-19991211"
        } else if (year == "2000"){
          stump2 <- "F12-F15_20000103-20001229"
        } else if (year == "2003"){
          stump2 <- "F14-F15_20021230-20031127"
        } else if (year == "2004"){
          stump2 <- "F14_20040118-20041216"
        } else if (year == "2006"){
          stump2 <- "F16_20051128-20061224"
        } else if (year == "2010"){
          stump2 <- "F16_20100111-20101209"
        } else {
          next # sequence fills all years, so skip to next year if current one
          # is not covered by corrected DMSP
        }
        
        # test whether lightfile and/or qualityfile is already downloaded
        lightfile_test <- paste0(light_location, "/",
                                 stump2, stump3, ".avg_vis.tif")
        qualityfile_test <- paste0(light_location, "/",
                                   stump2, stump3, ".cf_cvg.tif")
        
        if(!file.exists(lightfile_test) | !file.exists(qualityfile_test)){
          
          utils::download.file(url = paste0(stump1, stump2, stump3,
                                            ".geotiff.tgz"),
                               destfile = paste0(light_location, "/",
                                                 stump2, stump3,
                                                 ".geotiff.tgz"),
                               mode = "wb")
          R.utils::gunzip(filename = paste0(light_location, "/",
                                            stump2, stump3, ".geotiff.tgz"),
                          destname = paste0(light_location, "/",
                                            stump2, stump3, ".geotiff.tar"))
          utils::untar(paste0(light_location, "/",
                              stump2, stump3, ".geotiff.tar"),
                       exdir = paste0(light_location, "/"))
          
          remove_files <- list.files(light_location)
          remove_files <- remove_files[grep(remove_files,
                                            pattern = stump2)]
          remove_files <- remove_files[-grep(remove_files,
                                             pattern = "_rad_v4.avg_vis.tif")]
          remove_files <- remove_files[-grep(remove_files,
                                             pattern = "_rad_v4.cf_cvg.tif")]
          unlink(paste0(light_location, "/", remove_files), recursive = TRUE)
          
        } else if (file.exists(lightfile_test) &
                   file.exists(qualityfile_test)){
          print(paste0("The light file and quality file for ", year,
                       " are already downloaded."))
        }
        
      } # end sequence loop
      
    } else if (corrected_lights == TRUE & harmonized_lights == TRUE){
      stop(paste0("Please choose either harmonized, corrected or ",
                  "standard yearly lights."))
      
    } else if (corrected_lights == FALSE & harmonized_lights == TRUE){
      
      for (j in 1:length(sequence)){
        year <- sequence[j]
        
        stump1 <- "https://ndownloader.figshare.com/files/17626"
        stump3 <- "Harmonized_DN_NTL_"
        stump_dmsp <- "_calDMSP.tif"
        stump_viirs <- "_simVIIRS.tif"
        
        if (year == "1992"){
          stump2 <- "052"
        } else if (year == "1993"){
          stump2 <- "055"
        } else if (year == "1994"){
          stump2 <- "061"
        } else if (year == "1995"){
          stump2 <- "067"
        } else if (year == "1996"){
          stump2 <- "070"
        } else if (year == "1997"){
          stump2 <- "073"
        } else if (year == "1998"){
          stump2 <- "079"
        } else if (year == "1999"){
          stump2 <- "082"
        } else if (year == "2000"){
          stump2 <- "085"
        } else if (year == "2001"){
          stump2 <- "088"
        } else if (year == "2002"){
          stump2 <- "091"
        } else if (year == "2003"){
          stump2 <- "094"
        } else if (year == "2004"){
          stump2 <- "097"
        } else if (year == "2005"){
          stump2 <- "100"
        } else if (year == "2006"){
          stump2 <- "103"
        } else if (year == "2007"){
          stump2 <- "109"
        } else if (year == "2008"){
          stump2 <- "016"
        } else if (year == "2009"){
          stump2 <- "019"
        } else if (year == "2010"){
          stump2 <- "022"
        } else if (year == "2011"){
          stump2 <- "025"
        } else if (year == "2012"){
          stump2 <- "031"
        } else if (year == "2013"){
          stump2 <- "034"
        } else if (year == "2014"){
          stump2 <- "037"
        } else if (year == "2015"){
          stump2 <- "040"
        } else if (year == "2016"){
          stump2 <- "043"
        } else if (year == "2017"){
          stump2 <- "046"
        } else if (year == "2018"){
          stump2 <- "049"
        }
        
        numericyear <- as.numeric(year)
        
        if (numericyear >= 1992 & numericyear < 2014){
          utils::download.file(url = paste0(stump1, stump2),
                               destfile = paste0(light_location, "/",
                                                 stump3, year, stump_dmsp),
                               mode = "wb")
        } else if (numericyear >= 2014){
          utils::download.file(url = paste0(stump1, stump2),
                               destfile = paste0(light_location, "/",
                                                 stump3, year, stump_viirs),
                               mode = "wb")
        }
        
      } # end sequence loop
      
    } # end corrected_lights if condition
    
  } else if (lightdata_time == "monthly"){
    
    warning(paste0("The Colorado School of Mines has changed the access to ",
                "their VIIRS nighttime light data to require a free user account. ",
                "Please download the files manually to proceed. For further ",
                "information, see https://payneinstitute.mines.edu/eog-2/transition-to-secured-data-access/"))
    
  #    stumpurl = paste0("https://eogdata.mines.edu/pages/",
  #                      "download_dnb_composites_iframe.html")
  #   
  #    Scrape the links directly from the overview page
  #    overview_page <- xml2::read_html(stumpurl)
  #    links <- rvest::html_attr(rvest::html_nodes(overview_page, "a"), "href")
  #    rm(overview_page)
  #    
  #    if ((!is.null(user_coordinates) & area_names == "world") |
  #        (!is.null(shapefiles) & area_names == "world")){
  #      area_names <-  c("")
  #    } # in case someone gives coordinates or shapefiles but leaves
  #    area_names at world,
  #    this gives the area an empty name
  #    so the "world" download will not be activated
  #   
  #   if (area_names == "world"){
  #     
  #     for (j in 1:length(sequence)){
  #       
  #       year <- as.character(data.table::year(sequence[j]))
  #       numericyear <- as.numeric(year)
  #       month <- as.character(data.table::month(sequence[j]))
  #       
  #       if (nchar(month) == 1){
  #         month <- paste0("0", month)
  #       } # month needs to be in 2-digit format for following
  #       # lightfile-search string
  #       
  #       yearmonth <- paste0(year, month)
  #       
  #       if (month == "01" | month == "03" | month == "05" |
  #           month == "07" | month == "08" | month == "10" |
  #           month == "12"){
  #         numberdays <- c("31")
  #       } else if (month == "04" | month == "06" |
  #                  month == "09" | month == "11"){
  #         numberdays <- c("30")
  #       } else if (month == "02" &
  #                  numericyear %% 4 == 0 &
  #                  numericyear %% 100 != 0){
  #         numberdays <- c("29")
  #       } else if (month == "02" &
  #                  numericyear %% 400 == 0){
  #         numberdays <- c("29")
  #       } else {
  #         numberdays <- c("28")
  #       }
  #       yearmonthspan <- paste0(yearmonth, "01-",
  #                               yearmonth, numberdays)
  #       
  #       links_current <- links[grep(links, pattern = yearmonthspan)]
  #       
  #       tilenumbers <- c("1", "2", "3", "4", "5", "6")
  #       
  #       for (t in 1:length(tilenumbers)){
  #         
  #         tilenumber <- tilenumbers[t]
  #         
  #         if (tilenumber == "1"){
  #           tilestump <- "75N180W"
  #         } else if (tilenumber == "2"){
  #           tilestump <- "75N060W"
  #         } else if (tilenumber == "3"){
  #           tilestump <- "75N060E"
  #         } else if (tilenumber == "4"){
  #           tilestump <- "00N180W"
  #         } else if (tilenumber == "5"){
  #           tilestump <- "00N060W"
  #         } else if (tilenumber == "6"){
  #           tilestump <- "00N060E"
  #         }
  #         
  #         links_current_tile <- links_current[grep(links_current,
  #                                                  pattern = tilestump)]
  #         
  #         if (corrected_lights == FALSE){
  #           links_current_tile <- links_current_tile[grep(links_current_tile,
  #                                                         pattern = "vcmcfg")]
  #         } else if (corrected_lights == TRUE){
  #           links_current_tile <- links_current_tile[grep(links_current_tile,
  #                                                         pattern = "vcmslcfg")]
  #         }
  #         lightfile_test <- strsplit(links_current_tile, "/")
  #         lightfile_test <- lightfile_test[[1]][11]
  #         lightfile_test <- strsplit(lightfile_test, ".tgz")
  #         lightfile_test1 <- paste0(light_location, "/",
  #                                   lightfile_test,
  #                                   ".avg_rade9.tif")
  #         lightfile_test2 <- paste0(light_location, "/",
  #                                   lightfile_test,
  #                                   ".avg_rade9h.tif")
  #         qualityfile_test <- paste0(light_location, "/",
  #                                    lightfile_test,
  #                                    ".cf_cvg.tif")
  #         
  #         if(!(file.exists(lightfile_test1) | file.exists(lightfile_test2)) |
  #            !(file.exists(qualityfile_test))){
  #           utils::download.file(links_current_tile,
  #                                destfile = paste0(light_location, "/",
  #                                                  yearmonth, ".tgz"),
  #                                mode = "wb")
  #           
  #           R.utils::gunzip(filename = paste0(light_location, "/",
  #                                             yearmonth, ".tgz"),
  #                           destname = paste0(light_location, "/",
  #                                             yearmonth, "_unzipped.tar"))
  #           
  #           utils::untar(paste0(light_location, "/",
  #                               yearmonth, "_unzipped.tar"),
  #                        exdir = paste0(light_location, "/"))
  #           
  #           unlink(paste0(light_location, "/", yearmonth, "_unzipped.tar"),
  #                  recursive = TRUE)
  #         } else if ((file.exists(lightfile_test1) |
  #                     file.exists(lightfile_test2)) &
  #                    file.exists(qualityfile_test)){
  #           print(paste0("The light file and quality file for tile ",
  #                        tilenumber, ", ", year, "/", month,
  #                        " are already downloaded."))
  #         }
  #         
  #       } # end tilenumbers loop
  #       
  #     } # end sequence loop
  #     
  #     
  #   }  else if (area_names != "world"){
  #      
  #      for (i in 1:length(area_names)){
  #        
  #        area_name <- area_names[i]
  #        ISO3 <- ISO3s[i]
  #        
  #        if (is.na(ISO3)){
  #          print(paste0("An iso3c countrycode for ", area_name, "could not be ",
  #                       "found, hence the download from GADM will not work. ",
  #                       "Either your shapefile is not a country or, if it is a ",
  #                       "country, the countryname was not recognized correctly."))
  #        }
  #        
  #        # check whether the given adm level exists for the country
  #        # (in case it is a country). do that by simply checking whether an rds url
  #        # on GADM exists (for this file type, each adm level has its separate url,
  #        # which makes things easy)
  #        
  #        if (!is.na(ISO3)){
  #          test_url <- paste0("https://biogeo.ucdavis.edu/data/gadm3.6/Rsp",
  #                             "/gadm36_", ISO3, "_", admlevel, "_sp.rds")
  #          if (!RCurl::url.exists(test_url)){
  #            print(paste0("There is no adm level ", admlevel, " for ", area_name,
  #                         ". The area was skipped."))
  #            next
  #          }
  #        }
  #        
  #        get_shapefile(i = i,
  #                      area_name = area_name,
  #                      ISO3 = ISO3,
  #                      help_shapefiles = help_shapefiles,
  #                      shapefiles = shapefiles,
  #                      admlevel = admlevel,
  #                      gpkg_layer = gpkg_layer,
  #                      user_coordinates = user_coordinates,
  #                      shapefile_location = shapefile_location,
  #                      download_shape = download_shape,
  #                      lightdata_time = lightdata_time)
  #        
  #        for (t in 1:length(tilenumbers)){
  #          
  #          tilenumber <- tilenumbers[t]
  #          
  #          for (j in 1:length(sequence)){
  #            # build a string out of date and search for lightdata that
  #            # matches this date
  #            year <- as.character(data.table::year(sequence[j]))
  #            numericyear <- as.numeric(year)
  #            month <- as.character(data.table::month(sequence[j]))
  #            
  #            if (nchar(month) == 1){
  #              month <- paste0("0", month)
  #            } # month needs to be in 2-digit format for following
  #            # lightfile-search string
  #            
  #            yearmonth <- paste0(year, month)
  #            
  #            if (month == "01" | month == "03" | month == "05" |
  #                month == "07" | month == "08" | month == "10" |
  #                month == "12"){
  #              numberdays <- c("31")
  #            } else if (month == "04" | month == "06" |
  #                       month == "09" | month == "11"){
  #              numberdays <- c("30")
  #            } else if (month == "02" &
  #                       numericyear %% 4 == 0 &
  #                       numericyear %% 100 != 0){
  #              numberdays <- c("29")
  #            } else if (month == "02" &
  #                       numericyear %% 400 == 0){
  #              numberdays <- c("29")
  #            } else {
  #              numberdays <- c("28")
  #            }
  #            yearmonthspan <- paste0(yearmonth, "01-", yearmonth, numberdays)
  #            
  #            links_current <- links[grep(links, pattern = yearmonthspan)]
  #            
  #            if (tilenumber == "1"){
  #              tilestump <- "75N180W"
  #            } else if (tilenumber == "2"){
  #              tilestump <- "75N060W"
  #            } else if (tilenumber == "3"){
  #              tilestump <- "75N060E"
  #            } else if (tilenumber == "4"){
  #              tilestump <- "00N180W"
  #            } else if (tilenumber == "5"){
  #              tilestump <- "00N060W"
  #            } else if (tilenumber == "6"){
  #              tilestump <- "00N060E"
  #            }
  #            
  #            links_current <- links_current[grep(links_current,
  #                                                pattern = tilestump)]
  #            if (corrected_lights == FALSE){
  #              links_current <- links_current[grep(links_current,
  #                                                  pattern = "vcmcfg")]
  #            } else if (corrected_lights == TRUE){
  #              links_current <- links_current[grep(links_current,
  #                                                  pattern = "vcmslcfg")]
  #            }
  #            # test whether light file and/or quality file is already downloaded
  #            lightfile_test <- strsplit(links_current, "/")
  #            lightfile_test <- lightfile_test[[1]][11]
  #            lightfile_test <- strsplit(lightfile_test, ".tgz")
  #            lightfile_test1 <- paste0(light_location, "/",
  #                                      lightfile_test, ".avg_rade9.tif")
  #            lightfile_test2 <- paste0(light_location, "/",
  #                                      lightfile_test, ".avg_rade9h.tif")
  #            qualityfile_test <- paste0(light_location, "/",
  #                                       lightfile_test, ".cf_cvg.tif")
  #            
  #            if(!(file.exists(lightfile_test1) | file.exists(lightfile_test2)) |
  #               !(file.exists(qualityfile_test))){
  #              utils::download.file(links_current,
  #                                   destfile = paste0(light_location, "/",
  #                                                     yearmonth, ".tgz"),
  #                                   mode = "wb")
  #              
  #              R.utils::gunzip(filename = paste0(light_location, "/",
  #                                                yearmonth, ".tgz"),
  #                              destname = paste0(light_location, "/",
  #                                                yearmonth, "_unzipped.tar"))
  #              
  #              utils::untar(paste0(light_location, "/",
  #                                  yearmonth, "_unzipped.tar"),
  #                           exdir = paste0(light_location, "/"))
  #              
  #              unlink(paste0(light_location, "/", yearmonth,
  #                            "_unzipped.tar"), recursive = TRUE)
  #            } else if ((file.exists(lightfile_test1) |
  #                        file.exists(lightfile_test2)) &
  #                       file.exists(qualityfile_test)){
  #              print(paste0("The light file and quality file for tile ",
  #                           tilenumber, ", ", year, "/", month,
  #                           " are already downloaded."))
  #            }
  #            
  #          } # end sequence loop
  #        } # end tilenumbers loop
  #      } # end area loop
  #    } # end area_names != world if condition
  } # end monthly if condition
} # end function
