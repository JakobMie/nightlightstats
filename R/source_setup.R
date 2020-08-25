source_setup <- function(light_location,
                         shapefile_location,
                         time,
                         area_names,
                         shapefiles){

  # need the locations without "/" at the end.
  # in case user types this with "/", remove that
  if (substr(light_location,
             start = nchar(light_location),
             stop = nchar(light_location)) == "/"){
    light_location <- substr(light_location, 1, nchar(light_location) - 1)
  }

  if(!is.null(shapefile_location)){
    if (substr(shapefile_location,
               start = nchar(shapefile_location),
               stop = nchar(shapefile_location)) == "/"){
      shapefile_location <- substr(shapefile_location, 1,
                                   nchar(shapefile_location) - 1)
    }
  }

  time <- as.character(time)
  # need time as character to build strings later
  if (length(time) == 1){
    time_from <- time
    time_to <- time
  } else if (length(time) == 2){
    time_from <- time[1]
    time_to <- time[2]
  } else {
    stop(paste0("Please enter either one or two dates inside a vector for the ",
    "time argument to provide a valid timespan."))
  }

  lightdata_time <- "monthly" # set default to monthly

  if (nchar(time_from) == 4 & nchar(time_to) == 4){
    lightdata_time <- "yearly"
  } # change to yearly if year is given (only possibility for 4 characters)

  # create the time sequence to perform the loop for each period
  # between "from" and "to"
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

  if (length(area_names) == 1 & length(shapefiles) > 1){
    # if multiple shapefiles but only one area name provided: give
    # that name to all shapefiles
    area_names <- rep(area_names, length = length(shapefiles))
    for (l in 1:length(shapefiles)){
      area_names[l] <- paste0(area_names[l], "_", l)
    }
  }

  if (!is.null(shapefiles)){
    shapefiles <- paste0(shapefile_location, "/", shapefiles)
    help_shapefiles <- shapefiles
  } else if (is.null(shapefiles)){
    help_shapefiles <- NA
    # help_shapefiles is a duplicate of shapefiles for later if
    # provided by the user, otherwise NA
  }

  ISO3s <- suppressWarnings(
    countrycode::countrycode(area_names,
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
                                              'Spratly Islands' = 'XSP')))
  # these countries have GADM shapefiles but are not recognized by countrycode

  # assign the variables that are needed outside of this helper
  # function to the parent frame so they are available one level higher

  assign("light_location", light_location, envir = parent.frame())
  assign("shapefile_location", shapefile_location, envir = parent.frame())
  assign("time_from", time_from, envir = parent.frame())
  assign("time_to", time_to, envir = parent.frame())
  assign("lightdata_time", lightdata_time, envir = parent.frame())
  assign("sequence", sequence, envir = parent.frame())
  assign("area_names", area_names, envir = parent.frame())
  assign("shapefiles", shapefiles, envir = parent.frame())
  assign("help_shapefiles", help_shapefiles, envir = parent.frame())
  assign("ISO3s", ISO3s, envir = parent.frame())

}
