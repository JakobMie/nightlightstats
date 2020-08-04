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
