\name{nightlight_calculate}
\alias{nightlight_calculate}
\title{nightlight_calculate}
\description{
Perform calculations on nightlights of a region in a given time interval (defaults are sum of light values, min, mean and max).}
\usage{
nightlight_calculate(
area_names,
time,
light_location,
shapefile_location,
admlevel = 0,
shapefiles = NULL,
download_shape = "sp.rds",
single_dataframes = FALSE,
functions_calculate = NULL,
function_names = NULL,
rectangle_calculate = FALSE,
rawdata = FALSE,
cut_low = NULL,
cut_high = NULL,
cut_quality = 0,
user_coordinates = NULL)
}
\arguments{
\item{area_names}{May not be empty. String (vector) with the name(s) of your region(s). If you provide a country name, the country shapefile will be downloaded automatically. Shapefiles in your shapefile location that have the area_names or (in case they exist) their ISO-3 countrycodes in their filename will be detected automatically. If you provide own shapefiles in the shapefiles argument, put the names in order of the shapefile order. If you only put one name for multiple shapefiles, all shapefiles will be processed with that name.}

\item{time}{May not be empty. Vector of strings with the start/end dates in the format "2012-04" (monthly data) or "1992" (yearly data). The monthly data goes from Apr 2012 onwards. The yearly data goes from 1992 to 2013. If only one time period is desired, then simply input one date.}

\item{light_location}{May not be empty. Provide the location of the lightfiles on your drives as a string.}

\item{shapefile_location}{May be empty. Provide the location of the shapefiles on your drives as a string in the case that you want to use shapefiles and not a set of coordinates.}

\item{admlevel}{Default is 0. Change this when working with different administrative levels. Important for nightlight_calculate: if your shapefile is not from GADM and features admlevels larger than 0, you must rename the column names to NAME_X, X being the admlevel. You can do this by reading the shapefile e.g. with readRDS or rgdal::readOGR, renaming the columns of the SpatialPolygonsDataFrame accordingly and then saving the shapefile again. Only with this naming will the calculation with lower admlevels work if the shapefile is not from GADM.}

\item{shapefiles}{May be empty. You can provide own shapefiles here (input are the filenames) if no automatic download is desired. If there is a shapefile in your shapefile location that has either the name of the region which you enter in area_names or the ISO3 of the country (if it is a country) in its filename, it will be detected automatically and you do not have to use this argument.}

\item{download_shape}{Default is "sp.rds". Change to ".shp", ".kml" or ".gpkg" if you want to download a different shapefile format from GADM. Will only download if no own shapefiles are provided in the shapefiles argument or present in the shapefile location.}

\item{single_dataframes}{Default is FALSE. If set to TRUE, this will not only output a dataframe with all areas into the global environment, but also a dataframe for each area specified in area_names.}

\item{functions_calculate}{Defaults (activated if this is NULL) are sum of light values, min, mean, and max. These calculations will be performed on the night light data for your given time and region. Functions need to support the possibility to set na.rm as an argument. See the documentation of raster::extent, into which the functions are fed, for further details when encountering problems. The functions have to be given in a vector, even if there is only one function.}

\item{function_names}{Defaults (activated if this is NULL) are lightsum, lightmin, lightmean and lightmax. This is a vector of strings and gives names to the columnns in the final dataframe output for each calculation you perform.}

\item{rectangle_calculate}{Default is FALSE. If your shapefile does not allow calculating values within its borders (always possible for GADM shapefiles), you should set this to true. The code will calculate the values for the smallest rectangle that reaches over the full extent of the shapefile. If you are not sure whether this applies for you, you can check whether your shapefile encloses a well-defined area by plotting it.}

\item{rawdata}{Default is FALSE. If set to TRUE, an additional file with the raw night light data, i.e. the light values and their coordinates, will be produced for each time period.}

\item{cut_low}{May be empty. If a value is provided, light values below cut_low will be set to NA before performing calculations.}

\item{cut_high}{May be empty. If a value is provided, light values above cut_high will be set to NA before performing calculations.}

\item{cut_quality}{Default is 0. Will set all light values to NA for which the number of observations that went into the calculation of a pixel is smaller than or equal to cut_quality. Hence, with the default value you will have all pixels set to NA for which 0 observations were made but you can set this to a higher number if you want to drop all pixels below a given number of observations.}

\item{user_coordinates}{May be empty. Decimal numbers (longlat). Can be specified if you want to calculate for a region defined by specific coordinates. Input order is xmin, xmax, ymin, ymax.}
