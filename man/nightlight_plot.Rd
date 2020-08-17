\name{nightlight_plot}
\alias{nightlight_plot}
\title{nightlight_plot}
\description{
Plot a shapefile with its nightlights for a given period of time. Note: even though it is possible to produce multiple plots by using multiple inputs for area_names and a timespan for time, you should pay attention to the number of plots that will be produced this way - all plots will be loaded into the global environment as ggplot objects, hence a large number can load many objects into your environment quickly.}
\usage{
nightlight_plot(
area_names,
time,
light_location,
shapefile_location = NULL,
shapefiles = NULL,
download_shape = "sp.rds",
admlevel = 0,
saveraster = FALSE,
colours = c("white", "darkblue", "gray", "black"),
fixed_scale_low = NULL,
fixed_scale_high = NULL,
cut_quality = 0,
user_projection = NULL,
user_coordinates = NULL,
corrected_lights = FALSE,
harmonized_lights = FALSE)
}
\arguments{
\item{area_names}{May not be empty. String (vector) with the name(s) of your region(s). If you provide a country name, the country shapefile will be downloaded automatically. Shapefiles in your working directory that have the area_names or (in case they exist) their ISO-3 countrycodes in their filename will be detected automatically. If you provide own shapefiles in the shapefiles argument, put the names in order of the shapefile order. If you only put one name for multiple shapefiles, all shapefiles will be processed with that name.}

\item{time}{May not be empty. Vector of strings with the start/end dates in the format "2012-04" (monthly data) or "1992" (yearly data). The monthly data goes from Apr 2012 onwards. The yearly data goes from 1992 to 2013. If only one time period is desired, then simply input one date.}

\item{light_location}{May not be empty. Provide the location of the lightfiles on your drives as a string. Note about the light files: for the DMSP images, some years have 2 versions available. Generally, if possible, a consistent version for your timespan will be chosen. Otherwise the newest version for each year will be selected. If 2 consistent versions are available for your timespan, the newer one will be selected.}

\item{shapefile_location}{May be empty. Provide the location of the shapefiles on your drive as a string in the case that you want to use shapefiles and not a set of coordinates.}

\item{shapefiles}{May be empty. You can provide own shapefiles here (input are the filenames) if no automatic download is desired. If there is a shapefile in your working directory that has either the name of the region which you enter in area_names or the ISO3 of the country (if it is a country) in its filename, it will be detected automatically and you do not have to use this argument.}

\item{download_shape}{Default is "sp.rds". Change to ".shp", ".kml" or ".gpkg" if you want to download a different shapefile format from GADM. Will only download if no own shapefiles are provided in the shapefiles argument or present in the working directory.}

\item{admlevel}{Default is 0. Change this when working with different administrative levels.}

\item{saveraster}{Default is FALSE. If set to true, an additional dataframe with the raw raster nightlight data will be produced for each time period.}

\item{colours}{Vector of strings. Minimum colour of the colour scale (default is "white"), maximum colour of the colour scale (default is "darkblue"), colour for NAs (default is "gray") and colour of the shapefile (default is "black"). Input inside the vector has to be in this order.}

\item{fixed_scale_low}{Default is the minimum light value that is calculated (in the case if this is NULL). You can provide a fixed minimum scale value here to keep the color scale constant across several plots.}

\item{fixed_scale_high}{Default is the maximum light value that is calculated (in the case if this is NULL). You can provide a fixed maximum scale value here to keep the color scale constant across several plots.}

\item{cut_quality}{Default is 0. Will set all light values to NA for which the number of observations that went into the calculation of a pixel is smaller than or equal to cut_quality. Hence, with the default value you will have all pixels set to NA for which 0 observations were made but you can set this to a higher number if you want to drop all pixels below a given number of observations.}

\item{user_projection}{May be empty. If empty, the standard projection is mercator. For another projection, you can choose from the projections listed here (enter the projection as a string without brackets behind it): https://www.rdocumentation.org/packages/mapproj/versions/1.2.7/topics/mapproject}

\item{user_coordinates}{May be empty. Decimal numbers (longlat). Can be specified if you want to calculate for a region defined by specific coordinates. Input order is xmin, xmax, ymin, ymax.}

\item{corrected_lights}{Default is FALSE. If set to TRUE, the radiance-calibrated version of the DMSP data or the straylight-corrected version of the VIIRS data will be used. Note that you have to download these versions first, since they are different images from the standard ones.}

\item{harmonized_lights}{Default is FALSE. If set to TRUE, the harmonized DMSP-VIIRS yearly dataset by Li et al. (2020) will be used. Note that you have to download these versions first, since they are different images from the standard ones.}
}
