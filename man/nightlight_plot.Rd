\name{nightlight_plot}
\alias{nightlight_plot}
\title{nightlight_plot}
\description{
Plot a shapefile with its nightlights for a given period of time.}
\usage{
nightlight_plot(
area_names,
time_from,
time_to,
light_location,
output_location = ".",
admlevel = 0,
shapefiles = NULL,
download_shape = "sp.rds",
saveraster = FALSE,
colour_from = "white",
colour_to = "darkblue",
shapefile_colour = "black",
na_colour = "gray",
fixed_scale_low = NULL,
fixed_scale_high = NULL,
user_projection = NULL,
xmin = NULL,
xmax = NULL,
ymin = NULL,
ymax = NULL)
}
\arguments{
\item{area_names}{May not be empty. String (vector) with the name(s) of your region(s). If you provide a country name, the country shapefile will be downloaded automatically. Shapefiles in your working directory that have the area_names or (in case they exist) their ISO-3 countrycodes in their filename will be detected automatically. If you provide own shapefiles in the shapefiles argument, put the names in order of the shapefile order. If you only put one name for multiple shapefiles, all shapefiles will be processed with that name.}

\item{time_from}{May not be empty. String with the starting date in the format "2012-04" (monthly data) or "1992" (yearly data). The monthly data goes from Apr 2012 onwards. The yearly data goes from 1992 to 2013. If only one time period is desired, then time_from = time_to.}

\item{time_to}{May not be empty. String with the end date in the format "2012-04" (monthly data) or "1992" (yearly data). The monthly data goes from Apr 2012 onwards. The yearly data goes from 1992 to 2013. If only one time period is desired, then time_from = time_to.}

\item{light_location}{May not be empty. Provide the location of the lightfiles on your drives as a string.}

\item{output_location}{Default is the working directory. Will output the plot here.}

\item{admlevel}{Default is 0. Change this when working with different administrative levels.}

\item{shapefiles}{May be empty. You can provide own shapefiles here if no automatic download is desired. If there is a shapefile in your working directory that has either the name of the region which you enter in area_names or the ISO3 of the country (if it is a country) in its filename, it will be detected automatically and you do not have to use this argument.}

\item{download_shape}{Default is "sp.rds". Change to ".shp", ".kml" or ".gpkg" if you want to download a different shapefile format from GADM. Will only download if no own shapefiles are provided in the shapefiles argument or present in the working directory.}

\item{saveraster}{Default is FALSE. If set to true, an additional .RData file with the raw raster nightlight data will be produced for each time period.}

\item{colour_from}{Default is "white". Minimum colour of the colour scale.}

\item{colour_to}{Default is "darkblue". Maximum colour of the colour scale.}

\item{shapefile_colour}{Default is "black". Colour of the shapefile in the plot.}

\item{na_colour}{Default is "gray". NA colour of the colour scale.}

\item{fixed_scale_low}{Default is the minimum light value that is calculated (in the case if this is NULL). You can provide a fixed minimum scale value here to keep the color scale constant across several plots.}

\item{fixed_scale_high}{Default is the maximum light value that is calculated (in the case if this is NULL). You can provide a fixed maximum scale value here to keep the color scale constant across several plots.}

\item{user_projection}{May be empty. If empty, the standard projection is mercator. For another projection, you can choose from the projections listed here (enter the projection as a string without brackets behind it): https://www.rdocumentation.org/packages/mapproj/versions/1.2.7/topics/mapproject}

\item{xmin}{May be empty. Decimal number (longlat). Can be specified if you want to plot a region defined by specific coordinates.}

\item{xmax}{May be empty. Decimal number (longlat). Can be specified if you want to plot a region defined by specific coordinates.}

\item{ymin}{May be empty. Decimal number (longlat). Can be specified if you want to plot a region defined by specific coordinates.}

\item{ymax}{May be empty. Decimal number (longlat). Can be specified if you want to plot a region defined by specific coordinates.}
}

