\name{nightlight_download}
\alias{nightlight_download}
\title{nightlight_download}
\description{
Download nightlight data, either for the whole world or for a given region. Note: the yearly data is of lower resolution than the monthly data and takes up less space (1 year image for the whole world = 1/20 space of a monthly image for the whole world). Hence, yearly data will be fine on your normal drive (all years together ca. 15 GB), but working with monthly data likely requires an external drive (about 1.5+ TB for all files).}
\usage{
nightlight_download(
area_names = "world",
time_from,
time_to,
light_location,
admlevel = 0,
shapefiles = NULL,
download_shape = "sp.rds",
xmin = NULL,
xmax = NULL,
ymin = NULL,
ymax = NULL}
\arguments{
\item{area_names}{Default is "world", in which case data for the whole world will be downloaded (no difference for yearly data because there the whole world is one file; important for monthly data). String (vector) with the name(s) of your region(s). If you provide a country name, the country shapefile will be downloaded automatically, except if it is already in your working directory. If you provide own shapefiles in the shapefiles argument, put the names in order of the shapefile order. If you only put one name for multiple shapefiles, all shapefiles will be processed with that name.}

\item{time_from}{May not be empty. String with the starting date in the format "2012-04". The monthly data goes from Apr 2012 onwards. If only one time period is desired, then time_from = time_to.}

\item{time_to}{May not be empty. String with the end date in the format "2012-04". The monthly data goes from Apr 2012 onwards. If only one time period is desired, then time_from = time_to.}

\item{light_location}{May not be empty. Provide the location where you store your light files on your drive as a string.}

\item{admlevel}{Default is 0. Change this when working with different administrative levels.}

\item{shapefiles}{May be empty. You can provide own shapefiles here if no automatic download is desired. If there is a shapefile in your working directory that has either the name of the region which you enter in area_names or the ISO3 of the country (if it is a country) in its filename, it will be detected automatically and you do not have to use this argument.}

\item{download_shape}{Default is "sp.rds". Change to ".shp", ".kml" or ".gpkg" if you want to download a different shapefile format from GADM. Will only download if no own shapefiles are provided in the shapefiles argument or present in the working directory.}

\item{xmin}{May be empty. Decimal number (longlat). Can be specified if you want to download lightdata for a rectangle specified by coordinates.}

\item{xmax}{May be empty. Decimal number (longlat). Can be specified if you want to download lightdata for a rectangle specified by coordinates.}

\item{ymin}{May be empty. Decimal number (longlat). Can be specified if you want to download lightdata for a rectangle specified by coordinates.}

\item{ymax}{May be empty. Decimal number (longlat). Can be specified if you want to download lightdata for a rectangle specified by coordinates.}
}
