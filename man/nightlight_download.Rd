\name{nightlight_download}
\alias{nightlight_download}
\title{nightlight_download}
\description{
Download nightlight data, either for the whole world or for a given region. Note: the yearly data is of lower resolution than the monthly data and takes up less space (1 year image for the whole world = 1/20 space of a monthly image for the whole world). Hence, yearly data will be fine on your normal drive (all years together ca. 45 GB incl. quality indicator files), but working with monthly data likely requires an external drive (about 1.5+ TB for all files incl. quality indicator files).}
\usage{
nightlight_download(
area_names = "world",
time,
light_location,
shapefile_location = NULL,
admlevel = 0,
shapefiles = NULL,
download_shape = "sp.rds",
dmsp_oldversion = FALSE,
user_coordinates = NULL)
}
\arguments{
\item{area_names}{Default is "world", in which case data for the whole world will be downloaded (no difference for yearly data because there the whole world is one file; important for monthly data). String (vector) with the name(s) of your region(s). If you provide a country name, the country shapefile will be downloaded automatically. Shapefiles in your shapefile location that have the area_names or (in case they exist) their ISO-3 countrycodes in their filename will be detected automatically. If you provide own shapefiles in the shapefiles argument, put the names in order of the shapefile order. If you only put one name for multiple shapefiles, all shapefiles will be processed with that name.}

\item{time}{May not be empty. Vector of strings with the start/end dates in the format "2012-04" (monthly data) or "1992" (yearly data). The monthly data goes from Apr 2012 onwards. The yearly data goes from 1992 to 2013. If only one time period is desired, then simply input one date.}

\item{light_location}{May not be empty. Provide the location where you store your light files on your drive as a string.}

\item{shapefile_location}{May be empty. Provide the location of the shapefiles on your drives as a string in the case that you want to use shapefiles and not a set of coordinates.}

\item{admlevel}{Default is 0. Change this when working with different administrative levels.}

\item{shapefiles}{May be empty. You can provide own shapefiles here (input are the filenames) if no automatic download is desired. If there is a shapefile in your shapfile location that has either the name of the region which you enter in area_names or the ISO3 of the country (if it is a country) in its filename, it will be detected automatically and you do not have to use this argument.}

\item{download_shape}{Default is "sp.rds". Change to ".shp", ".kml" or ".gpkg" if you want to download a different shapefile format from GADM. Will only download if no own shapefiles are provided in the shapefiles argument or present in the shapefile location.}

\item{dmsp_oldversion}{Default is FALSE. The DMSP yearly data have different satellite versions, changing over time. Some years have 2 satellite versions available. By default, the newer version will be chosen if there are 2 for a given year. However, it could be useful to use a consistent satellite version if possible. Years and their possible satellite versions: 1992-1994 "F10",  1994-1999 "F12", 1997-2003 "F14", 2000-2007 "F15", 2004-2009 "F16", 2010-2013 "F18". Inputting TRUE for this argument will give the older versions of the satellites in those cases where 2 are available.}

\item{user_coordinates}{May be empty. Decimal numbers (longlat). Can be specified if you want to calculate for a region defined by specific coordinates. Input order is xmin, xmax, ymin, ymax.}
}
