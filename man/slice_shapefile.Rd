\name{slice_shapefile}
\alias{slice_shapefile}
\title{slice_shapefile}
\description{
This is an auxiliary function that allows cutting shapefiles into smaller pieces. You may want to use this if you are trying to use nightlight_plot or nightlight_calculate, but your geographic area is too large and the capacities of your PC do not allow running these functions for such a large region. If that is the case, an error message "Cannot allocate vector of size ..." will appear in nightlight_plot or nightlight_calculate. To continue nevertheless, you can use this function to slice your shapefile into smaller pieces. The function will save the sliced pieces on your drive (the original shapefile will be unaffected), then you can continue working with the sliced shapefiles. You can gather the names of the sliced shapefiles into a vector and input the vector into the "shapefiles" arugment of the other functions. The area names will automatically adjust using indices if you simply input one argument (the name of your region) into "area_names".}
\usage{
slice_shapefile(
shapefile,
shapefile_location,
slice_direction,
number_pieces)
}
\arguments{
\item{shapefile}{May not be empty. Provide the filename of your shapefile as a string here.}

\item{shapefile_location}{May not be empty. Provide the location of the shapefile on your drive as a string here.}

\item{slice_direction}{May not be empty. Provide the direction of the slicing as a string here. Options are "horizontal" and "vertical".}

\item{number_pieces}{May not be empty. Provide the number of pieces you want the area to be split up into as an integer here.}
}
