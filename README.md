# nightlightstats

Night light satellite data can be useful as a proxy for economic activity in a region for which no GDP data are available, for example at the sub-national level, or if GDP measurement is of poor quality, for example in some developing countries (see e.g. Henderson et al., 2012). 

This package allows to perform calculations on night light satellite data and build databases for any given region using the function `nightlight_calculate`. Plots of the night lights in the desired area are also made very easy with `nightlight_plot`.

You can either work with yearly DMSP data ranging from 1992 to 2013 (https://www.ngdc.noaa.gov/eog/dmsp/downloadV4composites.html - Image and data processing by NOAA's National Geophysical Data Center, DMSP data collected by US Air Force Weather Agency) or monthly VIIRS data beginning in Apr 2012 (https://eogdata.mines.edu/download_dnb_composites.html - Earth Observation Group, Payne Institute for Public Policy). The package (if desired) automatically downloads country-level spatial data from GADM (https://gadm.org/data.html).
 
At the time of writing, the yearly VIIRS data are not uploaded so the package does not process these data. Please contact the authors if you notice that this has changed. 

Note about the DMSP night light images: There are several versions of DMSP satellites. For some years, these versions overlap and there are two images available for a year. `nightlight_download` always downloads both. Then, for `nightlight_calculate` and `nightlight_plot`, the versions to be used will be chosen based on the timespan you input into the functions. If possible, a consistent version for your timespan will be chosen. If 2 consistent versions are available for your timespan, the newer one will be selected. If no consistent version is available, the newest version for each year will be chosen.

## nightlight_download

You will want to use this function first if you want to start working with night light satellite data. The function is simply a tool that facilitates downloading the data, so you do not have to do that by hand.

A note about the disk space used by the files: The yearly data are of lower resolution than the monthly data and take up less space (1 year image for the whole world = 1/16 space of a monthly image for the whole world). Hence, yearly data will probably be fine on your normal drive (all years together ca. 45 GB incl. quality indicator files), but working with monthly data likely requires an external drive (about 1.5+ TB for all files incl. quality indicator files). Quality indicator files (recognizable by the ending cf_cvg) show how many observations went into the value of a pixel in the aggregated night light image in a given period.

For the yearly data, you can just use the `time` argument. The DMSP data are available per year in one image for the whole world. To download all of the data at once, you can input:

    nightlight_download(
    time = c("1992", "2013"),
    light_location = "D:"/nightlights)

The monthly data are a bit more tricky. VIIRS images divide the whole world into 6 geographic tiles. You have the option of either downloading only the tile you need (by inputting geographic information, either through coordinates or using a shapefile) or by downloading all 6 tiles (by providing no geographic information). Note: it may happen that the region you want to analyze is overlapping on two or more of these tiles. In that case, all of them will be downloaded.

For example, if you only want to analyze the night lights of Germany in the year 2013, you can input:

    nightlight_download(
    area_names = "Germany",
    time = c("2013-01", "2013-12"),
    shapefile_location = "D:/shapefiles",
    light_location = "D:"/nightlights)
    
or

    nightlight_download(
    time = c("2013-01", "2013-12"),
    light_location = "D:"/nightlights)
    user_coordinates = c(5.866, 15.042, 47.270, 55.057)

In the first example, the shapefile of Germany will automatically be downloaded from the GADM database (only possible for countries!), or, if there is already a shapefile for Germany present in your `shapefile_location`, it will automatically be detected. In the second example, you simply provide the minium and maximum coordinates of Germany. The downloaded tile (in this case just one) of the VIIRS data will be the same, but no shapefile will be downloaded from GADM.

## nightlight_calculate

This function allows to perform calculations on night lights of a region in a given time interval. The output will be an aggregated dataframe in your environment, or if desired an additional dataframe for each area provided in `area_names`. For these single dataframes, you have to set `single_dataframes` to TRUE.

For example, if you want to get a dataframe for the DMSP night lights in adm 1 regions of Luxembourg between 1992 and 1995, you can input:

    nightlight_calculate(
    area_names = "Luxembourg",
    time = c("1992", "1995"),
    shapefile_location = "D:/shapefiles",
    light_location = "D:/nightlights",
    admlevel = 1)

which will give you this dataframe called "lights" in the R environment:

<img src="screenshot_calculate_luxembourg_adm1.png" width="710">

You can see that there are some useful default output elements. Firstly, you get the name of the area you input in `area_names`. If there is an ISO3 countrycode in case this area is a country, this will be registered and output as well. The area in square kilometers will automatically be calculated based on your shapefiles or coordinates, so you can easily integrate it into further calculations. NAME_1 indicates the names of the adm 1 regions of Luxembourg. Columns with lower-level administrative names will only appear if you specify the adm level in the argument `admlevel` (the default is 0, which refers to country borders for countries or does nothing in case your shapefile is e.g. a city or other region not included in a system of administrative districts). mean_obs refers to the mean number of observations per pixel that went into the aggregated image for a time period in a given area. Useful default calculations are the sum of the light values in your area and their mean. Outliers can easily be identfied with the minimum and maximum light values. 

You can, however, use any function for the calculations that you wish. You have to load it into your environment first in case it is a user-written function. Existing functions from base R or packages work as well. Then, you can input the name of the R object as a string into a vector using the argument `functions_calculate`. The function has to accept an `na.rm` argument. In case it does not, you have to wrap the function accordingly. If encountering problems, check the documentation of `raster::extract`, into which all functions are fed. This function sets the conditions for which other functions than the default settings work.

Other useful arguments in `nightlight_calculate`, for which you can consult the helpfiles for further details about their specific usage are:

- `rawdata`: This argument allows to output a dataframe with simply the raw light pixels and their values and coordinates for each region and time period additionally to the standard processed dataframe.

- `cut_low`, `cut_high`, `cut_quality`: These arguments allow to exclude certain pixels from the calculation. If desired, any values smaller than `cut_low`, any values larger than `cut_high` and any pixels with number of observations less or equal to `cut_quality` will be set to NA. The default setting for `cut_quality` is 0, which means that pixels with 0 observations in a time period will be set to NA.

- `rectangle_calculate`: In case your shapefile does not feature an enclosed area with which calculations can be performed, the code will automatically transform your shapefile into a rectangle based on the minimum/maximum x- and y-coordinates. If this for some reason does not work, you can set this to TRUE or FALSE manually (the default is NULL, which activates automatic detection, but for non-standard shapefiles the detection might fail). Below, you can see an illustration of what the code would do in case you input this shapefile with a non-enclosed area (the railway system of Ulm, Germany).

<img src="rectangle_ulm.png" width="550">

## nightlight_plot

This function allows to plot a shapefile with its night lights for a given period of time. Note: even though it is possible to produce multiple plots by using multiple inputs for `area_names` and a timespan for `time`, you should pay attention to the number of plots that will be produced this way - all plots will be loaded into your global environment as ggplot objects, hence a large number of objects can be loaded into your environment quickly.

The basic input arguments are the same as for the other functions. For example, if you input:

    nightlight_plot(
    area_names = "Germany",
    time = "1992",
    shapefile_location = "D:/shapefiles",
    light_location = "D:/nightlights",
    admlevel = 1)

You get the following image, either by already having the shapefile for Germany adm1 stored in your `shapefile_location`, or, if this is not the case, by the function automatically downloading the shapefile from GADM.

<img src="germany_adm1.png" width="270">

In case you want to plot a region that is not availale on GADM (i.e. a region that is not a country), you must have the downloaded shapefile in your `shapefile_location`, so the function can detect it according to the name you give in `area_names`. If this fails, there is always the option to use the `shapefiles` argument and just give the filenames of the shapefiles instead (you still have to set the `area_names` for the naming of the output).

In case you input a set of coordinates, you will get an image with a rectangular shapefile constructed from your coordinates.

## Caveats and limitations of the package and the data

- The code is not explicitly written for fast performance.

- The yearly DMSP data are of suboptimal quality. Problems are a lower resolution and more blooming/blurring of the lights compared to the VIIRS data. Moreover, the DMSP data feature a discrete scale that is top-coded at a digital number of 63, compared to the VIIRS data which have a continuous scale and no top-coding. Detection for low illumination ranges is also better in VIIRS.

- You could use a pareto distribution to circumvent top-coding and extrapolate light values e.g. in city centers (see Bluhm & Krause, 2018).

- There is a [Matlab code](https://github.com/alexeiabrahams/nighttime-lights) to de-blur the DMSP data (see Abrahams et al., 2018).

- The monthly data lack values for summer months when a region is not close to equator due to stray light during summer nights when the sun sets late. This is an issue with the DMSP data as well, but since there are yearly composite images for the DMSP data, it is not directly visible. However, fewer values of the summer months have gone into calculating the yearly composite image the further you go away from the equator.

- Snowfall in winter influences light reflection and increases brightness depending on the amount of snowfall. This point and the previous one can nicely be illustrated by looking at the mean of light values of Moscow, a bright city with a lot of snow in winter and a large distance to the equator. You can see that values for the summer months are not available and that the values in the winter months fluctuate strongly.

<figure>
  <img src="moscow.png" width="600">
  <figcaption>Monthly light means for Moscow.</figcaption>
</figure>

## References

- Abrahams, A., Oram, C., & Lozano-Gracia, N. (2018). Deblurring DMSP nighttime lights: A new method using Gaussian filters and frequencies of illumination. Remote Sensing of Environment, 210, 242-258.

- Bluhm, R. & Krause, M. (2018). Top lights - Bright cities and their contribution to economic development. CESifo Working Paper No. 7411.

- Henderson, J. V., Storeygard, A., & Weil, D. N. (2012). Measuring economic growth from outer space. American Economic Review, 102(2).
