#' nightlight_plot
#'
#' Plot a shapefile with its nightlights for a given period of time. Note: even
#' though it is possible to produce multiple plots by using multiple inputs for
#' area_names and a timespan for time, you should pay attention to the number
#' of plots that will be produced this way - all plots will be loaded into the
#' global environment as ggplot objects, hence a large number can load many
#' objects into your environment quickly.
#'
#' @param area_names May not be empty. String (vector) with the name(s) of your
#' region(s). If you provide a country name, the country shapefile will be
#' downloaded automatically. Shapefiles in your working directory that have the
#' area_names or (in case they exist) their iso3c countrycodes in their
#' filename will be detected automatically. If you provide own shapefiles in
#' the shapefiles argument, put the names in order of the shapefile order. If
#' you only put one name for multiple shapefiles, all shapefiles will be
#' processed with that name.
#' @param time May not be empty. Vector of strings with the start/end dates in
#' the format "2012-04" (monthly data) or "1992" (yearly data). If only one
#' time period is desired, then simply input one date.
#' @param light_location May not be empty. Provide the location of the
#' lightfiles on your drive as a string. Note about the light files: for the
#' DMSP images, some years have 2 versions available. Generally, if possible,
#' a consistent version for your timespan will be chosen. Otherwise the newest
#' version for each year will be selected. If 2 consistent versions are
#' available for your timespan, the newer one will be selected.
#' @param shapefile_location May be empty. Provide the location of the
#' shapefiles on your drive as a string in the case that you want to use
#' shapefiles and not a set of coordinates.
#' @param shapefiles May be empty. You can provide own shapefiles here
#' (input are the filenames) if no automatic download is desired. If there is a
#' shapefile in your working directory that has either the name of the region
#' which you enter in area_names or the iso3c countrycode (if it is a country)
#' in its filename, it will be detected automatically and you do not have to
#' use this argument.
#' @param download_shape Default is ".gpkg". Change to ".shp" or
#' ".kml" if you want to download a different shapefile format from GADM. Will
#' only download if no own shapefiles are provided in the shapefiles argument
#' or automatically detected in the shapefile location.
#' @param gpkg_layer May be empty. You might need this argument if the code
#' does not detect the correct layer of a .gpkg file automatically. This can
#' happen if the layers of your .gpkg shapefile do not include an admlevel in
#' their names. In that case, enter the layer here as a string. Note that this
#' only works for one area at a time. To find out which layers are included in
#' your .gpkg shapefile, you can use sf::st_layers().
#' @param admlevel Default is 0. Change this when working with different
#' administrative levels.
#' @param saveraster Default is FALSE. If set to TRUE, an additional RData
#' output with the raw raster nightlight data will be produced
#' for each time period.
#' @param colours Vector of strings. Minimum colour of the colour scale
#' (default is "white"), maximum colour of the colour scale (default is
#' "darkblue"), colour for NAs (default is "gray") and colour of the shapefile
#' (default is "black"). The input inside the vector has to be in this order.
#' @param fixed_scale_low Default is the minimum light value that is calculated
#' (in the case if this is NULL). You can provide a fixed minimum scale value
#' here to keep the colour scale constant across several plots.
#' @param fixed_scale_high Default is the maximum light value that is
#' calculated (in the case if this is NULL). You can provide a fixed maximum
#' scale value here to keep the colour scale constant across several plots.
#' @param cut_quality Default is 0. This will set all light values to NA
#' for which the number of observations that went into the calculation of a
#' pixel is smaller than or equal to cut_quality. Hence, with the default
#' value you will have all pixels set to NA for which 0 observations were
#' made. You can set this to a higher number if you want to drop all pixels
#' below a given number of observations.
#' @param user_projection May be empty. If empty, the standard projection is
#' mercator. For another projection, you can choose from the projections listed
#' here (enter the projection as a string without brackets):
#' https://www.rdocumentation.org/packages/mapproj/versions/1.2.7/topics/mapproject
#' @param user_coordinates May be empty. Inputs are decimal numbers (longlat).
#' Can be specified if you want to plot night lights for a region
#' defined by specific coordinates. Input order is xmin, xmax, ymin, ymax.
#' @param corrected_lights Default is FALSE. If set to TRUE, the
#' radiance-calibrated version of the DMSP data or the straylight-corrected
#' version of the VIIRS data will be used. Note that you have to download these
#' versions first, since they are different images from the standard ones. The data
#' refer to the corrected lights by Bluhm and Krause (2022).
#' @param harmonized_lights Default is FALSE. If set to TRUE, the harmonized
#' DMSP-VIIRS yearly dataset by Li et al. (2020) will be used. Note that you
#' have to download these versions first, since they are different images
#' from the standard ones.
#' @export

nightlight_plot <- function(area_names,
                            time,
                            light_location,
                            shapefile_location = NULL,
                            shapefiles = NULL,
                            download_shape = ".gpkg",
                            gpkg_layer = NULL,
                            admlevel = 0,
                            saveraster = FALSE,
                            colours = c("white", "darkblue", "gray", "black"),
                            fixed_scale_low = NULL,
                            fixed_scale_high = NULL,
                            cut_quality = 0,
                            user_projection = NULL,
                            user_coordinates = NULL,
                            corrected_lights = FALSE,
                            harmonized_lights = FALSE){

  group <- lat <- light <- lon <- long <- raster_lights <- help_shapefiles <-
    lightdata_time <- tilenumbers <- area_name <- ISO3 <- ISO3s <-
    dmsp_consistent <- dmsp_stump <- skip_period <- extent <- year <-
    month <- NULL # set the variables
  # to NULL first to bind variables so there is no "binding for global
  # variables" problem later

  if (is.null(shapefile_location) & is.null(user_coordinates)){
    stop(paste0("Please input geographical information, either by providing ",
               "shapefiles (do not forget to use shapefile_location to ",
               "specify the location on your drive) or a set of coordinates."))
  }

  source_setup(light_location = light_location,
        shapefile_location = shapefile_location,
        time = time, area_names = area_names,
        shapefiles = shapefiles)

  for (i in 1:length(area_names)){

    area_name <- area_names[i]
    ISO3 <- ISO3s[i]

    if (is.na(ISO3)){
      print(paste0("An iso3c countrycode for ", area_name, "could not be ",
                   "found, hence the download from GADM will not work. Either ",
                   "your shapefile is not a country or, if it is a country, ",
                   "the countryname was not recognized correctly."))
    }

    get_shapefile(i = i,
                  area_name = area_name,
                  ISO3 = ISO3,
                  help_shapefiles = help_shapefiles,
                  shapefiles = shapefiles,
                  admlevel = admlevel,
                  gpkg_layer = gpkg_layer,
                  user_coordinates = user_coordinates,
                  shapefile_location = shapefile_location,
                  download_shape = download_shape,
                  lightdata_time = lightdata_time)

    for (j in 1:length(sequence)){

      get_lightfile(j = j,
                    light_location = light_location,
                    lightdata_time = lightdata_time,
                    sequence = sequence,
                    tilenumbers = tilenumbers,
                    corrected_lights = corrected_lights,
                    harmonized_lights = harmonized_lights,
                    extent = extent)

      if (skip_period == TRUE){
        next
      }

      if (saveraster == TRUE){
        if (lightdata_time == "monthly"){
          raster_name <- paste0("raster_", area_name, "_", year, "-", month)
        } else if (lightdata_time == "yearly"){
          raster_name <- paste0("raster_", area_name, "_", year)
        }
        assign(raster_name, lightdata, envir = .GlobalEnv)
      }

      lightdata <- raster::rasterToPoints(lightdata)
      df_light <- data.frame(lightdata)
      colnames(df_light) <- c("lon", "lat", "light")

      if (harmonized_lights == FALSE){
        qualitydata <- raster::rasterToPoints(qualitydata)
        df_quality <- data.frame(qualitydata)
        colnames(df_quality) <- c("lon", "lat", "obs")
        df_light$light[df_quality$obs <= cut_quality] <- NA
      }

      if (is.null(fixed_scale_low)){
        fixed_scale_low_plot <- suppressWarnings(min(df_light$light,
                                                     na.rm = TRUE))
      } else if (!is.null(fixed_scale_low)){
        fixed_scale_low_plot <- fixed_scale_low
      }

      if (is.null(fixed_scale_high)){
        fixed_scale_high_plot <- suppressWarnings(max(df_light$light,
                                                      na.rm = TRUE))
      } else if (!is.null(fixed_scale_high)){
        fixed_scale_high_plot <- fixed_scale_high
      }

      if (lightdata_time == "monthly"){
        title <- paste0("Nighttime Lights ", area_name,
                        " (", month, "/", year, ")")
      } else if (lightdata_time == "yearly"){
        title <- paste0("Nighttime Lights ", area_name, " (", year, ")")
      }

      if (is.null(user_projection)){
      plot_lights <- ggplot2::ggplot() +
          ggplot2::geom_tile(data = df_light,
                             mapping = ggplot2::aes(x = lon,
                                                    y = lat,
                                                    fill = light),
                             alpha = 0.8) +
          ggplot2::scale_fill_gradientn(
            limits = c(fixed_scale_low_plot,
                       fixed_scale_high_plot),
            colors = plotrix::smoothColors(colours[1], 100, colours[2]),
            na.value = colours[3]) +
          ggplot2::coord_quickmap() +
          ggplot2::geom_sf(data = shapefile,
                             colour = colours[4], fill = NA) +
          ggplot2::labs(title = title) +
          ggplot2::scale_x_continuous(
            guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::theme_bw() +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                         panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         axis.title.x = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank(),
                         axis.title.y = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_blank(),
                         axis.ticks.y = ggplot2::element_blank())
      } else if (!is.null(user_projection)){
        plot_lights <- ggplot2::ggplot() +
          ggplot2::geom_tile(data = df_light,
                             mapping = ggplot2::aes(x = lon,
                                                    y = lat,
                                                    fill = light),
                             alpha = 0.8) +
          ggplot2::scale_fill_gradientn(limits = c(fixed_scale_low_plot,
                                                   fixed_scale_high_plot),
                                        colors = plotrix::smoothColors(
                                          colours[1], 100, colours[2]),
                                        na.value = colours[3]) +
          ggplot2::coord_map(projection = user_projection) +
          ggplot2::geom_sf(data = shapefile,
                             colour = colours[4], fill = NA) +
          ggplot2::labs(title = title) +
          ggplot2::scale_x_continuous(
            guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::theme_bw()+
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                         panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         axis.title.x = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank(),
                         axis.title.y = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_blank(),
                         axis.ticks.y = ggplot2::element_blank())
      }

      if (lightdata_time == "monthly"){
        plot_name <- paste0(area_name, "_", year, "-", month, "_plot")
      } else if (lightdata_time == "yearly"){
        plot_name <- paste0(area_name, "_", year, "_plot")
      }
      assign(plot_name, plot_lights, envir = .GlobalEnv)
      print(plot_lights)
      # will output the plot as a ggplot object into the global environment
      # name of the object is area + time

    } # end sequence loop

    rm(area_name)
    rm(ISO3)
    shapefile <- NULL

  } # end area loop

  if (lightdata_time == "yearly" &
      harmonized_lights == FALSE){
    if (dmsp_consistent == TRUE | length(sequence) == 1){
      print(paste0("The consistent DMSP version selected for your timespan is ",
                   dmsp_stump, "."))
    } else if (dmsp_consistent == FALSE & length(sequence) > 1){
      print(paste0("It was not possible to select a consistent DMSP version ",
                   "for your timespan. For each year, the newest DMSP version ",
                   "was chosen."))
    }
  }

} # end function
