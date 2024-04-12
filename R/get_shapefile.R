get_shapefile <- function(i,
                          area_name,
                          ISO3,
                          help_shapefiles,
                          shapefiles,
                          admlevel,
                          gpkg_layer,
                          user_coordinates,
                          shapefile_location,
                          download_shape,
                          lightdata_time){

  help_shapefile <- help_shapefiles[i]
  shapefile <- shapefiles[i]
  # these are either provided by the user and hence activated at this point
  # or shapefile will be NULL and help_shapefile will be NA. then, shapefile
  # will be found in directory or downloaded from GADM below

  # if shapefile available at this point, its type is detected and it is
  # read with the according function

  if (!is.na(help_shapefile)){
    if (length(grep(help_shapefile, pattern = "gpkg")) != 0){
      if (!is.null(gpkg_layer)){
        shapefile <- sf::st_read(shapefile, gpkg_layer)
      } else if (is.null(gpkg_layer)){
        layers_all <- sf::st_layers(shapefile)[["name"]]
        layer <- layers_all[grep(as.character(admlevel), layers_all)]
        if (length(layer) > 1){ # if layer is not uniquely found by admlevel
          # alone e.g. when adm level 3 is chosen and all layers are
          # called "gadm3_6..."
          adm_matches <- stringr::str_count(layers_all, as.character(admlevel))
          layer_index <- match(max(adm_matches), adm_matches)
          layer <- layer[layer_index]
        }
        # this will find the layer in which the admlevel number most
        # often appears. the reason to do it this way is because
        # (even within GADM), layers are sometimes in ascending, sometimes
        # in descending order. so a general approach e.g. for ascending
        # order "admlevel + 1" or descending order
        # "length(layers) - admlevel" does not work. plus, this is more
        # robust to other types of shapefiles than GADM as well.
        if (!is.na(layer)){
          shapefile <- sf::st_read(shapefile, layer)
        } else if (is.na(layer)){
          stop(paste0("Please enter the layer of your .gpkg shapefile for ",
                      area_name, " by hand using the gpkg_layer argument."))
        }
      }
    } else if (length(grep(help_shapefile, pattern = "shp")) != 0){
      shapefile <- sf::st_read(shapefile)
    } else {
      stop(paste0("Unfortunately, the function does not work the format of ",
                  "your shapefile. If no other option is available to you, ",
                  "you can try using the min/max x- and y-coordinates of your ",
                  "shapefile in the function input instead."))
    }
  }

  if(!is.null(user_coordinates)){
    # create a rectangular shapefile if user provides coordinates
    extent <- raster::extent(user_coordinates)
    extent_bbox <- sp::bbox(extent)
    shapefile <- methods::as(extent, "SpatialPolygons")
    raster::crs(shapefile) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
    shapefile <- sp::SpatialPolygonsDataFrame(shapefile,
                                              data.frame(N = c("1"),
                                                         row.names = c("1")))
    shapefile <- sf::st_as_sf(shapefile)
  }

  # if no shapefile available at this point:
  # check if shapefile is already downloaded
  # check for every format. will preferably find
  # .gpkg, then .shp

  # first: check for GADM shapefiles which are identified by ISO3 and admlevel

  if (!is.na(ISO3)){ # need this if condition because otherwise some files are
    # detected by ISO3 = NA. for some reason only .gkpg files

    # note about reasoning behind the if conditions for help_shapefile:
    # if length() == 1, then 1 matching file was identified which is fine
    # if length() > 1, then some files have similar names. in this case,
    # the user gets an error to enter the filename by hand because it seems to
    # be there but not uniquely identifiable.
    # if length() == 0, there is no identifiable file. so just go on with the
    # loop to find e.g. the file in another format, no seperate if condition
    # needed for this case

    # .gpkg
    if (is.null(shapefile)){
      help_shapefile <- list.files(shapefile_location, pattern = "gpkg")
      help_shapefile <- help_shapefile[grep(help_shapefile, pattern = "zip",
                                            invert = TRUE)]
      help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
      if (length(help_shapefile) == 1){
        # here there is no need to check for gpkg_layer argument since gadm
        # shapefiles will always be able to identify the layer according to the
        # adm level
        layers_all <- sf::st_layers(paste0(shapefile_location, "/",
                                                  help_shapefile))[["name"]]
        layer <- layers_all[grep(as.character(admlevel), layers_all)]
        if (length(layer) > 1){ # if layer is not uniquely found by admlevel
          # alone e.g. when adm level 3 is chosen and all layers are
          # called "gadm3_6..."
          adm_matches <- stringr::str_count(layers_all, as.character(admlevel))
          layer_index <- match(max(adm_matches), adm_matches)
          layer <- layer[layer_index]
        }
        # this will find the layer in which the admlevel number most
        # often appears. the reason to do it this way is because
        # (even within GADM), layers are sometimes in ascending, sometimes
        # in descending order. so a general approach e.g. for ascending
        # order "admlevel + 1" or descending order
        # "length(layers) - admlevel" does not work. plus, this is more
        # robust to other types of shapefiles than GADM as well.
        if (!is.na(layer)){
          shapefile <- sf::st_read(paste0(shapefile_location, "/",
                                             help_shapefile), layer)
        } else if (is.na(layer)){
          stop(paste0("Please enter the layer of your .gpkg shapefile for ",
                      area_name, " by hand using the gpkg_layer argument."))
        }
      } else if (length(help_shapefile) > 1){
        stop(paste0("Please enter the filename of your .gpkg shapefile for ",
                    area_name, " by hand using the shapefiles argument. It ",
                    "could not be loaded automatically.") )
      }
    }
    # .gpkg but still zipped as .zip
    if (is.null(shapefile)){
      help_shapefile <- list.files(shapefile_location, pattern = "gpkg.zip")
      help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
      if (length(help_shapefile) == 1){
        help_shapefile <- utils::unzip(zipfile = paste0(shapefile_location,
                                                        "/", help_shapefile),
                                       exdir = shapefile_location)
        help_shapefile <- help_shapefile[grep(help_shapefile,
                                              pattern = "gpkg")]
        layers_all <- sf::st_layers(help_shapefile)[["name"]]
        layer <- layers_all[grep(as.character(admlevel), layers_all)]
        if (length(layer) > 1){ # if layer is not uniquely found by admlevel
          # alone e.g. when adm level 3 is chosen and all layers are
          # called "gadm3_6..."
          adm_matches <- stringr::str_count(layers_all, as.character(admlevel))
          layer_index <- match(max(adm_matches), adm_matches)
          layer <- layer[layer_index]
        }
        # this will find the layer in which the admlevel number most
        # often appears. the reason to do it this way is because
        # (even within GADM), layers are sometimes in ascending, sometimes
        # in descending order. so a general approach e.g. for ascending
        # order "admlevel + 1" or descending order
        # "length(layers) - admlevel" does not work. plus, this is more
        # robust to other types of shapefiles than GADM as well.
        if (!is.na(layer)){
          shapefile <- sf::st_read(help_shapefile, layer)
        } else if (is.na(layer)){
          stop(paste0("Please enter the layer of your .gpkg shapefile for ",
                      area_name, " by hand using the gpkg_layer argument."))
        }
        zipfile <- list.files(shapefile_location, pattern = "gpkg.zip")
        zipfile <- zipfile[grep(zipfile, pattern = ISO3)]
        unlink(paste0(shapefile_location, "/", zipfile), recursive = TRUE)
      } else if (length(help_shapefile) > 1){
        stop(paste0("Please enter the filename of your .gpkg shapefile for ",
                    area_name, " by hand using the shapefiles argument. It ",
                    "could not be loaded automatically.") )
      }
    }
    # .shp
    if (is.null(shapefile)){
      help_shapefile <- list.files(shapefile_location, pattern = "shp")
      help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
      help_shapefile <- help_shapefile[grep(help_shapefile, pattern = admlevel)]
      if (length(help_shapefile) == 1){
        shapefile <- sf::st_read(paste0(shapefile_location,
                                           "/", help_shapefile))
      } else if (length(help_shapefile) > 1){
        stop(paste0("Please enter the filename of your .shp shapefile for ",
                    area_name,
                    " by hand using the shapefiles argument. It could not ",
                    "be loaded automatically.") )
      }
    }
    # .shp but still zipped as .zip
    if (is.null(shapefile)){
      help_shapefile <- list.files(shapefile_location, pattern = "shp.zip")
      help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
      if (length(help_shapefile) == 1){
        help_shapefile <- utils::unzip(zipfile = paste0(shapefile_location,
                                                        "/", help_shapefile),
                                       exdir = shapefile_location)
        help_shapefile <- help_shapefile[grep(help_shapefile,
                                              pattern = "shp")]
        help_shapefile <- help_shapefile[grep(help_shapefile,
                                              pattern = admlevel)]
        shapefile <- sf::st_read(help_shapefile)
        zipfile <- list.files(shapefile_location, pattern = "shp.zip")
        zipfile <- zipfile[grep(zipfile, pattern = ISO3)]
        unlink(paste0(shapefile_location, "/", zipfile), recursive = TRUE)
      } else if (length(help_shapefile) > 1){
        stop(paste0("Please enter the filename of your .shp shapefile for ",
                    area_name,
                    " by hand using the shapefiles argument. It could not be ",
                    "loaded automatically.") )
      }
    }
    # .kml
    #if (is.null(shapefile)){
    #  help_shapefile <- list.files(shapefile_location, pattern = "kml")
    #  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
    #  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = admlevel)]
    #  if (length(help_shapefile) == 1){
    #    shapefile <- sf::st_read(
    #      paste0(shapefile_location, "/", help_shapefile))
    #  } else if (length(help_shapefile) > 1){
    #    stop(paste0("Please enter the filename of your .kml shapefile for ",
    #                area_name,
    #                " by hand using the shapefiles argument. It could not be ",
    #                "loaded automatically.") )
    #  }
    #}
    # .kml but still zipped as .kmz
    #if (is.null(shapefile)){
    #  help_shapefile <- list.files(shapefile_location, pattern = "kmz")
    #  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ISO3)]
    #  if (length(help_shapefile) == 1){
    #    help_shapefile <- utils::unzip(zipfile = paste0(shapefile_location, "/",
    #                                                    help_shapefile),
    #                                   exdir = shapefile_location)
    #    shapefile <- sf::st_read(help_shapefile)
    #    zipfile <- list.files(shapefile_location, pattern = "kmz")
    #    zipfile <- zipfile[grep(zipfile, pattern = ISO3)]
    #    unlink(paste0(shapefile_location, "/", zipfile), recursive = TRUE)
    #  } else if (length(help_shapefile) > 1){
    #    stop(paste0("Please enter the filename of your .kml shapefile for ",
    #                area_name,
    #                " by hand using the shapefiles argument. It could not be ",
    #                "loaded automatically.") )
    #  }
    #}

  }

  # if not found so far, check for shapefiles that are identified by the
  # name entered by the user in area_names

  # .gpkg
  if (is.null(shapefile)){
    help_shapefile <- list.files(shapefile_location, pattern = "gpkg")
    help_shapefile <- help_shapefile[grep(help_shapefile, pattern = "zip",
                                          invert = TRUE)]
    help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
    if (length(help_shapefile) == 1){
      if (!is.null(gpkg_layer)){
        shapefile <- sf::st_read(paste0(shapefile_location, "/",
                                           help_shapefile), gpkg_layer)
      } else if (is.null(gpkg_layer)){
        layers_all <- sf::st_layers(paste0(
          shapefile_location, "/", help_shapefile))[["name"]]
        layer <- layers_all[grep(as.character(admlevel), layers_all)]
        if (length(layer) > 1){ # if layer is not uniquely found by admlevel
          # alone e.g. when adm level 3 is chosen and all layers are
          # called "gadm3_6..."
          adm_matches <- stringr::str_count(layers_all, as.character(admlevel))
          if (max(adm_matches) > 0){
            layer_index <- match(max(adm_matches), adm_matches)
            layer <- layer[layer_index]
            # this will find the layer in which the admlevel number most
            # often appears. the reason to do it this way is because
            # (even within GADM), layers are sometimes in ascending, sometimes
            # in descending order. so a general approach e.g. for ascending
            # order "admlevel + 1" or descending order
            # "length(layers) - admlevel" does not work. plus, this is more
            # robust to other types of shapefiles than GADM as well.
            if (!is.na(layer)){
              shapefile <- sf::st_read(paste0(shapefile_location, "/",
                                                 help_shapefile), layer)
            } else if (is.na(layer)){
              stop(paste0("Please enter the layer of your .gpkg shapefile for ",
                          area_name, " by hand using the gpkg_layer argument."))
            }
          } else {
            stop(paste0("Please enter the layer of your .gpkg shapefile for ",
                        area_name, " by hand using the gpkg_layer argument."))
          }
        } else if (length(layer) == 1){
          # if admlevel alone matches a layer unambiguously, read directly
          shapefile <- sf::st_read(
            paste0(shapefile_location, "/", help_shapefile), layer)
        } else if (length(layer) == 0){ # if nothing is matched, this means
          # the layers are some strings e.g. landscape properties. user has to
          # enter the strings in this case, no clear way to automatically
          # detect that
          stop(paste0("Please enter the layer of your .gpkg shapefile for ",
                      area_name, " by hand using the gpkg_layer argument."))
        }
      }
    } else if (length(help_shapefile) > 1){
      stop(paste0("Please enter the filename of your .gpkg shapefile for ",
                  area_name, " by hand using the shapefiles argument and the ",
                  "layer using the gpkg_layer argument. It could not be loaded ",
                  "automatically."))
    }
  }
  # .gpkg but still zipped as .zip
  if (is.null(shapefile)){
    help_shapefile <- list.files(shapefile_location, pattern = "gpkg.zip")
    help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
    if (length(help_shapefile) == 1){
      help_shapefile <- utils::unzip(zipfile = paste0(
        shapefile_location, "/", help_shapefile), exdir = shapefile_location)
      help_shapefile <- help_shapefile[grep(help_shapefile, pattern = "gpkg")]
      if (!is.null(gpkg_layer)){
        shapefile <- sf::st_read(help_shapefile, gpkg_layer)
      } else if (is.null(gpkg_layer)){
        layers_all <- sf::st_layers(help_shapefile)[["name"]]
        layer <- layers_all[grep(as.character(admlevel), layers_all)]
        if (length(layer) > 1){ # if layer is not uniquely found by admlevel
          # alone e.g. when adm level 3 is chosen and all layers are
          # called "gadm3_6..."
          adm_matches <- stringr::str_count(layers_all, as.character(admlevel))
          if (max(adm_matches) > 0){
            layer_index <- match(max(adm_matches), adm_matches)
            layer <- layer[layer_index]
            # this will find the layer in which the admlevel number most
            # often appears. the reason to do it this way is because
            # (even within GADM), layers are sometimes in ascending, sometimes
            # in descending order. so a general approach e.g. for ascending
            # order "admlevel + 1" or descending order
            # "length(layers) - admlevel" does not work. plus, this is more
            # robust to other types of shapefiles than GADM as well.
            if (!is.na(layer)){
              shapefile <- sf::st_read(help_shapefile, layer)
            } else if (is.na(layer)){
              stop(paste0("Please enter the layer of your .gpkg shapefile for ",
                          area_name, " by hand using the gpkg_layer argument."))
            }
          } else {
            stop(paste0("Please enter the layer of your .gpkg shapefile for ",
                        area_name, " by hand using the gpkg_layer argument."))
          }
        } else if (length(layer) == 1){
          # if admlevel alone matches a layer unambiguously
          shapefile <- sf::st_read(help_shapefile, layer)
        } else if (length(layer) == 0){ # if nothing is matched, this means
          # the layers are some strings e.g. landscape properties. user has to
          # enter the strings in this case, no clear way to automatically
          # detect that
          stop(paste0("Please enter the layer of your .gpkg shapefile for ",
                      area_name, " by hand using the gpkg_layer argument."))
        }
      }
      zipfile <- list.files(shapefile_location, pattern = "gpkg.zip")
      zipfile <- zipfile[grep(zipfile, pattern = area_name)]
      unlink(paste0(shapefile_location, "/", zipfile), recursive = TRUE)
    } else if (length(help_shapefile) > 1){
      stop(paste0("Please enter the filename of your .gpkg shapefile for ",
                  area_name, " by hand using the shapefiles argument and the ",
                  "layer using the gpkg_layer argument. It could not be ",
                  "loaded automatically.") )
    }
  }
  # .shp
  if (is.null(shapefile)){
    help_shapefile <- list.files(shapefile_location, pattern = "shp")
    help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
    if (length(help_shapefile) == 1){
      shapefile <- sf::st_read(paste0(shapefile_location,
                                         "/", help_shapefile))
    } else if (length(help_shapefile) > 1){
      stop(paste0("Please enter the filename of your .shp shapefile for ",
                  area_name, " by hand using the shapefiles argument. It ",
                  "could not be loaded automatically.") )
    }
  }
  # .shp but still zipped as .zip
  if (is.null(shapefile)){
    help_shapefile <- list.files(shapefile_location, pattern = "shp.zip")
    help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
    if (length(help_shapefile) == 1){
      help_shapefile <- utils::unzip(zipfile = paste0(shapefile_location, "/",
                                                      help_shapefile),
                                     exdir = shapefile_location)
      help_shapefile <- help_shapefile[grep(help_shapefile, pattern = "shp")]
      if (length(help_shapefile) == 1){
        shapefile <- sf::st_read(help_shapefile)
        zipfile <- list.files(shapefile_location, pattern = "shp.zip")
        zipfile <- zipfile[grep(zipfile, pattern = area_name)]
        unlink(paste0(shapefile_location, "/", zipfile), recursive = TRUE)
      } else if (length(help_shapefile) > 1){
        stop(paste0("Please enter the filename of your .gpkg shapefile for ",
                    area_name, " by hand using the shapefiles argument. It ",
                    "could not be loaded automatically.") )
      }
    } else if (length(help_shapefile) > 1){
      stop(paste0("Please enter the filename of your .gpkg shapefile for ",
                  area_name, " by hand using the shapefiles argument. It ",
                  "could not be loaded automatically.") )
    } # the outer error message refers to non-unique shapefile zip names,
    # the inner one to non-unique shapefile names when unpacked (for gadm
    # shapefiles above, this is unique because they always have the admlevel
    # in the name as well. but if the code searches for adm level here,
    # shapefiles with no admlevel e.g. cities are excluded)
  }
  # .kml
  #if (is.null(shapefile)){
  #  help_shapefile <- list.files(shapefile_location, pattern = "kml")
  #  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
  #  if (length(help_shapefile) == 1){
  #    shapefile <- sf::st_read(paste0(
  #      shapefile_location, "/", help_shapefile))
  #  } else if (length(help_shapefile) > 1){
  #    stop(paste0("Please enter the filename of your .kml shapefile for ",
  #                area_name, " by hand using the shapefiles argument. It ",
  #                "could not be loaded automatically.") )
  #  }
  #}
  # .kml but still zipped as .kmz
  #if (is.null(shapefile)){
  #  help_shapefile <- list.files(shapefile_location, pattern = "kmz")
  #  help_shapefile <- help_shapefile[grep(help_shapefile, pattern = area_name)]
  #  if (length(help_shapefile) == 1){
  #    help_shapefile <- utils::unzip(zipfile = paste0(
  #      shapefile_location, "/", help_shapefile), exdir = shapefile_location)
  #    shapefile <- sf::st_read(paste0(shapefile_location, "/",
  #                                       help_shapefile))
  #    zipfile <- list.files(shapefile_location, pattern = "kmz")
  #    zipfile <- zipfile[grep(zipfile, pattern = area_name)]
  #    unlink(paste0(shapefile_location, "/", zipfile), recursive = TRUE)
  #  } else if (length(help_shapefile) > 1){
  #    stop(paste0("Please enter the filename of your .kml shapefile for ",
  #                area_name, " by hand using the shapefiles argument. It ",
  #                "could not be loaded automatically.") )
  #  } # for kmz there is only one shapefile per zipfile so
  #  # only 1 error message is necessary
  #}

  # if shapefile is not downloaded yet: download from GADM
  stumpurl1 <- "https://geodata.ucdavis.edu/gadm/gadm4.1/"
  stumpurl2 <- "/gadm41_"
  
  # .gpkg
  if (is.null(shapefile) & download_shape == ".gpkg"){
    utils::download.file(paste0(stumpurl1, "gpkg",
                                stumpurl2 , ISO3, ".gpkg"),
                         destfile = paste0(shapefile_location,
                                           "/gadm41_", ISO3, ".gpkg"),
                         mode = "wb")
    help_shapefile <- paste0(shapefile_location,
                             "/gadm41_", ISO3, ".gpkg")
    help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ".gpkg")]
    layers_all <- sf::st_layers(help_shapefile)[["name"]]
    layer <- layers_all[grep(as.character(admlevel), layers_all)]
    if (length(layer) > 1){ # if layer is not uniquely found by admlevel alone
      #  e.g. when adm level 3 is chosen and all layers are called "gadm4_1..."
      adm_matches <- stringr::str_count(layers_all, as.character(admlevel))
      layer_index <- match(max(adm_matches), adm_matches)
      layer <- layer[layer_index]
    }
    # this will find the layer in which the admlevel number most
    # often appears. the reason to do it this way is because
    # (even within GADM), layers are sometimes in ascending, sometimes
    # in descending order. so a general approach e.g. for ascending
    # order "admlevel + 1" or descending order
    # "length(layers) - admlevel" does not work. plus, this is more
    # robust to other types of shapefiles than GADM as well.
    shapefile <- sf::st_read(help_shapefile, layer)
  }
  # .shp
  if (is.null(shapefile) & download_shape == ".shp"){
    utils::download.file(paste0(stumpurl1, "shp", stumpurl2, ISO3, "_shp.zip"),
                         destfile = paste0(shapefile_location,
                                           "/gadm41_", ISO3, "_shp.zip"),
                         mode = "wb")
    help_shapefile <- paste0(shapefile_location, "/gadm41_",
                             ISO3, "_shp.zip")
    help_shapefile <- utils::unzip(zipfile = help_shapefile,
                                   exdir = shapefile_location)
    help_shapefile <- help_shapefile[grep(help_shapefile, pattern = ".shp")]
    help_shapefile <- help_shapefile[grep(help_shapefile, pattern = paste0("_", admlevel))]
    shapefile <- sf::st_read(help_shapefile)
    unlink(paste0(shapefile_location, "/gadm41_", ISO3, "_shp.zip"),
           recursive = TRUE)
  }
  # .kml
  #if (is.null(shapefile) & download_shape == ".kml"){
  #  utils::download.file(paste0(stumpurl1, "kmz", stumpurl2,
  #                              ISO3, "_", admlevel, ".kmz"),
  #                       destfile = paste0(shapefile_location,
  #                                         "/gadm41_", ISO3, "_",
  #                                         admlevel, ".kmz"), mode = "wb")
  #  help_shapefile <- paste0(shapefile_location, "/gadm41_",
  #                           ISO3, "_", admlevel, ".kmz")
  #  help_shapefile <- utils::unzip(zipfile = help_shapefile,
  #                                 exdir = shapefile_location)
  #  shapefile <- sf::st_read(help_shapefile)
  #  unlink(paste0(shapefile_location, "/gadm41_",
  #                ISO3, "_", admlevel, ".kmz"),
  #         recursive = TRUE)
  #}

  # the following is only for the case that coordinates of the shapefile
  # are not in longlat format - in that case transform it into longlat
  shapefileprojection <- suppressWarnings(raster::crs(shapefile))
  shapefileprojection <- as.character(shapefileprojection)
  if (length(shapefileprojection[grep(shapefileprojection,
                                      pattern = "longlat")]) == 0){
    shapefile <- suppressWarnings(sf::st_transform(shapefile,
                                                   crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  }

  if (is.null(user_coordinates)){
    extent <- raster::extent(shapefile)
    extent_bbox <- sp::bbox(extent)
  }

  xmin = extent_bbox[1,1]
  xmax = extent_bbox[1,2]
  ymin = extent_bbox[2,1]
  ymax = extent_bbox[2,2]

  if (lightdata_time == "monthly"){
    # search for tiles on which the shapefile is located
    tilenumbers <- c()
    if (ymax > 0 & c(xmin < -60 | xmax < -60)){
      tilenumbers <- append(tilenumbers, "1")
    }
    if (ymax > 0 & c(c(xmin > -60 & xmin < 60) |
                     c(xmax > -60 & xmax < 60))){
      tilenumbers <- append(tilenumbers, "2")
    }
    if (ymax > 0 & c(xmin > 60 | xmax > 60)){
      tilenumbers <- append(tilenumbers, "3")
    }
    if (ymin < 0 & c(xmin < -60 | xmax < -60)){
      tilenumbers <- append(tilenumbers, "4")
    }
    if (ymin < 0 & c(c(xmin > -60 & xmin < 60) |
                     c(xmax > -60 & xmax < 60))){
      tilenumbers <- append(tilenumbers, "5")
    }
    if (ymin < 0 & c(xmin > 60 | xmax > 60)){
      tilenumbers <- append(tilenumbers, "6")
    }
  }

  # assign the variables that are needed outside of this helper
  # function to the parent frame so they are available one level higher

  assign("shapefile", shapefile, envir = parent.frame())
  assign("extent", extent, envir = parent.frame())
  assign("extent_bbox", extent_bbox, envir = parent.frame())
  if (lightdata_time == "monthly"){
    assign("tilenumbers", tilenumbers, envir = parent.frame())
  }

}
