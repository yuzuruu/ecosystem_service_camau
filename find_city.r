#######################################################################
# function to find provinces, districts, and subdistricts in certain countries
# using shapefiles
#######################################################################

find_city <- 
  function(sp_polygon = df, lon = lon, lat = lat){
    # find a polygon containing a certain pair of lon / lat
    which.row <- 
      sf::st_contains(
        sp_polygon, 
        sf::st_point(
          c(
            lon, 
            lat
          )
        ), 
        sparse = FALSE
      ) %>%  
      grep(TRUE, .)
    # If not, leave a warning message
    # -> deliver values ("hoge") to avoid malfunction.
    # We will remove NA by the values later. 
    if (identical(which.row, integer(0)) == TRUE) {
      # Original code provides the following message.
      # message("指定した座標がポリゴンに含まれません")
      geos <- 
        data.frame(
          ID_1 = "hoge", 
          NAME_1 = "hoge",
          ID_2 = "hoge", 
          NAME_2 = "hoge",
          ID_3 = "hoge", 
          # To obtain English names, 
          # here should be revised in accordance with shapefiles' format.
          # When the shapefiles include the English names for a certain variable
          # (province, district, ...etc.), we need to provide the variable. 
          # Usually, NAME_** includes in original language and VARNAME_** does in
          # English
          VARNAME_3 = "hoge"
        )
    }
    # If exist, obtain information of coordinates
    else {
      geos <-
        sp_polygon[which.row, ] %>%
        # transform from factor to character
        # dplyr::mutate_if(
        #   is.factor, 
        #   as.character
        # We revised here in accordance with change in specification.
        # old: We used to use mutate_if()
        # new: We use dplyr::across() and where()
        # 
        dplyr::mutate(
          dplyr::across(
            where(~ is.factor(.)),
            as.character
            )
          ) # %>% 
      # obtain necessary part of the shapefile
      # We omit NA check part.
        # dplyr::mutate(
        #   across(everything(),
        #   ~
        #     dplyr::if_else(
        #       # Is it NA?
        #       condition = is.na(.),
        #       # if NA, return blank
        #       true = "",
        #       # if not, use it
        #       false = .
        #     )
        #   )
        #   )
      # make a dataset of administrative boundaries
      # Names and IDs are obtained from shapefiles
      res <- 
        tibble::data_frame(
          province_code = geos$GID_1,
          district_code = geos$GID_2,
          village_code = geos$GID_3,
          province_name = geos$NAME_1,
          district_name = geos$NAME_2,
          village_name = geos$VARNAME_3
        )
      # for inspecting function movement
      # print(res)
      # return results
      return(res)
    }
  }
