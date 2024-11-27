#######################################################################
# Ecosystem service in Ca Mau province, Vietnam
# 
# First: 26th. Nov. 2024
# Revised: 
#
# by Vo Quac Tuan and Yuzuru Utsunomiya
#######################################################################
#
# ----- load.library -----
library(tidyverse)
library(janitor)
library(sf)
library(furrr)
plan(multisession, workers = 16)
#
# ----- read.data -----
hhsurvey_2022 <- 
  readxl::read_excel(
    "152_Household_survey_29_08_2022.xlsx",
    sheet = "Sheet1",
    col_names = TRUE
    ) %>%
  # Use the first row as columns' names
  janitor::row_to_names(row_number = 1) %>% 
  janitor::clean_names() %>% 
  # Transform from UTM to WGS48
  # Thw WGS48 can be treated easier.
  # 1. Define the existing columns as UTM
  sf::st_as_sf(
    .,
    coords = c("x", "y"),
    crs = "+proj=utm +zone=48"
  ) %>% 
  # 2. Transform the UTM into WGS48
  sf::st_transform(4326) %>% 
  # 3. Add columns indicating the lon\lat coordinates
  dplyr::mutate(
    lon = sf::st_coordinates(.)[,1],
    lat = sf::st_coordinates(.)[,2]
    )
# shapefiles
shp_03_camau <- 
  sf::read_sf("./gadm41_VNM_shp/gadm41_VNM_3.shp") %>% 
  dplyr::filter(NAME_1 == "Cà Mau")
# add address in English
# In detail, refer to another file named "find_city.r"
# load function to find them
source("find_city.r")
# find to record
hhsurvey_2022_address <-
  hhsurvey_2022 %>%
  dplyr::mutate(
    area_info = furrr::future_map2_dfr(
      .x = lon,
      .y = lat,
      ~ try(
        find_city(
          sp_polygon = shp_03_camau,
          lon = .x,
          lat = .y
          )
        )
      )
    ) %>%
  tibble() %>% 
  dplyr::mutate(
    province_name = area_info[4]$province_name,
    district_name = area_info[5]$district_name,
    village_name = area_info[6]$village_name %>% factor(),
    # replace Vietnamese names with English names
    # The 2-byte characters often result in malfunctions and they should be
    # replaced.
    district_name = dplyr::case_when(
      district_name == "Đầm Dơi" ~ "Dam Doi",
      district_name == "Năm Căn" ~ "Nam Can",
      district_name == "Ngọc Hiển" ~ "Ngoc Hien",
      district_name == "Phú Tân" ~ "Phu Tan",
      TRUE ~ "hoge"
    ) %>% factor(),
    province_name = dplyr::case_when(
      province_name == "Cà Mau" ~ "Ca Mau",
      TRUE ~ "hoge"
    ) %>% factor()
  ) 


