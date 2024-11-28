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
library(gtsummary)
plan(multisession, workers = 16)
#
# ----- read.data -----
# load function to find them
source("find_city.r")
# shapefiles
shp_03_camau <- 
  sf::read_sf("./gadm41_VNM_shp/gadm41_VNM_3.shp") %>% 
  dplyr::filter(NAME_1 == "Cà Mau")
# survey results
hhsurvey_2022 <- 
  readxl::read_excel(
    "152_Household_survey_29_08_2022.xlsx",
    sheet = "copy",
    col_names = TRUE
    ) %>%
  # Use the first row as columns' names
  janitor::row_to_names(row_number = 1) %>% 
  # convert variables' name into clearer formats
  janitor::clean_names() %>% 
  # remove the 1st. row
  dplyr::slice(-1) %>%
  # remove columns containing "na" in their names
  dplyr::select(-tidyselect::starts_with("na")) %>%
  dplyr::select(-tidyselect::starts_with("percent")) %>%
  # remove blank columns
  # select columns that n of NA observation is not equivalent to the n of row
  dplyr::select_if(
    colSums(is.na(.)) != nrow(.)
    ) %>% 
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
    # convert ymd in MSExcel serial number into visible expresion, refer to the following page (stack overflow). 
    # https://stackoverflow.com/questions/19172632/converting-excel-datetime-serial-number-to-r-datetime
    date = janitor::excel_numeric_to_date(as.numeric(as.character(date), date_system = "modern")) %>% lubridate::ymd(),
    lon = sf::st_coordinates(.)[,1],
    lat = sf::st_coordinates(.)[,2],
    area_info = furrr::future_map2_dfr(
      .options = furrr_options(seed = 123),
      .x = lon,
      .y = lat,
      ~ try(
        find_city(
          sp_polygon = shp_03_camau,
          lon = .x,
          lat = .y
          )
        )
      ),
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
    ) %>% factor(),
    sex = dplyr::case_when(
      sex == "1" ~ "male",
      sex == "2" ~ "female",
      TRUE ~ "hoge"
    ),
    edu_le = dplyr::case_when(
      edu_le == "1" ~ "illiteracy",
      edu_le == "2" ~ "Primary_school",
      edu_le == "3" ~ "Secondary_school",
      edu_le == "4" ~ "High_school",
      edu_le == "5" ~ "College_or_university",
      TRUE ~ "hoge"
    ) %>% factor(., levels = c("illiteracy","Primary_school","Secondary_school","High_school","College_or_university"))
    ) %>% 
  tibble::tibble()
# 
# ------ table.one -----
# data
hhsurvey_2022_demography <- 
  hhsurvey_2022 %>% 
  dplyr::select(age:income, district_name, -majorjob, -village_name, -where, -from_where, -when, -source, -village, -disct) %>% 
  dplyr::mutate(
    across(c(age, exp, how_mem, total_area, mangr_area, aqua_area, veget_area, residential_area, income), as.numeric)
  ) %>% 
  dplyr::mutate(
    across(where(is.character), factor)
  ) 
# table one
hhsurvey_2022_demography_summary <- 
  hhsurvey_2022_demography %>% 
  gtsummary::tbl_summary(
    by = district_name,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      age = "Age",
      sex = "Gender",
      edu_le = "Education attainment",
      exp = "Experience of mangrove management (Unit: year)",
      how_mem = "N. of family member (unit: pax)",
      total_area = "Total area (unit: ha)",
      mangr_area = "Area of mangrove (unit: ha)",
      aqua_area = "Area for aquaculture (unit: ha)",
      veget_area = "Area for vegetation (unit: ha)",
      residential_area = "Residential area (unit: ha)",
      income = "Monthly income (unit: VND) "
      
    )
  ) %>% 
  as_flex_table()
