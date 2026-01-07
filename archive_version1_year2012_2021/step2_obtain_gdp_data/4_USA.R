# --------------------------------- Task Summary --------------------------------- #
# This file retrieves GDP data for the USA at the county, state, and national 
#   levels, and processes the associated geometries.
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(units)
library(gdata)
library(tigris)
library(sf)
library(jsonlite)
library(qgisprocess)

# Obtain GDP data in current dollars: year 2001 - 2021

county_gdp <- read.csv("step2_obtain_gdp_data/inputs/gdp_data/regional/USA/CAGDP2/CAGDP2__ALL_AREAS_2001_2021.csv")  %>% 
  filter(LineCode %in% c(1))  %>% # 1 stands for All industry
  dplyr::select(-c(Region, TableName, IndustryClassification, Description, Unit))  %>% 
  pivot_longer(cols = starts_with("X"),
                names_to = "year",
                names_prefix = "X",
                values_to = "value")  %>% 
  rename(variable = LineCode, name = GeoName, fips = GeoFIPS) %>%
  mutate(variable = ifelse(variable == 1, "total"))  %>% 
  filter(str_detect(name, ","))  %>% # I only want county level data
  mutate(fips = str_sub(fips, 2,))  %>% 
  mutate(name = gsub("Do\xf1a Ana, NM", "Doña Ana, NM", name)) %>%                
  mutate(name = gsub("\\*$", "", name)) %>% 
  mutate(name = str_sub(name, 1, -5))  %>% 
  pivot_wider(names_from = "variable", names_prefix = "rgdp_") %>% 
  arrange(fips, year)  %>% 
  mutate(rgdp_total = ifelse(rgdp_total %in% c("(D)", "(NA)"), NA, rgdp_total))  %>% 
  mutate(state_fips = str_sub(fips, 1, 2),
         rgdp_total = as.numeric(rgdp_total))  %>% 
  mutate(across(.cols = matches("rgdp_"),
                .fns = ~ .x/1000))  %>% # in millions of $
  filter(substr(fips,1,2) != "02") # let's do not include Alaska's county level data, because the geom keeps changing; consider it as a seaprate big country

state_ctry_gdp <- read.csv("step2_obtain_gdp_data/inputs/gdp_data/regional/USA/CAGDP2/CAGDP2__ALL_AREAS_2001_2021.csv")  %>% 
  filter(LineCode %in% c(1))  %>% # 1 stands for All industry
  dplyr::select(-c(Region, TableName, IndustryClassification, Description, Unit))  %>% 
  pivot_longer(cols = starts_with("X"),
                names_to = "year",
                names_prefix = "X",
                values_to = "value")  %>% 
  rename(variable = LineCode, name = GeoName, state_fips = GeoFIPS) %>%
  mutate(variable = ifelse(variable == 1, "total"))  %>% 
  filter(!str_detect(name, ","))  %>% # only need state level data: remove county level data here
  filter(!name %in% c("Mideast", "Great Lakes","New England",
                      "Plains", "Southeast", "Southwest", "Rocky Mountain", "Far West")) %>% # only need state level data: remove country and region level data here
  mutate(state_fips = str_sub(state_fips, 2,))  %>%
  pivot_wider(names_from = "variable", names_prefix = "admin_2_rgdp_") %>% 
  arrange(state_fips, year) %>% 
  mutate(state_fips = str_sub(state_fips, 1, 2),
         across(.cols = c("admin_2_rgdp_total"),
                .fns = ~ as.numeric(gsub(",", "", .x))/1000)) %>% # in millions of $
  mutate(name = ifelse(name == "United States *", "United States", name))

country_gdp <- filter(state_ctry_gdp, name == "United States") %>%
  rename_with(matches("admin_2"), .fn = ~ gsub("2", "1", .x)) %>% 
  dplyr::select(-state_fips) %>% 
  rename(admin_1_name = name) %>% 
  mutate(iso = "USA")

state_gdp <- filter(state_ctry_gdp, name != "United States") %>% 
  rename(admin_2_name = name) %>% 
  mutate(iso = "USA") %>% 
  left_join(country_gdp)

# ------------------------------------------------- #
# Create shapefiles -----

county_sf_2020 <- counties(cb = T, year = 2020) %>% 
  rename(name = NAME, fips = GEOID, state_fips = STATEFP)  %>% 
  mutate(name = case_when(fips == "51600" ~ "Fairfax City",
                          fips == "51620" ~ "Franklin City",
                          fips == "51770" ~ "Roanoke City",
                          T ~ name)) %>% 
  filter(!name %in% c("United States", "New England", "Mideast", 
                          "Great Lakes", "Plains", "Southeast",
                          "Southwest", "Rocky Mountain", "Far West")) %>% 
  filter(!str_detect(fips, "^69|^78|^60|^66")) %>%
  filter(state_fips != "02")  %>% # remove Alaska's county geometry
  dplyr::select(name, fips, state_fips, geometry) %>% 
  st_transform("epsg:4326")

county_sf_pre <- county_gdp  %>% 
  left_join(county_sf_2020)  %>% 
  st_as_sf() # there are some rows with empty geometry because BEA did some modifications to FIPS codes; Address these rows below to ensure the geometries align with the GDP data.

# BEA did some modifications to FIPS codes:
# 1. Kalawao County in Hawaii is combined with Maui County and the combined area is designated with fips code 15901
# 2. The independent cities of Virginia with populations of less than 100,000 have been combined with an adjacent county and 
#       given codes beginning with 51901. In the name of the combined area, the county name appears first and is followed by the city name(s).
# 3. Menominee County, Wisconsin is combined with Shawano County for 1969–1988 as 55901. Separate estimates for Menominee and Shawano Counties begin in 1989.

# Important: whenenver you update data, please check whether those modification changes
#   they should be shown in the place where you download the GDP data (See Appendix for where to download the data)

which_county <- county_sf_pre  %>% 
  filter(st_is_empty(.))  %>% 
  pull(name)  %>% 
  unique()

kala_maui <- county_sf_2020  %>% 
  filter(name %in% c("Kalawao", "Maui"))  %>% 
  summarize(name = "Maui + Kalawao",
            fips = "15901",
            state_fips = "15",
            geometry = st_union(geometry))

fremont_yellowstone <- county_sf_2020  %>% 
  filter(name %in% c("Fremont"), fips == "16043")  %>% # The geometry of Fremont county already contains the geom of Yellowstone park, so we only need to change the name of the geometry
  mutate(name = "Fremont (includes Yellowstone Park)")

lagrange <- county_sf_2020  %>% 
  filter(name %in% c("LaGrange"))  %>% 
  mutate(name = "Lagrange")

bsc <- county_sf_2020  %>% 
  filter(fips %in% c("24510", "29510", "32510"))  %>% 
  mutate(name = paste0(name, " (Independent City)"))

# almost have to redo everything of Viginia
indp_city_51 <- county_sf_2020  %>% 
  filter(name %in% c("Baltimore", "St. Louis", "Carson City", "Alexandria", "Chesapeake", "Hampton", "Newport News", "Norfolk",
                     "Portsmouth", "Richmond", "Roanoke City", "Suffolk", "Virginia Beach"), state_fips == "51")  %>% 
  filter(fips != "51159") %>% # There are two counties named Richmond in Viginia, the one with fips code 51760 is what we need to change. 
  mutate(name = ifelse(name == "Roanoke City", "Roanoke (Independent City)", paste0(name, " (Independent City)")))

albe_charlo <- county_sf_2020  %>% 
  filter(name %in% c("Albemarle", "Charlottesville"), state_fips == "51")  %>% 
  summarize(name = "Albemarle + Charlottesville",
            fips = "51901",
            state_fips = "51",
            geometry = st_union(geometry))

alle_covi <- county_sf_2020  %>% 
  filter(name %in% c("Alleghany", "Covington"), state_fips == "51")  %>% 
  summarize(name = "Alleghany + Covington",
            fips = "51903",
            state_fips = "51",
            geometry = st_union(geometry))

aug_sta_way <- county_sf_2020  %>% 
  filter(name %in% c("Augusta", "Staunton", "Waynesboro"), state_fips == "51")  %>% 
  summarize(name = "Augusta, Staunton + Waynesboro",
            fips = "51907",
            state_fips = "51",
            geometry = st_union(geometry))

camp_lyn <- county_sf_2020  %>% 
  filter(name %in% c("Campbell", "Lynchburg"), state_fips == "51")  %>% 
  summarize(name = "Campbell + Lynchburg",
            fips = "51911",
            state_fips = "51",
            geometry = st_union(geometry))

car_gal <-  county_sf_2020  %>% 
  filter(name %in% c("Carroll", "Galax"), state_fips == "51")  %>% 
  summarize(name = "Carroll + Galax",
            fips = "51913",
            state_fips = "51",
            geometry = st_union(geometry))

dpc <- county_sf_2020  %>% 
  filter(name %in% c("Dinwiddie", "Colonial Heights", "Petersburg"), state_fips == "51")  %>% 
  summarize(name = "Dinwiddie, Colonial Heights + Petersburg",
            fips = "51918",
            state_fips = "51",
            geometry = st_union(geometry))

fff <- county_sf_2020  %>% 
  filter(name %in% c("Fairfax", "Fairfax City", "Falls Church"), state_fips == "51")  %>% 
  summarize(name = "Fairfax, Fairfax City + Falls Church",
            fips = "51919",
            state_fips = "51",
            geometry = st_union(geometry))

fw <- county_sf_2020  %>% 
  filter(name %in% c("Frederick", "Winchester"), state_fips == "51")  %>% 
  summarize(name = "Frederick + Winchester",
            fips = "51921",
            state_fips = "51",
            geometry = st_union(geometry))

ge <- county_sf_2020  %>% 
  filter(name %in% c("Greensville", "Emporia"), state_fips == "51")  %>% 
  summarize(name = "Greensville + Emporia",
            fips = "51923",
            state_fips = "51",
            geometry = st_union(geometry))

hm <- county_sf_2020  %>% 
  filter(name %in% c("Henry", "Martinsville"), state_fips == "51")  %>% 
  summarize(name = "Henry + Martinsville",
            fips = "51929",
            state_fips = "51",
            geometry = st_union(geometry))

jw <- county_sf_2020  %>% 
  filter(name %in% c("James City", "Williamsburg"), state_fips == "51")  %>% 
  summarize(name = "James City + Williamsburg",
            fips = "51931",
            state_fips = "51",
            geometry = st_union(geometry))

mr <- county_sf_2020  %>% 
  filter(name %in% c("Montgomery", "Radford"), state_fips == "51")  %>% 
  summarize(name = "Montgomery + Radford",
            fips = "51933",
            state_fips = "51",
            geometry = st_union(geometry)) 

pd <- county_sf_2020  %>% 
  filter(name %in% c("Pittsylvania", "Danville"), state_fips == "51")  %>% 
  summarize(name = "Pittsylvania + Danville",
            fips = "51939",
            state_fips = "51",
            geometry = st_union(geometry)) 

pgh <- county_sf_2020  %>% 
  filter(name %in% c("Prince George", "Hopewell"), state_fips == "51")  %>% 
  summarize(name = "Prince George + Hopewell",
            fips = "51941",
            state_fips = "51",
            geometry = st_union(geometry)) 

pwmmp <- county_sf_2020  %>% 
  filter(name %in% c("Prince William", "Manassas", "Manassas Park"), state_fips == "51")  %>% 
  summarize(name = "Prince William, Manassas + Manassas Park",
            fips = "51942",
            state_fips = "51",
            geometry = st_union(geometry))   

rs <- county_sf_2020  %>% 
  filter(name %in% c("Roanoke", "Salem"), state_fips == "51")  %>% 
  summarize(name = "Roanoke + Salem",
            fips = "51944",
            state_fips = "51",
            geometry = st_union(geometry))   

rbl <- county_sf_2020  %>% 
  filter(name %in% c("Rockbridge", "Buena Vista", "Lexington"), state_fips == "51") %>% 
  summarize(name = "Rockbridge, Buena Vista + Lexington",
            fips = "51945",
            state_fips = "51",
            geometry = st_union(geometry))   

rh <- county_sf_2020  %>% 
  filter(name %in% c("Rockingham", "Harrisonburg"), state_fips == "51") %>% 
  summarize(name = "Rockingham + Harrisonburg",
            fips = "51947",
            state_fips = "51",
            geometry = st_union(geometry))   

sf <- county_sf_2020  %>% 
  filter(name %in% c("Southampton", "Franklin City"), state_fips == "51")  %>% 
  summarize(name = "Southampton + Franklin",
            fips = "51949",
            state_fips = "51",
            geometry = st_union(geometry))   

spfre <- county_sf_2020  %>% 
  filter(name %in% c("Spotsylvania", "Fredericksburg"), state_fips == "51") %>% 
  summarize(name = "Spotsylvania + Fredericksburg",
            fips = "51951",
            state_fips = "51",
            geometry = st_union(geometry))   

wb <- county_sf_2020  %>% 
  filter(name %in% c("Washington", "Bristol"), state_fips == "51") %>% 
  summarize(name = "Washington + Bristol",
            fips = "51953",
            state_fips = "51",
            geometry = st_union(geometry))   

wn <- county_sf_2020  %>% 
  filter(name %in% c("Wise", "Norton"), state_fips == "51") %>% 
  summarize(name = "Wise + Norton",
            fips = "51955",
            state_fips = "51",
            geometry = st_union(geometry))   

yp <- county_sf_2020  %>% 
  filter(name %in% c("York", "Poquoson"), state_fips == "51") %>% 
  summarize(name = "York + Poquoson",
            fips = "51958",
            state_fips = "51",
            geometry = st_union(geometry))   

# now combine them
county_sf_more <- bind_rows(kala_maui, fremont_yellowstone, lagrange, indp_city_51, bsc, albe_charlo,
                            alle_covi, aug_sta_way, camp_lyn, car_gal, dpc, fff, fw, ge, hm, jw, mr,
                            pd, pgh, pwmmp, rs, rbl, rh, sf, spfre, wb, wn, yp)

county_sf <- county_sf_pre %>%
  filter(!st_is_empty(.)) %>%
  distinct(name, fips, state_fips, geometry) %>%
  bind_rows(county_sf_more)  %>% 
  rename(id = fips)  %>% 
  mutate(iso = "USA")  %>% 
  dplyr::select(id, iso)  %>% 
  rename(geom = geometry)
  
# check whether every line of gdp data have a corresponding geometry
check <- county_gdp  %>% 
  rename(id = fips) %>% 
  left_join(county_sf)  %>% 
  filter(st_is_empty(geom)) # it is fine that Broomfield's rgdp_total is NA in 2001, because it is founded in 2002

county_gdp_recoded <- county_gdp %>% 
  mutate(min_admin_unit = 3) %>% 
  rename_with(matches("rgdp_"), .fn = ~ paste0("admin_3_", .x)) %>% 
  rename(admin_3_name = name) %>% 
  left_join(state_gdp)

training_df <- county_gdp_recoded %>%
  mutate(id = as.character(fips)) %>% 
  rename_with(matches("admin_2|admin_1"), 
              .fn = ~ paste0(str_sub(.x, 1, 8), "parent_", str_sub(.x, 9, -1))) %>% 
  pivot_longer(cols = matches("admin_2|admin_1"),
               names_to = c("parent_admin_unit", ".value"),
               names_pattern = "admin_(\\d)_(.+)") %>% 
  rename_with(starts_with("admin_3"), .fn = ~ gsub("admin_3", "unit", .x)) %>% 
  dplyr::select(id, year, iso, unit_name, min_admin_unit, matches("unit_rgdp"),
         parent_admin_unit, parent_name, matches("parent_rgdp"))
  
country_sf <- county_sf_2020 %>% 
  filter(state_fips %in% state_gdp$state_fips) %>% 
  summarize(name = "United States", admin_unit = 1, geom = st_union(geometry)) %>% 
  mutate(iso = "USA")

st_write(country_sf, "step2_obtain_gdp_data/temp/usa_admin_1_with_waters.gpkg", append = F)
st_write(county_sf, "step2_obtain_gdp_data/temp/usa_admin_3_with_waters.gpkg", append = F)

write.csv(county_gdp_recoded, "step2_obtain_gdp_data/temp/usa_gdp_clean.csv", row.names = F)
write.csv(training_df, "step2_obtain_gdp_data/temp/usa_training_data.csv", row.names = F)
write.csv(state_gdp, "step2_obtain_gdp_data/temp/usa_state_gdp.csv", row.names = F)

# now remove large inland waters

difference <- qgis_run_algorithm(
  alg = "native:difference",
  INPUT = "step2_obtain_gdp_data/temp/usa_admin_1_with_waters.gpkg", 
  OVERLAY = "step1_obtain_gis_data/inputs/large_inland_waters_geom_GLWD_level1/glwd_1.shp", 
  OUTPUT = "step2_obtain_gdp_data/temp/usa_admin_1.gpkg", 
  .quiet = FALSE
)

difference <- qgis_run_algorithm(
  alg = "native:difference",
  INPUT = "step2_obtain_gdp_data/temp/usa_admin_3_with_waters.gpkg", 
  OVERLAY = "step1_obtain_gis_data/inputs/large_inland_waters_geom_GLWD_level1/glwd_1.shp", 
  OUTPUT = "step2_obtain_gdp_data/temp/usa_admin_3.gpkg", 
  .quiet = FALSE
)

