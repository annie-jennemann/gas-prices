#####For adding to the existing csv initiall scraped

#Loading libraries
library(tidyverse)
library(jsonlite)
library(rvest)
library(DatawRappr)
library(httr)
library(V8)
library(tidycensus)

##Loading in DW cues
api_key <- Sys.getenv("API_KEY")
gasTable <- Sys.getenv("GAS_TABLE_KEY")
gasMap <- Sys.getenv("GAS_MAP_KEY")

datawrapper_auth(api_key =  api_key, overwrite=TRUE)


#Marking today's date
today_head <- format(as.POSIXct(Sys.time(), tz = "America/New_York"), "%b. %d")
today_head

yesterday <- format(as.Date(with_tz(Sys.time(), tz = 'America/New_York')) - 1, "%b. %d")

#Loading in the old data
old_data <- read_csv('gas_prices.csv')

#Scraping today's gas data
parse_url <- function(map_id) {
  url <- paste0("https://gasprices.aaa.com/index.php?premiumhtml5map_js_data=true&map_id=", map_id)
  response <- GET(url)
  data <- content(response, "text")
  
  #Pulling out the JS
  js_block <- str_extract(
    data,
    "(?s)var\\s+premiumhtml5map_map_cfg_\\d+\\s*=\\s*\\{.*?\\};"
  )
  
  #Accounting for NAs
  if (is.na(js_block)) {
    return(tibble(name = character(), comment = character(),
                  state_id = integer()))
  }
  
  var_name <- str_match(js_block, "var\\s+(premiumhtml5map_map_cfg_\\d+)\\s*=")[,2]
  
  ctx <- v8()
  ctx$eval(js_block)
  
  json_map <- ctx$eval(paste0("JSON.stringify(", var_name, ".map_data)"))
  
  map_data <- fromJSON(json_map)
  
  #Build df
  df <- map_data %>%
    map_dfr(~tibble(
      name = .x$name %||% NA_character_,
      comment = .x$comment %||% NA_character_
    )) %>%
    mutate(
      state_id = map_id,
    )
  
  return(df)
}

#Run it through each state
all_data <- map_dfr(setdiff(1:52, 17), parse_url)


##Join to FIPS

#Normalize the data
norm_nm <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("\\.", "") %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish() %>%
    str_replace_all("\\bsaint\\b", "st") %>%  # Saint -> st (matching)
    str_replace("\\s+(county|parish|borough|census area|municipality)$", "") %>%
    str_squish()
}

data("fips_codes", package = "tidycensus")
fips_ref <- fips_codes %>%
  transmute(
    state_name       = state,
    statefp          = state_code,
    county_name      = county,
    countyfp         = county_code,
    GEOID            = paste0(state_code, county_code),
    county_name_norm = norm_nm(county)
  ) %>%
  distinct()

#Infer state name
state_guess <- all_data %>%
  mutate(name_norm = norm_nm(name)) %>%
  distinct(state_id, name_norm) %>%
  inner_join(
    fips_ref %>% distinct(state_name, county_name_norm),
    by = c("name_norm" = "county_name_norm")
  ) %>%
  count(state_id, state_name, name = "hits", sort = TRUE) %>%
  group_by(state_id) %>%
  slice_max(hits, n = 1, with_ties = FALSE) %>%
  ungroup()

final_df <- all_data %>%
  mutate(
    name_norm  = norm_nm(name),
    county     = str_replace(name, "^Saint\\b", "St.")  # display: Saint -> St.
  ) %>%
  left_join(state_guess, by = "state_id") %>%
  left_join(
    fips_ref %>% select(state_name, county_name_norm, GEOID, statefp),
    by = c("state_name", "name_norm" = "county_name_norm")
  ) %>%
  mutate(
    price = as.numeric(gsub("\\$", "", comment))
  ) %>%
  select(
    state_id,
    state_name,
    statefp,
    GEOID,
    county,      
    comment,
    price
  )

new_data <- final_df

###Datawrapper table

dw_edit_chart(
  chart_id = gasTable,
  title = 'Current gas prices by county across the U.S.',
  intro = "Search by county in the table below to see what the gas prices are where you're traveling throughout Thanksgiving week. The table is sorted by most expensive to least expensive.",
  byline = 'Susie Webb/Get the Facts Data Team',
  source_name = 'AAA',
  source_url = 'aaa.com',
  annotate = paste0("<i>Data as of ",today_head," and represents the previous day's average cost for regular gas Any NA values mean that there was not enough data to calculate the price.")
  )

#Adding data to the chart
dw_data_to_chart(new_data,
                 chart_id = gasTable
)

#Republishing the chart
dw_publish_chart(gasTable)

####Editing data for map

new_final_df <- final_df %>%
  mutate(
    GEOID = case_when(
      state_name == "FL" & county == "De Soto" ~ "12027", 
      state_name == "AL" & county == "De Kalb" ~ "01049", 
      state_name == "AK" & county == "Juneau" ~ "02110", 
      state_name == "AK" & county == "Prince Wales Ketchikan" ~ "02201", 
      state_name == "AK" & county == "Sitka" ~ "02220", 
      state_name == "AK" & county == "Skagway Hoonah Angoon" ~ "02232", 
      state_name == "AK" & county == "Yakutat" ~ "02282", 
      state_name == "IL" & county == "Dewitt" ~ "17039", 
      state_name == "IL" & county == "De Kalb" ~ "17037", 
      state_name == "IL" & county == "Du Page" ~ "17043", 
      state_name == "IL" & county == "La Salle" ~ "17099", 
      
      TRUE ~ GEOID
    ),
    county = case_when(
      state_name == "FL" & county == "De Soto" ~ "DeSoto",
      state_name == "AL" & county == "De Kalb" ~ "DeKalb",
      state_name == "IL" & county == "Dewitt" ~ "De Witt", 
      state_name == "IL" & county == "De Kalb" ~ "DeKalb",
      state_name == "IL" & county == "Du Page" ~ "DuPage", 
      
      TRUE ~ county
    ))
  




###Datawrapper map
dw_edit_chart(
  chart_id = gasMap,
  title = 'Map of current as prices by county across the U.S.',
  intro = 'Here are the latest gas prices as drivers take to the roads for Thanskgiving holiday travel.',
  byline = 'Susie Webb/Get the Facts Data Team',
  source_name = 'AAA',
  source_url = 'aaa.com',
  annotate = paste0("<i>Data as of ",today_head," and represents the previous day's average cost for regular gas.")
)

#Adding data to the chart
dw_data_to_chart(new_final_df,
                 chart_id = gasMap
)

#Republishing the chart
dw_publish_chart(gasMap)
