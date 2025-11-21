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
  
  # Get the actual variable name (e.g., premiumhtml5map_map_cfg_12)
  var_name <- str_match(js_block, "var\\s+(premiumhtml5map_map_cfg_\\d+)\\s*=")[,2]
  
  # Evaluate and extract map_data via V8
  ctx <- v8()
  ctx$eval(js_block)
  
  # stringify only map_data
  json_map <- ctx$eval(paste0("JSON.stringify(", var_name, ".map_data)"))
  
  map_data <- fromJSON(json_map)
  
  # Build dataframe: name + comment + ids
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
all_data <- map_dfr(1:52, parse_url) 


#Join to FIPS first

# Normalizer used for matching
norm_nm <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("\\.", "") %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish() %>%
    str_replace_all("\\bsaint\\b", "st") %>%  # Saint -> st (matching)
    str_replace("\\s+(county|parish|borough|census area|municipality|city)$", "") %>%
    str_squish()
}

# Reference from tidycensus
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

# 1) Infer state_name for each state_id by overlap of county names
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

# 2) Join back to rows and attach FIPS on (state_name, county)
final_df <- all_data %>%
  mutate(
    name_norm  = norm_nm(name),
    county     = str_replace(name, "^Saint\\b", "St.")  # display: Saint -> St.
  ) %>%
  left_join(state_guess, by = "state_id") %>%  # adds inferred state_name
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


#Now join them 
if (!yesterday %in% colnames(old_data)){
  new_data <- old_data %>%
    left_join(final_df %>%
                select(county, price ), by ='county') %>%
    mutate(latest = price) %>%
    rename(!!yesterday := price)
  write_csv(new_data, 'gas_prices.csv')} else{
  new_data <- old_data %>%
    mutate(latest = !!yesterday)
}
  
###Datawrapper table

dw_edit_chart(
  chart_id = gasTable,
  title = 'Current as prices by county across the U.S.',
  intro = 'Search for your county in the table below to see what gas prices may look like for you throughout Thanksgiving Week',
  byline = 'Susie Webb/Get the Facts Data Team',
  source_name = 'AAA',
  source_url = 'aaa.com',
  annotate = paste0("<i>Data as of ",today_head,".")
)

#Adding data to the chart
dw_data_to_chart(new_data,
                 chart_id = gasTable
)

#Republishing the chart
dw_publish_chart(gasTable)


###Datawrapper map
dw_edit_chart(
  chart_id = gasMap,
  title = 'Map of current as prices by county across the U.S.',
  intro = 'Search for your county in the table below to see what gas prices may look like for you throughout Thanksgiving Week',
  byline = 'Susie Webb/Get the Facts Data Team',
  source_name = 'AAA',
  source_url = 'aaa.com',
  annotate = paste0("<i>Data as of ",today_head,".")
)

#Adding data to the chart
dw_data_to_chart(new_data,
                 chart_id = gasMap
)

#Republishing the chart
dw_publish_chart(gasMap)
