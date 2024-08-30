###
# INITIAL USER ENTRY
###

COL_NAMES <- cols <- c("date", "location", "DO", "PO4", "test", "test1")
# Won't be needed if using import template
# Include default variable names to be dropped (eg, DROP_1, DROP_2...)

START_DATE <- NULL
END_DATE <- NULL

WQ_SITE_IDS <- NULL
# Site IDs for EA WIMS database. If NULL, no download.

TEMPLATE_PATH #<- "file_name.csv" or "path/to/folder/with/files"
# Opening multiple files/sheets https://stackoverflow.com/questions/66749376/writing-for-loop-to-read-excel-sheets

SHEETS <- c("PROBE_DATA", "MANUAL_FIELD", "LAB_DATA")
# May be needed if importing current excel workbooks or for selecting specific sheets from the template.
# Can be compared the readxl::excel_sheets("path")
# Must be specified is length(readxl::excel_sheets("path")) > 1

PARAMETERS <- "all"
# Can be any list of parameter names (see PARAMETERS tab in import template for allowed parameters)


## LIBRARIES

# Load required libraries
if(!require(pacman)) install.packages("pacman")
pacman::p_load(remotes, readxl, dplyr, stringr, lubridate, ggplot2, writexl,
               tidyr, DT, rnrfa, leaflet, downloader, sf, GGally, gridextra,
               rlang, readr) # new for WQ tool

# Conditionally install hetoolkit from github
if ("hetoolkit" %in% installed.packages() == FALSE) {
  remotes::install_github("APEM-LTD/hetoolkit")
}

library(hetoolkit)


###
# FUNCTION 1
# Check column names specified by the user are present in a list of allowed names.
# Assumes column names set as part of user input
# Column names specified must be valid for use in R
###


library(dplyr)

cols <- c("date", "location", "DO", "PO4", "test", "test1")

check_column_names <- function(cols) {
  ###
  # ERROR CHECK: are user specified column names present in an allowed list.
  #
  # args:
  #   cols (string): vector of column names.
  #
  # return:
  #   (error): if >0 names in cols not in allowed names list.
  ###

  allowed <- c("date", "location", "DO", "PO4", "NH4")

  error <- vector(length = length(cols))

  for (col in cols){
    if (!(col %in% allowed)) {
      error <- c(error, col)
    }

  }

  error <- error[error != "FALSE"]
  if(length(error) > 0) {
    stop(paste0("The following column name is not allowed: ", error, "\n"))
  }

}

check_column_names(cols)


###
# FUNCTION 2
# Split a datetime variable into date and time variables
###

library(readr)
library(lubridate)
library(rlang)

extract_date_time <- function(df, dt_var="datetime"){
  ###
  # Function to convert values in a datetime variable into separate date and time variables.
  # https://stackoverflow.com/questions/48219732/pass-a-string-as-variable-name-in-dplyrfilter
  #
  # args:
  #   df (dataframe/tibble): Dataframe containing data of interest.
  #   dt_var (string): the name of the datetime variable.
  #
  # return:
  #   (dataframe/tibble): Dataframe with additional variables `date` and `time`.
  ###

  # Error check
  if (!(dt_var %in% colnames(df))) {
    stop(paste0("Datetime variable `", dt_var, "` not in data frame"))
  }

  df <- df %>%
    dplyr::mutate(!!dt_var := as.character(!!rlang::sym(dt_var)),              ## !! is to unquote the string
                  datetime_temp = lubridate::dmy_hm(!!rlang::sym(dt_var)),
                  hour_temp = lubridate::hour(datetime_temp),
                  min_temp = lubridate::minute(datetime_temp),
                  date = lubridate::date(datetime_temp),
                  time = paste(hour_temp, min_temp, sep = ":")) %>%
    dplyr::select(!c(datetime_temp, hour_temp, min_temp))


  return(df)
}

data <- readr::read_csv(file = "Data/WQ_probe_example.csv", skip = 6)
#colnames(data)[1] <- "date time"
test <- extract_date_time(data, "Date Time")


###
# Function 3 Checks if a field is a date (for error checking)
###
############################# IsDate Function ###################################

IsDate <- function(x, date.format = NULL) {
  # Check if field is a date using as.Date that looks for unambiguous dates
  #   Assumes date format so NA returned not Character error.
  #   Why? with no date format, R tries two defaults then gives error.
  #   BUT With a dateformat R returns NA
  # Args
  #   Suspected date and optional date format string
  # Returns
  #   TRUE if thinks it is a date
  formatted = try(as.Date(x, date.format), silent = TRUE)
  is_date = as.character(formatted) == x & !is.na(formatted)  # valid and identical to input
  is_date[is.na(x)] = NA  # Insert NA for NA in x
  return(is_date)
}

### Test processing sheets within lists
## Process probe data
if("PROBE_DATA" %in% names(sheets) == TRUE){
  #probe_data <- sheets[["PROBE_DATA"]]
  sheets[["PROBE_DATA"]] <- extract_date_time(sheets[["PROBE_DATA"]], "date_time")

  # Add blank variables for matching
  sheets[["PROBE_DATA"]] <- sheets[["PROBE_DATA"]] %>%
    dplyr::mutate(surveyor = NA_character_,
                  location_ID = NA_character_,
                  lab = NA_character_,
                  sample_ID = NA_character_,
                  source = "probe")
}

## Process field data
if("MANUAL_FIELD" %in% names(sheets) == TRUE){
  #field_data <- sheets[["MANUAL_FIELD"]]

  # Add blank variables for matching
  sheets[["MANUAL_FIELD"]] <- sheets[["MANUAL_FIELD"]] %>%
    dplyr::mutate(date_time = NA_character_,
                  device_sn = NA_character_,
                  lab = NA_character_,
                  sample_ID = NA_character_,
                  source = "field")
}

## Process probe data
if("LAB_DATA" %in% names(sheets) == TRUE){
  #lab_data <- sheets[["LAB_DATA"]]

  # Add blank variables for matching
  sheets[["LAB_DATA"]] <- sheets[["LAB_DATA"]] %>%
    dplyr::mutate(surveyor = NA_character_,
                  location_ID = NA_character_,
                  date_time = NA_character_,
                  device_sn = NA_character_,
                  source = "lab")
}


####
wq_data1 <- wq_data %>%
  tidyr::pivot_wider(id_cols = c(wq_site_id, date),
                     names_from = det_label,
                     values_from = result)


#### Test openxlsx workbook
wb <- openxlsx::buildWorkbook(list(probe_data, lab_data, field_data))
names(wb) <- c("PROBE_DATA", "LAB_DATA", "MANUAL_FIELD")
openxlsx::conditionalFormatting(wb, "PROBE_DATA", cols = 6, rows = 2:nrow(probe_data)+1,
                                rule = "<20",
                                style = createStyle(fontColour = "#0070C0"))
saveWorkbook(wb, "test.xlsx", overwrite = TRUE)



#### Process LOD results
probe_data <- sheets[[1]]
probe_data <- probe_data %>%
  tidyr::pivot_longer(cols = names(probe_data)[6:ncol(probe_data)],
                      names_to = "parameter",
                      values_to = "result") %>%
  dplyr::mutate(limit = ifelse(grepl("<", result) == TRUE | grepl(">", result) == TRUE,
                               substr(result,regexpr("[[:digit:]]", result), nchar(result)), NA_character_),
                limit = as.numeric(limit),
                original_result = result,
                result = as.numeric(result))


### outliers - using boxplot.stats
full_data <- full_data %>%
  dplyr::mutate(outlier = FALSE)

for (l in unique(full_data$location_name)){
  for (p in param_names) {
    temp <- full_data %>%
      dplyr::filter(parameter == p & location_name == l)

    t <- ggplot(temp) +
      aes(x = "", y = result) +
      geom_boxplot() +
      labs(title = p)

    #print(t)

    outs <- boxplot.stats(temp$result)$out
    #print(p)
    #print(outs)

    full_data <- full_data %>%
      dplyr::mutate(outlier = ifelse(location_name == l & parameter == p & result %in% outs,
                                     TRUE, outlier))
  }
}

### Outliers original attempt
### Identify outliers
full_data <- full_data %>%
  dplyr::left_join(summary_1)

full_data <- full_data %>%
  dplyr::group_by(location_name, parameter) %>%
  dplyr::mutate(upper_bound = quantile(result, 0.95, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(outlier = ifelse(result > upper_bound, TRUE, FALSE))

outliers_data <- full_data %>%
  dplyr::filter(outlier) %>%
  dplyr::select(location_name, date, source, parameter, result, mn)

if (nrow(outliers_data) > 0) {
  OUT_tab <- DT::datatable(outliers_data)
} else {
  OUT_tab <- "NA - No potential outliers identified"
}


###### using dygraphs

library(dygraphs)


## Time series plots alternative

```{r ts_plots_dy, out.width = "100%"}

### Add seasons
full_data <- calculate_season(full_data, "date")

### Set up for loop
charts <- list()
counter <- 1

### Loop through site/parameter combinations and plot
for (l in unique(full_data$location_name)) {
  for (p in unique(full_data$parameter)) {

    chart_data <- full_data %>%
      dplyr::filter(location_name == l & parameter == p) %>%
      dplyr::mutate(x_min_val = as.Date(paste0(as.character(lubridate::year(min(date))), "-01-01")),
                    x_max_val = as.Date(paste0(as.character(lubridate::year(max(date))), "-12-31"))) %>%
      dplyr::select(date, result, output_headers, std_order, HIGH, GOOD, MODERATE, POOR)

    high <- unique(chart_data$HIGH)
    good <- unique(chart_data$GOOD)
    mod  <- unique(chart_data$MODERATE)
    poor <- unique(chart_data$POOR)
    max  <- max(c(max(chart_data$result), chart_data$HIGH + 5, chart_data$POOR + 5), na.rm = TRUE)
    min  <- min(c(min(chart_data$result), chart_data$HIGH - 5, chart_data$POOR - 5), na.rm = TRUE)
    if (min < 0 ) { min <- 0 }
    p_name <- unique(chart_data$output_headers)

    ### Charts with high status to top (higher values, higher status)
    if (unique(chart_data$std_order == "asc")){
      c <- dygraph(chart_data[, 1:2], main = paste0(l, ": ", p_name)) %>%
        dySeries("result", strokeWidth = 0, drawPoints = TRUE, pointSize = 3) %>%
        dyAxis("y", label = p_name, valueRange = c(min, max)) %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.8) %>%
        dyShading(from = poor, to = max, axis = "y", color = "rgb(255, 153, 153)") %>%
        dyShading(from = mod, to = poor, axis = "y", color = "rgb(255, 204, 102)") %>%
        dyShading(from = good, to = mod, axis = "y", color = "rgb(255, 255, 102)") %>%
        dyShading(from = high, to = good, axis = "y", color = "rgb(216, 228, 188)") %>%
        dyShading(from = min, to = high, axis = "y", color = "rgb(183, 222, 232)") %>%
        dyOptions(axisLineWidth = 1.5, fillGraph = FALSE, drawGrid = FALSE)
    }

    ### Charts with bad  status to top (higher values, lower status)
    if(unique(chart_data$std_order == "desc")){
      c <- dygraph(chart_data[, 1:2], main = paste0(l, ": ", p_name)) %>%
        dySeries("result", strokeWidth = 0, drawPoints = TRUE, pointSize = 3) %>%
        dyAxis("y", label = p_name, valueRange = c(min, max)) %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.8) %>%
        dyShading(from = high, to = max, axis = "y", color = "rgb(183, 222, 232)") %>%
        dyShading(from = good, to = high, axis = "y", color = "rgb(216, 228, 188)") %>%
        dyShading(from = mod, to = good, axis = "y", color = "rgb(255, 255, 102)") %>%
        dyShading(from = poor, to = mod, axis = "y", color = "rgb(255, 204, 102)") %>%
        dyShading(from = min, to = poor, axis = "y", color = "rgb(255, 153, 153)") %>%
        dyOptions(axisLineWidth = 1.5, fillGraph = FALSE, drawGrid = FALSE)
    }

    ### pH specific chart: high status between 6 and 9
    if(unique(chart_data$std_order == "pH")){
      c <- dygraph(chart_data[, 1:2], main = paste0(l, ": ", p_name)) %>%
        dySeries("result", strokeWidth = 0, drawPoints = TRUE, pointSize = 3) %>%
        dyAxis("y", label = p_name, valueRange = c(0, 15)) %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.8) %>%
        dyShading(from = 6, to = 9, axis = "y", color = "rgb(183, 222, 232)") %>%
        dyOptions(axisLineWidth = 1.5, fillGraph = FALSE, drawGrid = FALSE)
    }

    ### Parameters without WFD standards
    if(unique(chart_data$std_order == "none")){
      c <- dygraph(chart_data[, 1:2], main = paste0(l, ": ", p_name)) %>%
        dySeries("result", strokeWidth = 0, drawPoints = TRUE, pointSize = 3) %>%
        dyAxis("y", label = p_name, valueRange = c(min, max)) %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.8) %>%
        dyOptions(axisLineWidth = 1.5, fillGraph = FALSE, drawGrid = FALSE)
    }

    charts[[counter]] <- c
    counter <- counter + 1

  }
}

htmltools::tagList(charts)

```


#### WIMS additional query

get_wq_site_info <- function(df, id_var){
  ###
  # Function to query WIMS and return details of EA sampling points.
  # https://environment.data.gov.uk/water-quality/view/doc/reference#api-summary
  #
  # args:
  #   df (dataframe/tibble): Dataframe containing WIMS site IDs.
  #   id_var (string): the name of the variable containing WIMS site IDs.
  #
  # return:
  #   (dataframe/tibble): Dataframe with WIMS sampling point details. `@id` and `comment` columns are dropped
  #                       `notation` column is renamed to match id_var
  ###

  site_info <- df %>%
    dplyr::mutate(dummy = 1) %>%
    dplyr::group_by(dummy) %>%
    dplyr::summarise(query_suffix = paste0(unique(eval(as.name(id_var))), collapse = "&notation=")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(query_prefix = "https://environment.data.gov.uk/water-quality/id/sampling-point.csv?notation=",
                  query = paste0(query_prefix, query_suffix)) %>%
    dplyr::select(query)

  downloader::download(site_info$query,
                       destfile = "WQ_SAMPLE_POINT_DOWNLOAD.csv",
                       mode = 'wb')

  col_types <- readr::cols()
  #)

  # readcsv
  data <- readr::read_csv("WQ_SAMPLE_POINT_DOWNLOAD.csv",
                          col_types = col_types)

  data <- data %>%
    dplyr::select(-"@id", comment) %>%
    dplyr::rename(!!id_var := notation)

  return(data)

}


site_info <- get_wq_site_info(ea_wq_data, "EA_ID")

df <- ea_wq_data
id_var <- "EA_ID"

#### Percentile calculations
vals <- c(5.9, 1.0, 2.5, 2.6, 5.0)
mn <- mean(vals, na.rm = TRUE)
std <- sd(vals, na.rm = TRUE)

log(mn/(sqrt(1+((std**std)/(mn*mn))))) #m
sqrt(log(1+((std*std)/(mn*mn))))  #s


#### distance difference
library(geosphere)

distances <- full_data %>%
  dplyr::group_by(location_name, source) %>%
  dplyr::summarise(long = mean(longitude),
                   lat = mean(latitude)) %>%
  dplyr::ungroup()

loc_src <- paste(distances$location_name, distances$source, sep = "_")

dm <- geosphere::distm(distances[3:4])
colnames(dm) <- loc_src
rownames(dm) <- loc_src

x <- 1
for (l in unique(full_data$location_name)) {

  temp <- dm[grepl(l, rownames(dm)),grepl(l, colnames(dm))]
  colnames(temp) <- substr(colnames(temp), nchar(l) + 2, nchar(colnames(temp)))

  if (x == 1) {
    dists <- temp
    x <- 0
  } else {
    dists <- rbind(dists, temp)
  }
}

dist <- as.data.frame(dists)
dist$loc_src <- rownames(dist)
rownames(dist) <- NULL

dist <- dist %>%
  dplyr::mutate(location_name = substr(loc_src, 0, regexpr("_", loc_src)-1),
                source = substr(loc_src,regexpr("_", loc_src)+1, nchar(loc_src)))

distances <- dplyr::left_join(distances, dist) %>%
  dplyr::select(c(location_name, source, lat, long, EA_DATA, LAB_DATA, MANUAL_FIELD, PROBE_DATA))

distances <- distances %>%
  dplyr::mutate(across(where(is.numeric), \(x) round(x, digits = 3)),
                across(c("EA_DATA", "LAB_DATA", "MANUAL_FIELD", "PROBE_DATA"), \(x) x/1000))

### Output table
distances_output <- DT::datatable(distances, extensions = "Buttons",
                                  options = list(
                                    pageLength = nrow(distances),
                                    dom = 'Brti',
                                    buttons = c('copy', 'csv', 'excel')))

# Format table: loop - needs additional columns for each source if > 100m, then each column formatted individually.
distances_output <- distances_output %>%
  formatStyle(c("EA_DATA", "LAB_DATA", "MANUAL_FIELD", "PROBE_DATA"),
              backgroundColor = styleEqual(c(1,0), c("red", "white")))




### Calculate orthophosphate standards
## Requires update to metadata tab of import template

metadata <- metadata %>%
  dplyr::mutate(altitude_m = 115,
                alkalinity = 22)

calc_orthop_stds <- function(df, loc_var, alk_var, alt_var){
  ###
  # Calculate site-specific WFD standards for orthophosphate
  #
  # args:
  #   df (dataframe/tibble): Dataframe containing alkalinity and altitude data for individual sites.
  #   loc_var (string): name of variable defining the site.
  #   alk_var (string): name of variable containing alkalinity data
  #   alt_var (string): name of variable containing altitude data (actual value, not just lowland/upland)
  #
  # return:
  #   (dataframe/tibble): Dataframe containing location ID and orthophosphate standards in ug and mg
  ###

  VARS <- c(loc_var, alk_var, alt_var)

  ortho_stds <- df %>%
    dplyr::select(all_of(VARS)) %>%
    dplyr::mutate(alt_used = ifelse(alt_var > 355, 355, round(alt_var)),
                  alk_used = ifelse(!!rlang::sym(alk_var) < 2, 2, round(!!rlang::sym(alk_var))),
                  ref_P = 10 ** (0.454*log10(alk_used) - 0.0018*alt_used + 0.476),
                  ref_P_used = ifelse(ref_P < 7, 7, round(ref_P, 1)))

  ortho_stds <- ortho_stds %>%
    dplyr::mutate(orthoP_HIGH = round(10**((1.0497*log10(0.702) + 1.066) * (log10(ref_P_used) - log10(3500)) + log10(3500))),
                  orthoP_GOOD = round(10**((1.0497*log10(0.532) + 1.066) * (log10(ref_P_used) - log10(3500)) + log10(3500))),
                  orthoP_MOD  = round(10**((1.0497*log10(0.356) + 1.066) * (log10(ref_P_used) - log10(3500)) + log10(3500))),
                  orthoP_POOR = round(10**((1.0497*log10(0.166) + 1.066) * (log10(ref_P_used) - log10(3500)) + log10(3500))),
                  orthoP_HIGH_mg = orthoP_HIGH / 1000,
                  orthoP_GOOD_mg = orthoP_GOOD / 1000,
                  orthoP_MOD_mg = orthoP_MOD / 1000,
                  orthoP_POOR_mg = orthoP_POOR / 1000)

  ortho_stds <- ortho_stds %>%
    dplyr::select(location_name, names(ortho_stds)[grepl("orthoP_", names(ortho_stds))])

  return(ortho_stds)

}

x <- calc_orthop_stds(metadata, "location_name", "alkalinity", "altitude_m")

ortho_stds <- metadata %>%
  dplyr::select(location_name) %>%
  dplyr::mutate(altitude_m = 115,
                alkalinity = 22)

ortho_stds <- ortho_stds %>%
  dplyr::mutate(alt_used = ifelse(altitude_m > 355, 355, round(altitude_m)),
                alk_used = ifelse(alkalinity < 2, 2, round(alkalinity)),
                ref_P = 10 ** (0.454*log10(alk_used) - 0.0018*alt_used + 0.476),
                ref_P_used = ifelse(ref_P < 7, 7, round(ref_P, 1)))

ortho_stds <- ortho_stds %>%
  dplyr::mutate(orthoP_HIGH = round(10**((1.0497*log10(0.702) + 1.066) * (log10(ref_P_used) - log10(3500)) + log10(3500))),
                orthoP_GOOD = round(10**((1.0497*log10(0.532) + 1.066) * (log10(ref_P_used) - log10(3500)) + log10(3500))),
                orthoP_MOD  = round(10**((1.0497*log10(0.356) + 1.066) * (log10(ref_P_used) - log10(3500)) + log10(3500))),
                orthoP_POOR = round(10**((1.0497*log10(0.166) + 1.066) * (log10(ref_P_used) - log10(3500)) + log10(3500))),
                orthoP_HIGH_mg = orthoP_HIGH / 1000,
                orthoP_GOOD_mg = orthoP_GOOD / 1000,
                orthoP_MOD_mg = orthoP_MOD / 1000,
                orthoP_POOR_mg = orthoP_POOR / 1000) %>%
  dplyr::select(location_name, orthoP_HIGH_mg, orthoP_GOOD_mg, orthoP_MOD_mg, orthoP_POOR_mg)

full_data <- full_data %>%
  dplyr::left_join(ortho_stds) %>%
  dplyr::mutate(HIGH = ifelse(parameter == "orthoP", orthoP_HIGH_mg, HIGH),
                GOOD = ifelse(parameter == "orthoP", orthoP_GOOD_mg, GOOD),
                MODERATE = ifelse(parameter == "orthoP", orthoP_MOD_mg, MODERATE),
                POOR = ifelse(parameter == "orthoP", orthoP_POOR_mg, POOR)) %>%
  dplyr::select(!c(orthoP_HIGH_mg, orthoP_GOOD_mg, orthoP_MOD_mg, orthoP_POOR_mg))

