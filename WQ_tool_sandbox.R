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
