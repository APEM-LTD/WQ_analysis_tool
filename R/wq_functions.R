############################# IsDate Function ###################################
# Copied from hetoolkit

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


check_names <- function(names, allowed) {
  ###
  # ERROR CHECK: are user-specified names present in an allowed list.
  #
  # args:
  #   names (string): vector of names to check.
  #   allowed (string): vector of allowed names to compare.
  #
  # return:
  #   (error): if >0 names in cols not in allowed names list.
  ###

  error <- vector(length = length(names))

  for (name in names){
    if (!(name %in% allowed)) {
      error <- c(error, name)
    }
  }

  error <- error[error != "FALSE"]
  if(length(error) > 0) {
    stop(paste0("The following name is not allowed: ", error, "\n"))
  }

}


extract_date_time <- function(df, dt_var="date_time"){
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
                  datetime_temp = lubridate::ymd_hms(!!rlang::sym(dt_var)),
                  hour_temp = lubridate::hour(datetime_temp),
                  min_temp = lubridate::minute(datetime_temp),
                  date = lubridate::date(datetime_temp),
                  time = paste(hour_temp, min_temp, sep = ":")) %>%
    dplyr::select(!c(datetime_temp, hour_temp, min_temp))


  return(df)
}


restructure_wq_data <- function(df){
  ###
  # Function to restructure a dataset and infill any missing variables
  #
  # args:
  #   df (dataframe/tibble): Dataframe containing data of interest.
  #
  # return:
  #   (dataframe/tibble): restructured dataframe
  ###

  cols <- c("date_time", "date", "time", "location_name", "location_id", "latitude", "longitude", "device_sn",
            "surveyor",  "lab", "sample_ID", "source")

  df_cols <- colnames(df)

  for (c in cols) {
    if(c %in% df_cols == FALSE){
      df <- df %>%
        dplyr::mutate(!!c := NA_character_)
    }
  }

  df_cols <- colnames(df)

  params <- df_cols[df_cols %in% cols == FALSE]

  df <- df %>%
    dplyr::select(c(all_of(cols), any_of(params)))

  return(df)

}

