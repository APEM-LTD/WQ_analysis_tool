############################# CONSTANTS ###################################

ALLOWED_PARAMS <- c("temp", "RDO_sat", "pH", "BOD", "orthoP", "amm_N", "nitrate")

SUMM_ROW_NAMES <- c("Site ID", "Data source(s)","First sample date", "Last sample date", "Latitude", "Longitude",
             "Total samples","BOD samples", "DO samples", "Ammoniacal N samples", "Nitrate samples",
             "Orthophosphate samples", "pH samples", "Temperature samples")

TEMP_Z <- 2.0537
DO_Z <- -1.2816
PH_Z <- -1.6449
AMM_Z <- 1.2816
BOD_Z <- 1.2816


############################# FUNCTIONS ###################################

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
                  time_str = paste(hour_temp, min_temp, sep = ":")) %>%
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

  cols <- c("date_time", "date", "time", "time_str", "location_name", "location_id", "latitude", "longitude", "device_sn",
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
    dplyr::select(c(all_of(cols), any_of(params))) %>%
    dplyr::mutate(across(all_of(params), as.character))

  return(df)

}


calculate_season <- function(df, date_col, to_factor = TRUE) {
  ###
  # Function to calculate season based on a date-format column
  #
  # args:
  #   df (dataframe/tibble): Dataframe containing data of interest.
  #   date_col (string): name of the column containing the dates.
  #   to_factor (logical): whether to convert the new season column to a factor. Levels start at Spring
  #
  # return:
  #   (dataframe/tibble): data frame with new column season
  ###

  if (is.character(date_col) == FALSE) {
    stop("date_col must be specified as a string")
  }

  df <- df %>%
    dplyr::mutate(mth = lubridate::month(date),
                  season := ifelse(lubridate::month(!!rlang::sym(date_col)) %in% c(3, 4, 5), "Spring",
                                  ifelse(lubridate::month(!!rlang::sym(date_col)) %in% c(6, 7, 8), "Summer",
                                         ifelse(lubridate::month(!!rlang::sym(date_col)) %in% c(9, 10, 11), "Autumn",
                                                ifelse(lubridate::month(!!rlang::sym(date_col)) %in% c(12, 1, 2), "Winter", "Error")))))

  if (to_factor == TRUE) {
    df <- df %>%
      dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter"), exclude = NA))
  }

  return(df)

}


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
    dplyr::mutate(alt_used = ifelse(!!rlang::sym(alt_var) > 355, 355, round(!!rlang::sym(alt_var))),
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

plotly_legends <- function(chart) {
  ###
  # Function to remove duplicate legend entries in interactive plots generated
  #    by plotly::ggplotly()
  #    Code copied and adapted from:
  #    https://stackoverflow.com/questions/69289623/avoid-legend-duplication-in-plotly-conversion-from-ggplot-with-facet-wrap
  #
  # args:
  #   chart (plotly): A plotly chart created using plotly::ggplotly()
  #
  # return:
  #   (plotly): plotly chart object with updated legend
  ###

  # Get the names of the legend entries
  df <- data.frame(id = seq_along(chart$x$data), legend_entries = unlist(lapply(chart$x$data, `[[`, "name")))
  # Extract the group identifier
  df$legend_group <- gsub("^\\((.*?),\\d+\\)", "\\1", df$legend_entries)
  # Add an indicator for the first entry per group
  df$is_first <- !duplicated(df$legend_group)

  # WQ tool - specify label for grey background
  df <- df %>%
    dplyr::mutate(legend_group = ifelse(legend_group == "#D9D9D9", "Tributary river", legend_group))

  for (i in df$id) {
    # Is the layer the first entry of the group?
    is_first <- df$is_first[[i]]
    # Assign the group identifier to the name and legendgroup arguments
    chart$x$data[[i]]$name <- df$legend_group[[i]]
    chart$x$data[[i]]$legendgroup <- chart$x$data[[i]]$name
    # Show the legend only for the first layer of the group
    if (!is_first) chart$x$data[[i]]$showlegend <- FALSE
  }

  return(chart)

}
