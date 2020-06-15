#' National High Safety Fatality Report Data Reader
#' \code{fars_read} will read data from the National Highway Traffic Safety
#' Administration's Fatality Analysis Reporting System into a dplyr
#' tbl_df data type
#'
#' @param filename A string representation of the filename where the data
#' is stored and must be a type of csv
#'
#' @return code will search for the filename is th filename exists the data
#' will be imported as a dplyr tbl_df
#'
#' @note Errors will be thrown if the argument filename is not a string
#' Or if the file does not exist in the directory
#' Or is the file is not a csv file
#'
#' @import readr
#' @import dplyr
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#'
#'@export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Year file name creator
#' \code{make_filename} will create a file name using the inputted year
#' in the format of \code{accident_year.csv.bz}.
#'
#' @param year a number representing the year of the file you want to create
#' must be a numeric type
#'
#' @return returns a string in the format of \code{accident_year.csv.bz} where
#' year is a the number of year that was inputted
#'
#' @note Errors will be thrown if the input year is not a numeric
#'
#' @examples
#' make_filename(2015)
#' make_filename(2013)
#'
#'@export
make_filename <- function(year) {
  year <- as.integer(year)
  fp <- sprintf("accident_%d.csv.bz2",year)
  return(system.file("extdata", fp, package = "farsss"))
}


#' Fatality report reader year by year
#' This function takes a vector of years as an input and goes through the years
#' one by one. The function takes one of the years and uses it as input to
#' \code{make_filename()} and creates a filename of the format
#' \code{accident_year.csv.bz}. Then function calls \code{fars_read()} to read in
#' the file as dply tbl_data then restructures it to be one year per column.
#' If the file is not available the function will return
#' \code{"invalid year: ..."}.
#'
#' @param years A vector representation the years of the files that you want to
#' load into R
#'
#' @return \code{fars_read_years} will search for all of the file years in the
#' inputted years vector and return the data if available if present an error
#' will be thrown
#'
#' @import dplyr
#' @import dplyr
#' @import magrittr
#'
#' @note View the \code{make_filename()} documentation for more info one how the
#' filenames are created.
#'
#' @note Errors will be thrown if the argument years are non-numeric
#' Or if the file does not exist in the directory
#' Or is the file is not a csv file
#' @examples
#' fars_read_years(2013:2015)
#' fars_read_years(c(2013,2015))
#'
#'@export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Summarizes information about the yearly fatalities
#' \code{fars_summarize_years} will provide a monthly and yearly count of
#' fatalities over the inputted years
#'
#' @param years vector of inputted years
#'
#' @return returns a tbl_df of monthly and yearly count of fatalities over the
#' inputted years
#'
#' @note Errors will be thrown if the argument years are non-numeric
#' Or if the file does not exist in the directory
#' Or is the file is not a csv file
#' @import dplyr
#' @import magrittr
#' @examples
#' fars_summarize_years(2013:2015)
#' fars_summarize_years(c(2013,2015))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plots all fatalities by location
#' \code{fars_map_state} will plot a map of fatalities by location for a a state
#' and a year
#'
#' @param state.num a number corresponding to a state
#' @param year a number corresponding to a year
#'
#' @return returns a map with a plot for fatalities for a state and year if no
#' fatalities in a given state in a year a message "no accidents to plot" will
#' be displayed as map
#'
#' @examples
#' fars_map_state(5,2015)
#'
#' @import dplyr
#' @import maps
#' @import graphics
#'
#' @note Errors will be thrown if the argument years are non-numeric
#' Or if the file does not exist in the directory
#' Or is the file is not a csv file
#' If state number is invalid
#' If state number is a non numeric
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
