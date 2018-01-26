
# Utils -------------------------------------------------------------------


#' @importFrom utils choose.dir
choose_path <- function() {
  if (exists("choose.dir", getNamespace("utils"))) {
    path <- utils::choose.dir(getwd(), "Select directory with files to read")
    if (is.na(path))
      stop("You have canceled the execution.")
  } else {
    path <- file.choose()
  }
  return(path)
}


select_file <- function(path, pattern = "Hydrauliques", fileext = "\\.xml$", multiple = FALSE) {
  if (dir.exists(path)) {
    path <- list.files(path = path, pattern = fileext, full.names = TRUE)
    path <- grep(pattern = pattern, x = path, value = TRUE)
    if (length(path) < 1)
      stop("No file found : specify complete path.", call. = FALSE)
    if (!multiple) {
      path <- sort(path, decreasing = TRUE)[1]
    } else {
      path <- sort(path, decreasing = TRUE)
    }
    message(paste("Reading file:", path, "\n"))
  }
  return(path)
}




locf <-  function(x) {
  x[cummax((!is.na(x)) * seq_along(x))]
}


clean_names <- function(x) {
  x <- tolower(x)
  x <- gsub(pattern = "[[:space:]]+", replacement = "_", x = x)
  char_e <- paste(intToUtf8(232:235, multiple = TRUE), collapse = "|")
  x <- gsub(pattern = char_e, replacement = "e", x = x)
  char_i <- paste(intToUtf8(236:239, multiple = TRUE), collapse = "|")
  x <- gsub(pattern = char_i, replacement = "i", x = x)
  char_u <- paste(intToUtf8(249:252, multiple = TRUE), collapse = "|")
  x <- gsub(pattern = char_u, replacement = "u", x = x)
  char_o <- paste(intToUtf8(c(240, 242:246), multiple = TRUE), collapse = "|")
  x <- gsub(pattern = char_o, replacement = "o", x = x)
  char_a <- paste(intToUtf8(224:229, multiple = TRUE), collapse = "|")
  x <- gsub(pattern = char_a, replacement = "a", x = x)
  x <- gsub(pattern = "[[:punct:]]+", replacement = "_", x = x)
  x
}

n_hours <- function(x, y) {
  abs(as.numeric(difftime(time1 = x, time2 = y, units = "hours"))) + 1
}


dropNulls <- function (x)
{
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

pasteAND <- function(...) {
  paste(..., sep = " AND ")
}




#' Get previous day from a date 
#'
#' @param what Name of the day to retrieve previous the specified date
#' @param date A date, by default the current date
#'
#' @return a Date
#' @export
#'
#' @examples
#' 
#' # Previous friday
#' get_previous(what = "vendredi")
#' 
#' # You can abreviate
#' get_previous(what = "ven")
#' 
#' # Previous saturday before previous friday
#' get_previous(what = "samedi", date = get_previous(what = "vendredi"))
#' 
#' # Alternatively you can pass a number between 1-7 where 1 is Monday
#' get_previous(what = 7)
#' 
get_previous <- function(what = "samedi", date = Sys.Date()) {
  stopifnot(length(what) == 1, length(date) == 1)
  french <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")
  english <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  if (!(is.numeric(what) & what %in% 1:7)) {
    what <- pmatch(what, french)
    if (is.na(what)) {
      what <- pmatch(what, english)
    } 
    if (is.na(what)) {
      stop(
        paste(
          "'what' must be the name of a day in french or",
          "english or a decimal number between 1-7 (Monday is 1)"
        ), 
        call. = FALSE
      )
    }
  }
  previous <- seq.Date(from = date - 6, to = date, by = "day")
  previous[format(previous, format = "%u") == what]
}



