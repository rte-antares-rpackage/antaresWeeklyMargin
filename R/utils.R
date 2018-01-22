
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


select_file <- function(path, pattern = "Hydrauliques", fileext = "\\.xml$") {
  if (dir.exists(path)) {
    path <- list.files(path = path, pattern = fileext, full.names = TRUE)
    path <- grep(pattern = pattern, x = path, value = TRUE)
    if (length(path) < 1)
      stop("No file found : specify complete path.", call. = FALSE)
    path <- sort(path, decreasing = TRUE)[1]
    message(paste("Reading file:", path))
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
  x
}

n_hours <- function(x, y) {
  abs(as.numeric(difftime(time1 = x, time2 = y, units = "hours"))) + 1
}
