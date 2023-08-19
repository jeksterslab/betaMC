#' Data Process - `data/nas1982.rda`
#'
DataProcessNAS1982 <- function(overwrite = FALSE) {
  root <- rprojroot::is_rstudio_project
  data_folder <- root$find_file(
    "data"
  )
  if (!dir.exists(data_folder)) {
    dir.create(
      data_folder,
      recursive = TRUE
    )
  }
  nas1982_rda <- file.path(
    data_folder,
    "nas1982.rda"
  )
  if (!file.exists(nas1982_rda)) {
    write <- TRUE
  } else {
    if (overwrite) {
      write <- TRUE
    } else {
      write <- FALSE
    }
  }
  if (write) {
    nas1982 <- read.csv(
      root$find_file(
        ".setup",
        "data-raw",
        "nas1982.txt"
      )
    )
    save(
      nas1982,
      file = root$find_file(
        "data",
        "nas1982.rda"
      ),
      compress = FALSE
    )
  }
}
DataProcessNAS1982(overwrite = TRUE)
rm(DataProcessNAS1982)
