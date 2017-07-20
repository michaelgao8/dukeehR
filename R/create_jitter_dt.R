#' Create Jittered Date Table
#'
#' This function creates a lookup dataframe where one column is the original key of an input dataframe and the other is a random number of days (by patient)
#' to jitter the dates
#' @param key A vector of all IDs that need to be jittered
#' @param max_days An integer representing the largest number of days that the dates can be jittered
#' @param seed The random seed used to generate the dataset. This can be used to reconstruct the id mapping at a later stage.
#' @return A data.frame that contains 2 columns named original.ID and dates to jitter
#' @import lubridate
#' @export

create_jitter_dt <- function(key, max_days = 100, seed = 123){
  # Set seed
  set.seed(seed)

  #assert(max_days > length(unique(dataset[[keycol_name]])))
  jdt <- data.frame(unique(key), sample(1:max_days, size = length(unique(key)), replace = TRUE))
  names(jdt) <- c("jdt_key", "date_jittered")
  jdt
}
