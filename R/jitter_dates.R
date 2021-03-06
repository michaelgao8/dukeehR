#' Jitter Dates
#'
#' This function creates a lookup dataframe where one column is the original key of an input dataframe and the other is a random number of days (by patient)
#' to jitter the dates. Assumes that all date columns have already been converted to lubridate objects
#' @param dataset The input data.frame that contains a column which is the key (unique identifier)
#' @param keycol_name A string that is the name of the column that the unique id is located in dataset
#' @param datecols A character vector that contains all date columns that need to be jittered
#' @param direction Should the function jitter dates forward or backward? Defaults to "backward"
#' @param lookup lookup is a 2-column dataframe with the first column being the key and the second column being the number of days to jitter. Usually the output from create_jitter_dt
#' @return The original dataframe with all dates jittered
#' @import lubridate
#' @export

jitter_dates <-
  function(dataset, keycol_name, datecols, direction = "backward", lookup){


    # Convert Dates -----------------------------------------------------
    # TODO: change this to an actual boolean
    # isdate <- numeric(length(date_cols))
    # check_date <- function(x){
    #   any(c(is.Date(x), is.POSIXt(x)))
    # }
    #
    # for (i in seq_along(1:length(date_cols))){
    #   isdate[i] <- check_date(dataset[[date_cols[i]]])
    # }
    #
    # # isdate now contains a boolean for whether or not each column specified in date_cols is a date already or not
    # # if it is, we can skip it
    #
    # # convert
    # for (j in seq_along(1:length(date_cols))) {
    #   if (isdate[j]) {
    #     # If this is true, skip
    #     next
    #   }
    #   else{
    #     lubr_func <- match.fun(tolower(names(date_cols)[j]))
    #     dataset[[date_cols[[j]]]] <- lubr_func(dataset[[date_cols[[j]]]])
    #   }
    # }
    #
    # convert lookup to a named numeric vector
    # datestojitter <- c(lookup[, 2])
    # names(datestojitter) <- as.character(lookup[,1])
    # if(direction=="backward"){
    #   for (k in seq_along(1:length(date_cols))) {
    #     for (l in seq_along(1:nrow(dataset))) {
    #       dataset[[date_cols[k]]][l] <- dataset[[date_cols[k]]][l] - ddays(datestojitter[[dataset[[keycol_name]][[l]]]])
    #
    #     }
    #   }
    # }
    #
    # if (direction == "forward") {
    #   for (k in seq_along(1:length(date_cols))) {
    #     for (l in seq_along(1:nrow(dataset))) {
    #       dataset[[date_cols[k]]][l] <-
    #         dataset[[date_cols[k]]][l] + ddays(datestojitter[[dataset[[keycol_name]][[l]]]])
    #     }
    #   }
    # }
    # return(dataset)

    if (direction == "forward"|direction == "forwards"){
      # join by
      jby <- c("jdt_key")
      names(jby) <- keycol_name
      # Join the dataset
      dataset <- dplyr::left_join(dataset, lookup, by = jby)
      for (col in datecols){
        dataset[[col]] <- dataset[[col]] + ddays(dataset[["date_jittered"]])
      }
      # drop the last column
      dataset[["date_jittered"]] <- NULL
    }

    if (direction == "backward"|direction == "backwards"){
      # join by
      jby <- c("jdt_key")
      names(jby) <- keycol_name
      # join dataset

      dataset <- dplyr::left_join(dataset, lookup, by = jby)
      for (col in datecols){
        dataset[[col]] <- dataset[[col]] - ddays(dataset[["date_jittered"]])
      }
      # drop the last column
      dataset[["date_jittered"]] <- NULL
    }

    return(dataset)
  }


