# Set seed
set.seed(seed)

# First convert all of the dates if they are not already of class Date
isdate <- numeric(length(date_cols))
check_date <- function(x){any(c(is.Date(x), is.POSIXt(x)))}
for(i in seq_along(1:length(date_cols))){
  isdate[i] <- check_date(dataset[[date_cols[i]]])
}

# isdate now contains a boolean for whether or not each column specified in date_cols is a date already or not
# if it is, we can skip it

# Next, convert to dates
for(j in seq_along(1:length(date_cols))){
  if(isdate[j]){
    # If this is true, skip
    next
  }
  else{
    lubr_func <- match.fun(tolower(names(date_cols)[j]))
    dataset[[date_cols[j]]] <- lubr_func(dataset[[date_cols[j]]])
  }
}


}

#' @param direction Should the function jitter dates forward or backward or both? Accepts a string with "forward", "backward", or "both". Defaults to "both"

