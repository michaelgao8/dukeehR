#' Create ID Lookup Table
#'
#' This function creates a lookup dataframe where one column is the original key of an input dataframe and the other is the replacement ID
#' @param key A vector of all IDs that need to be replaced.
#' @param digits An integer number of digits of the replacement id. The default is 7 to allow for enough entries. Note that increasing the argument too much will result in a large amount of memory used to internally store the range of all possible values.
#' @param seed The random seed used to generate the dataset. This can be used to reconstruct the id mapping at a later stage.
#' @return A data.frame that contains 2 columns named original.ID and replacement.ID
#' @export

create_lookup <- function(key, digits = 7, seed = 123){
  # Set the seed
  set.seed(seed)

  # Check to see if there are enough digits to create enough unique IDs
  if((10**digits-1)-(10**(digits-1)) < length(unique(key))){
    stop("There are insufficient digits to create a unique ID for every entity. Please increase the digits argument")
  }
  # Sample so that each unique entry gets its own ID
  sample.vec <- sample(x = (10**(digits-1)):(10**digits-1), size = length(unique(key)), replace = FALSE)

  # Return a dataframe that contains 1 column with the original ID and 1 with the new unique id
  data.frame(original.ID= unique(key), replacement.ID = sample.vec, stringsAsFactors = FALSE)
}
