#' Create ID Lookup Table
#'
#' This function creates a lookup dataframe where one column is the original key of an input dataframe and the other is the replacement ID
#' @param dataset The input data.frame that contains a column which is the key (unique identifier)
#' @param keycol_name A string that is the name of the column that the unique id is located in
#' @param digits An integer number of digits of the replacement id. The default is 7 to allow for enough entries. Note that increasing the argument too much will result in a large amount of memory used to internally store the range of all possible values. 
#' @param seed The random seed used to generate the dataset. This can be used to reconstruct the id mapping at a later stage. 
#' @return A data.frame that contains 2 columns named original.ID and replacement.ID
#' @export

create_lookup <- function(dataset, keycol_name, digits = 7, seed = 123){
  # Set the seed
  set.seed(seed)
  
  # Check to see if there are enough digits to create enough unique IDs
  if((10**digits-1)-(10**(digits-1)) < length(unique(dataset[[keycol_name]]))){
    stop("There are insufficient digits to create a unique ID for every entity. Please increase the digits argument")
  }
  # Sample so that each unique entry gets its own ID
  sample.vec <- sample(x = (10**(digits-1)):(10**digits-1), size = length(unique(dataset[[keycol_name]])), replace = FALSE)
  
  # Check to see if there are any conflicting names
  if(("original.ID" %in% names(dataset))|("replacement.ID" %in% names(dataset))){
    warning("One of your columns is named original.ID or replacement.ID, which are used by this function. Function execution was not halted; to avoid ambiguities, it is recommended that you rename the original columns and try again")
  }
  # Return a dataframe that contains 1 column with the original ID and 1 with the new unique id
  data.frame(original.ID= unique(dataset[[keycol_name]]), replacement.ID = sample.vec, stringsAsFactors = FALSE)
}