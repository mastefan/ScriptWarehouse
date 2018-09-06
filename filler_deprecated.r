#' Function for filling gaps in point climate data.
#' Can fill data, and label source station for each data point.
#' 
#' @param df1 dataframe containing data from station to be filled
#' @param df2 dataframe containing data from station to use for filling
#' @param stn1 the station name of df1. e.g. "Kingston"
#' @param stn2 the station name of df2.
#' @param label logical, default = FALSE. 
#' If TRUE will output data labels instead of data.

filler <- function(df1 = NULL, df2 = NULL, stn1 = NULL, stn2 = NULL, label = FALSE){
 
   # if the dimensions of df1 and df2 don't match, fill rows in df2 to enforce a match
  if(!identical(dim(df1), dim(df2))){
    if(dim(df1)[1] > dim(df2)[1]){
      fill <- matrix(nrow = dim(df1)[1] - dim(df2)[1],
                     ncol = dim(df1)[2])
      colnames(fill) <- colnames(df2)
      df2 <- rbind(fill, df2)
    }
    if(dim(df1)[1] < dim(df2)[1]){
      fill <- matrix(nrow = dim(df2)[1] - dim(df1)[1],
                     ncol = dim(df2)[2])
      colnames(fill) <- colnames(df1)
      df1 <- rbind(fill, df1)
    }
  }
  
  # get dimensions and names
  dimset <- dim(df1)
  nameset <- colnames(df1)
  
  # convert dataframe to vector
  df1 <- unlist(df1)
  df2 <- unlist(df2)
  
  # fill data gaps
  if(label == FALSE){df1 <- ifelse(test = is.na(df1), df2, df1)}
  
  # create station labels
  if(label == TRUE){
    if(is.null(stn1)){
      df1 <- ifelse(test = is.na(df1), stn2, df1)
    } else {
      df1 <- ifelse(test = is.na(df1), stn2, stn1)
    }
  }
  
  # create dataframe
  df1 <- as.data.frame(matrix(data = df1,
                              nrow = dimset[1],
                              ncol = dimset[2]),
                       stringsAsFactors = FALSE)
  colnames(df1) <- nameset
  
  # return output
  return(df1)
}