#' pointfill
#' 
#' Fill missing values in a climate dataset based in linear interpolation
#' 
#' @param tofill Dataframe with missing values to be filled. Can also be a list as produced
#' by this function, which will force station naming to TRUE.
#' @param filler Dataframe that will be used in filling missing values
#' @param station Should a dataframe of source stations be included with the output? T/F
#' @param station.names Vector of length 2 containing: [1] the name of the station to fill 
#' (ignored if tofill is a list, but must be present), & [2] the name of the station used 
#' for filling
#' 

pointfill <- function(tofill = NULL, filler = NULL, station = FALSE, station.names = NULL){
  
  # input checks
  if(is.null(tofill) | is.null(filler)){
    stop("please provide data from 2 stations")
  }
  if(station == TRUE & is.null(station.names)){
    cat("using placeholders for station names")
  }
  if(class(tofill)[1] != "list"){
    lflag = FALSE
  } else {
    out.s <- tofill[[2]]
    tofill <- tofill[[1]]
    lflag = TRUE
    station = TRUE
  }
  
  # get rownames of climate data
  ymax <- max(max(as.numeric(rownames(filler)), max(as.numeric(rownames(tofill)))))
  ymin <- min(min(as.numeric(rownames(filler)), min(as.numeric(rownames(tofill)))))
  yset <- ymin:ymax
  
  # create output matrix
  out <- as.data.frame(matrix(nrow = nrow(tofill), ncol = ncol(tofill)))
  rownames(out) <- rownames(tofill)
  colnames(out) <- colnames(tofill)
  
  # match series lengths
  if(nrow(tofill) != nrow(filler)){
    if(nrow(tofill) > nrow(filler)){
      diff = matrix(data = rep(NA, times = ((nrow(tofill) - 
                                               nrow(filler))) * 
                                 ncol(tofill)),
                    ncol = ncol(tofill))
      colnames(diff) <- colnames(tofill)
      filler <- rbind(diff, filler)
    }
    if(nrow(tofill) < nrow(filler)){
      diff = matrix(data = rep(NA, times = ((nrow(filler) - 
                                               nrow(tofill))) * 
                                 ncol(tofill)), 
                    ncol = ncol(tofill))
      colnames(diff) <- colnames(tofill)
      tofill <- rbind(diff, tofill)
    }
  }
  
  # loop through columns
  for(i in 1:ncol(tofill)){
    # fit linear model to corresponding columns in tofill and filler
    m <- lm(filler ~ tofill, data = data.frame(tofill = tofill[[i]],
                                               filler = filler[[i]]))
    # loop through rows in each column
    for(j in 1:length(tofill[[i]])){
      if(is.na(tofill[j,i]) & !is.na(filler[j,i])){
        # fill using interpolation if tofill is NA and filler is not NA
        out[j,i] <- (coefficients(m)[2]*filler[j,i])+coefficients(m)[1]
      } else {
        # fill with original value of conditions are not met
        out[j,i] <- tofill[j,i]
      }
    }
  }
  out <- round(out, 1)
  
  # set rownames of climate data
  try(rownames(out) <- yset, silent = TRUE)
  
  # create station labels
  if(isTRUE(station)){
    
    if(is.null(station.names)) { station.names = c("tofill", "filler") }
    
    if(!isTRUE(lflag)){
      
      out.s <- as.data.frame(ifelse(test = is.na(tofill), station.names[2], station.names[1]))
      for(i in 1:ncol(out.s)){
        for(j in 1:nrow(out.s)){
          if(isTRUE(is.na(out[j,i]))) { out.s[j,i] <- NA }
        }
      }
      out <- list(values = out, stations = out.s)
      
    } else {
      
      out.s <- sapply(out.s, FUN = as.character)
      for(i in 1:ncol(tofill)){
        for(j in 1:nrow(tofill)){
          if(isTRUE(is.na(tofill[j,i]))) { out.s[j,i] <- station.names[2] }
        }
      }
      for(i in 1:ncol(out.s)){
        for(j in 1:nrow(out.s)){
          if(isTRUE(is.na(out[j,i]))) { out.s[j,i] <- NA }
        }
      }
      out.s <- as.data.frame(out.s)
      out <- list(values = out, stations = out.s)
      
    }
  }
  
  return(out)
}