#' @name osrmNearest
#' @title Snap Coordinate to Grid
#' @export
osrmNearest <- function(src){
  tryCatch({
    
    # build the query
    req <- paste(getOption("osrm.server"),
                 "nearest/v1/", getOption("osrm.profile"), "/", 
                 src[2], ",", src[3],
                 ".json?number=1",
                 sep=""
    )
    
    # Sending the query
    resRaw <- RCurl::getURL(utils::URLencode(req),
                            useragent = "'osrm' R package")
    # Deal with \\u stuff
    vres <- jsonlite::validate(resRaw)[1]
    if(!vres){
      resRaw <- gsub(pattern = "[\\]", replacement = "zorglub", x = resRaw)
    }
    # Parse the results
    res <- jsonlite::fromJSON(resRaw)
    
    # Error handling
    e <- simpleError(res$message)
    if(res$code != "Ok"){stop(e)}
    
    new_lon <- res$waypoints$location[[1]][1]
    new_lat <- res$waypoints$location[[1]][2]
    
    new_src <- src
    new_src$lon <- new_lon
    new_src$lat <- new_lat
    
    return(new_src)
  }, error=function(e) {message("osrmNearest function returns an error: \n", e)})
  return(NULL)
}
