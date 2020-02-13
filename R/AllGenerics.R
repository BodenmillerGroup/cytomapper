#------------------------------------------------------------------------------
# Generic functions for the ImageList and Image classes
#------------------------------------------------------------------------------

#' @export
setGeneric("channelNames", function(x) standardGeneric("channelNames") )

#' @export
setGeneric("channelNames<-",
           function(x, value) standardGeneric("channelNames<-"))

#' @export
setGeneric("getImages",
           function(x, i) standardGeneric("getImages"))

#' @export
setGeneric("setImages<-",
           function(x, i, value) standardGeneric("setImages<-"))

#' @export
setGeneric("getChannels",
           function(x, i) standardGeneric("getChannels"))

#' @export
setGeneric("setChannels<-",
           function(x, i, value) standardGeneric("setChannels<-"))




