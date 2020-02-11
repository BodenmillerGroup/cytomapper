#------------------------------------------------------------------------------
# Generic functions for the ImageList and Image classes
#------------------------------------------------------------------------------

#' @export
#' @rdname channelNames
setGeneric("channelNames", function(x) standardGeneric("channelNames") )

#' @export
#' @rdname channelNames
setGeneric("channelNames<-",
           function(x, value) standardGeneric("channelNames<-"))

#' @export
#' @rdname ImageList-subsetting
setGeneric("getImages",
           function(x, i) standardGeneric("getImages"))

#' @export
#' @rdname ImageList-subsetting
setGeneric("setImages<-",
           function(x, i, value) standardGeneric("setImages<-"))

#' @export
#' @rdname ImageList-subsetting
setGeneric("getChannels",
           function(x, i) standardGeneric("getChannels"))

#' @export
#' @rdname ImageList-subsetting
setGeneric("setChannels<-",
           function(x, i, value) standardGeneric("setChannels<-"))




