# Utility functions for ImageList and Image class objects

setMethod("channelNames",
          signature = signature(x="Image"),
          definition = function(x){dimnames(x)[[3]]})

setReplaceMethod("channelNames",
          signature = signature(x="Image"),
          definition = function(x, value){
            # Add checks for names
            if(!is.character(value)){
              stop("Only character entris allowed as channel names.")
            }
            if(length(value) != dim(x)[3]){
              stop("Only character entris allowed as channel names.")
            }


            dimnames(x)[[3]] <- as.character(value)
            return(x)
          })

setMethod("channelNames",
          signature = signature(x="ImageList"),
          definition = function(x){dimnames(x[[1]])[[3]]})

setReplaceMethod("channelNames",
                 signature = signature(x="ImageList"),
                 definition = function(x, value){
                   # Add checks for names
                 })

setMethod("getChannels",
          signature = signature(x="ImageList"),
          definition = function(x){dimnames(x[[1]])[[3]]})



