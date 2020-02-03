# Utility functions for ImageList and Image class objects

setMethod("channelNames",
          signature = signature(x="Image"),
          definition = function(x){
            if(length(dim(x)) == 2){
              return(NULL)
            } else {
              return(dimnames(x)[[3]])
            }
            })

setReplaceMethod("channelNames",
          signature = signature(x="Image"),
          definition = function(x, value){
            # Image needs to be expanded to store channel names
            if(length(dim(x)) == 2){
              x <- Image(x, dim = c(dim(x)[1], dim(x)[2], 1))
            }

            dimnames(x)[[3]] <- value
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



