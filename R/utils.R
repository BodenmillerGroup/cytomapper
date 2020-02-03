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
          definition =  function(x){
            if(length(dim(x[[1]])) == 2){
              return(NULL)
            } else {
              return(dimnames(x[[1]])[[3]])
            }
          })

setReplaceMethod("channelNames",
                 signature = signature(x="ImageList"),
                 definition = function(x, value){
                   # Image needs to be expanded to store channel names
                   if(length(dim(x[[1]])) == 2){
                     x <- endoapply(x, function(y){
                       y <- Image(y, dim = c(dim(y)[1], dim(y)[2], 1))
                     })
                   }
                   x <- endoapply(x, function(y){
                     dimnames(y)[[3]] <- value
                   })
                   return(x)
                 })

#setMethod("getChannels",
#          signature = signature(x="ImageList"),
#          definition = function(x){dimnames(x[[1]])[[3]]})



