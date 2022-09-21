.onAttach <- function(libname, pkgname) {
    msg <- paste("The 'CytoImageList' data class has now been moved to its",
                 "independent Bioconductor package. To update old 'CytoImageList'",
                 "objects, please calls 'updateObject(object)'.")
    
    msg <- strwrap(msg, exdent=4, indent=4)
    
    packageStartupMessage(paste(msg, collapse="\n"), appendLF=TRUE)
}
