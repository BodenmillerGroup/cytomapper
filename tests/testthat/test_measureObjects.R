test_that("measureObjects: inputs work", {
    data("pancreasSCE")
    data("pancreasMasks")
    data("pancreasImages")
    
    # Works
    sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb")
    cur_H3 <- c(tapply(pancreasImages[[1]][,,1], pancreasMasks[[1]], mean)[-1],
                tapply(pancreasImages[[2]][,,1], pancreasMasks[[2]], mean)[-1],
                tapply(pancreasImages[[3]][,,1], pancreasMasks[[3]], mean)[-1])
    cur_CD99 <- c(tapply(pancreasImages[[1]][,,2], pancreasMasks[[1]], mean)[-1],
                tapply(pancreasImages[[2]][,,2], pancreasMasks[[2]], mean)[-1],
                tapply(pancreasImages[[3]][,,2], pancreasMasks[[3]], mean)[-1])
    cur_PIN <- c(tapply(pancreasImages[[1]][,,3], pancreasMasks[[1]], mean)[-1],
                tapply(pancreasImages[[2]][,,3], pancreasMasks[[2]], mean)[-1],
                tapply(pancreasImages[[3]][,,3], pancreasMasks[[3]], mean)[-1])
    cur_CD8a <- c(tapply(pancreasImages[[1]][,,4], pancreasMasks[[1]], mean)[-1],
                tapply(pancreasImages[[2]][,,4], pancreasMasks[[2]], mean)[-1],
                tapply(pancreasImages[[3]][,,4], pancreasMasks[[3]], mean)[-1])
    cur_CDH <- c(tapply(pancreasImages[[1]][,,5], pancreasMasks[[1]], mean)[-1],
                tapply(pancreasImages[[2]][,,5], pancreasMasks[[2]], mean)[-1],
                tapply(pancreasImages[[3]][,,5], pancreasMasks[[3]], mean)[-1])
    
    
    sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                          feature_types = "basic")
    sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                          feature_types = c("basic", "shape"))
    sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                          feature_types = c("basic", "shape", "moment"))
    sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                          feature_types = c("basic", "shape", "moment", "haralick"), 
                          haralick_feature = "ent.s1")
    sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb",
                          feature_types = c("basic", "moment"))
    sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb",
                          feature_types = c("basic", "moment"))
    sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb",
                          feature_types = c("basic", "haralick"),
                          haralick_feature = "ent.s1")
    sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb")
    sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb")
        
    # Error
    sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", feature_types = c("basic", "shape", "moment", "haralick"))

 
})

test_that("measureObjects: outputs work", {
    data("pancreasSCE")
    data("pancreasMasks")
    data("pancreasImagesß")
    
    
})


