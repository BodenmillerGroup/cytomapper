test_that("measureObjects: defaults work", {
    data("pancreasSCE")
    data("pancreasMasks")
    data("pancreasImages")
    
    # Works
    expect_silent(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb"))
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
    
    # Intensities
    expect_equal(as.numeric(cur_H3), counts(sce)["H3",])
    expect_equal(as.numeric(cur_CD99), counts(sce)["CD99",])
    expect_equal(as.numeric(cur_PIN), counts(sce)["PIN",])
    expect_equal(as.numeric(cur_CD8a), counts(sce)["CD8a",])
    expect_equal(as.numeric(cur_CDH), counts(sce)["CDH",])
    
    # Other entries
    expect_equal(sce$ImageNb, pancreasSCE$ImageNb)
    expect_equal(sce$object_id, pancreasSCE$CellNb)
    expect_equal(rownames(sce), rownames(pancreasSCE))
    expect_equal(rownames(sce), channelNames(pancreasImages))
    
    # colData entries
    expect_equal(names(colData(sce)), 
                 c("ImageNb", "object_id", "s.area", "s.radius.mean", 
                   "m.cx", "m.cy", "m.majoraxis", "m.eccentricity"))
    area <- c(table(pancreasMasks[[1]])[-1],
                table(pancreasMasks[[2]])[-1],
                table(pancreasMasks[[3]])[-1])
    expect_equal(sce$s.area, as.numeric(area))
    expect_equal(sce$s.radius.mean[20:30], 
                 c(4.636644, 4.764700, 3.166851, 3.637420, 4.538360, 4.423855, 5.031715, 4.556145, 4.854957, 2.911706, 3.895167),
                 tolerance = 0.0001)
    expect_equal(sce$m.cx[20:30], 
                 c(3.776316, 29.804878, 97.243902, 58.478261, 80.092105, 39.873239, 70.260870, 12.092105, 84.920000,  2.588235, 21.271186),
                 tolerance = 0.0001)
    expect_equal(sce$m.cy[20:30], 
                 c(13.71053, 17.71951, 16.12195, 18.56522, 18.36842, 21.67606, 23.11957, 23.78947, 25.02667, 24.50000, 27.27119),
                 tolerance = 0.0001)
    expect_equal(sce$m.majoraxis[20:30], 
                 c(14.651483, 12.792813,  8.718651, 10.983645, 11.665788, 11.333186, 13.358161, 13.151296, 16.216661,  9.080632,  9.137610),
                 tolerance = 0.0001)
    expect_equal(sce$m.eccentricity[20:30], 
                 c(0.8771370, 0.7160534, 0.7079446, 0.8471241, 0.6798998, 0.6019294, 0.7431450, 0.8167983, 0.9096242, 0.8334293, 0.4357446),
                 tolerance = 0.0001)
    
    # Other standard input
    sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                          feature_types = "basic")
    expect_equal(names(colData(sce)), 
                 c("ImageNb", "object_id"))
    sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                          feature_types = c("basic", "shape"))
    expect_equal(names(colData(sce)), 
                 c("ImageNb", "object_id", "s.area", "s.radius.mean"))
    sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                          feature_types = c("basic", "shape", "moment"))
    expect_equal(names(colData(sce)), 
                 c("ImageNb", "object_id", "s.area", "s.radius.mean", 
                   "m.cx", "m.cy", "m.majoraxis", "m.eccentricity"))
    sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                          feature_types = c("basic", "shape", "moment", "haralick"), 
                          haralick_feature = "ent.s1")
    expect_equal(names(colData(sce)), 
                 c("ImageNb", "object_id", "s.area", "s.radius.mean", 
                   "m.cx", "m.cy", "m.majoraxis", "m.eccentricity",
                   "H3.h.ent.s1", "CD99.h.ent.s1", "PIN.h.ent.s1", "CD8a.h.ent.s1", "CDH.h.ent.s1"))
    expect_equal(sce$H3.h.ent.s1[20:30], 
                 c(0.92239897, 0.18210610, 0.00000000, 0.28466019, 0.24327757, 0.28306235, 0.31983314, 0.26878196, 0.05829775, 0.00000000, 0.54267473),
                 tolerance = 0.0001)
    expect_equal(sce$CD99.h.ent.s1[20:30], 
                 c(1.0087171, 0.6879652, 0.0000000, 0.8714867, 0.8615940, 0.6785749, 0.8175975, 0.9346840, 0.7556055, 0.8351088, 0.7464104),
                 tolerance = 0.0001)
    expect_equal(sce$PIN.h.ent.s1[20:30], 
                 c(0.3824506, 0.4245154, 0.7619950, 0.5764059, 0.6587099, 0.5013036, 0.4560755, 0.1652518, 0.7064710, 0.2808083, 0.6687518),
                 tolerance = 0.0001)
    expect_equal(sce$CD8a.h.ent.s1[20:30], 
                 c(0.4807238, 0.3278883, 0.4608923, 0.5163900, 0.4407971, 0.4067465, 0.4099984, 0.5272945, 0.5660128, 0.6652107, 0.4108133),
                 tolerance = 0.0001)
    expect_equal(sce$CDH.h.ent.s1[20:30], 
                 c(0.2464482, 0.9223040, 0.3924199, 0.6494582, 0.7329984, 0.9315071, 0.5924960, 0.7332656, 0.6537887, 0.6607878, 0.7881618),
                 tolerance = 0.0001)
    
    sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb",
                          feature_types = c("basic", "moment"))
    expect_equal(names(colData(sce)), 
                 c("ImageNb", "object_id", 
                   "m.cx", "m.cy", "m.majoraxis", "m.eccentricity"))
    sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb",
                          feature_types = c("moment", "basic"))
    expect_equal(names(colData(sce)), 
                 c("ImageNb", "object_id", 
                   "m.cx", "m.cy", "m.majoraxis", "m.eccentricity"))
    sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb",
                          feature_types = c("basic", "haralick"),
                          haralick_feature = "ent.s1")
    expect_equal(names(colData(sce)), 
                 c("ImageNb", "object_id",
                   "H3.h.ent.s1", "CD99.h.ent.s1", "PIN.h.ent.s1", "CD8a.h.ent.s1", "CDH.h.ent.s1"))
    
    # Parallelisable
    expect_silent(sce.1 <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb",
                                        BPPARAM = MulticoreParam()))
    expect_silent(sce.2 <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb",
                                          BPPARAM = SerialParam()))
    expect_equal(sce.1, sce.2)
    expect_equal(as.numeric(cur_H3), counts(sce.1)["H3",])
        
    # Error
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic", "shape", "moment", "haralick")),
                 regexp = "Specify at least one haralick feature.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("test", "moment")),
                 regexp = "Only features of type 'basic', 'shape', 'moment' and 'haralick' are allowed.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("shape", "moment")),
                 regexp = "Please specify a basic feature to characterise the marker expression per cell.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic"), basic_feature = NULL),
                 regexp = "Please specify a basic feature to characterise the marker expression per cell.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic"), basic_feature = c("mean", "sd")),
                 regexp = "Only one intensity feature can be used to characterise the expression of each marker in each object.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic"), basic_quantiles = "test"),
                 regexp = "Only numeric quantiles between 0 and 1 allowed.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic"), basic_feature = "test"),
                 regexp = "Only basic features of type 'mean', 'sd', 'mad' or the selected quantiles allowed.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic"), basic_quantiles = 0.2, basic_feature = "q01"),
                 regexp = "Only basic features of type 'mean', 'sd', 'mad' or the selected quantiles allowed.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic", "shape"), shape_feature = NULL),
                 regexp = "Specify at least one shape feature.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic", "shape"), shape_feature = "test"),
                 regexp = "Only shape features of type 'area', 'perimeter', 'radius.mean', 'radius.sd', 'radius.max', 'radius.min' allowed.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic", "shape"), shape_feature = c("area", "test")),
                 regexp = "Only shape features of type 'area', 'perimeter', 'radius.mean', 'radius.sd', 'radius.max', 'radius.min' allowed.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic", "moment"), moment_feature = NULL),
                 regexp = "Specify at least one moment feature.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic", "moment"), moment_feature = "test"),
                 regexp = "Only moment features of type 'cx', 'cy', 'majoraxis', 'eccentricity', 'theta' allowed.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic", "moment"), moment_feature = c("cx", "test")),
                 regexp = "Only moment features of type 'cx', 'cy', 'majoraxis', 'eccentricity', 'theta' allowed.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic", "haralick"), haralick_feature = NULL),
                 regexp = "Specify at least one haralick feature.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic", "haralick"), haralick_feature = "ent.s1",
                                       haralick_nbins = "test"),
                 regexp = "Specify the number of bins into which intensity levels are binned.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic", "haralick"), haralick_feature = "ent.s1",
                                       haralick_nbins = NULL),
                 regexp = "Specify the number of bins into which intensity levels are binned.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic", "haralick"), haralick_feature = "ent.s1",
                                       haralick_nbins = c(1, 2)),
                 regexp = "Specify the number of bins into which intensity levels are binned.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic", "haralick"), haralick_feature = "ent.s1",
                                       haralick_scales = NULL),
                 regexp = "Specify the scale (in pixels) over which the haralick features are computed.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic", "haralick"), haralick_feature = "ent.s1",
                                       haralick_scales = "test"),
                 regexp = "Specify the scale (in pixels) over which the haralick features are computed.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic", "haralick"), haralick_feature = "test"),
                 regexp = "Only haralick features of type asm.s1, con.s1, cor.s1, var.s1, idm.s1, sav.s1, sva.s1, sen.s1, ent.s1, dva.s1, den.s1, f12.s1, f13.s1, asm.s2, con.s2, cor.s2, var.s2, idm.s2, sav.s2, sva.s2, sen.s2, ent.s2, dva.s2, den.s2, f12.s2, f13.s2 allowed.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic", "haralick"), haralick_feature = c("var.s1", "test")),
                 regexp = "Only haralick features of type asm.s1, con.s1, cor.s1, var.s1, idm.s1, sav.s1, sva.s1, sen.s1, ent.s1, dva.s1, den.s1, f12.s1, f13.s1, asm.s2, con.s2, cor.s2, var.s2, idm.s2, sav.s2, sva.s2, sen.s2, ent.s2, dva.s2, den.s2, f12.s2, f13.s2 allowed.",
                 fixed = TRUE)
    expect_error(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", 
                                       feature_types = c("basic", "haralick"), haralick_feature = c("var.s1", "test"), haralick_scales = c(1, 3)),
                 regexp = "Only haralick features of type asm.s1, con.s1, cor.s1, var.s1, idm.s1, sav.s1, sva.s1, sen.s1, ent.s1, dva.s1, den.s1, f12.s1, f13.s1, asm.s3, con.s3, cor.s3, var.s3, idm.s3, sav.s3, sva.s3, sen.s3, ent.s3, dva.s3, den.s3, f12.s3, f13.s3 allowed.",
                 fixed = TRUE)
})

test_that("measureObjects: different settings work", {
    data("pancreasSCE")
    data("pancreasMasks")
    data("pancreasImages")

    # Basic features
    expect_silent(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", basic_feature = "sd"))
    cur_H3 <- c(tapply(pancreasImages[[1]][,,1], pancreasMasks[[1]], sd)[-1],
                tapply(pancreasImages[[2]][,,1], pancreasMasks[[2]], sd)[-1],
                tapply(pancreasImages[[3]][,,1], pancreasMasks[[3]], sd)[-1])
    cur_H3[is.na(cur_H3)] <- 0
    cur_CD99 <- c(tapply(pancreasImages[[1]][,,2], pancreasMasks[[1]], sd)[-1],
                  tapply(pancreasImages[[2]][,,2], pancreasMasks[[2]], sd)[-1],
                  tapply(pancreasImages[[3]][,,2], pancreasMasks[[3]], sd)[-1])
    cur_CD99[is.na(cur_CD99)] <- 0
    cur_PIN <- c(tapply(pancreasImages[[1]][,,3], pancreasMasks[[1]], sd)[-1],
                 tapply(pancreasImages[[2]][,,3], pancreasMasks[[2]], sd)[-1],
                 tapply(pancreasImages[[3]][,,3], pancreasMasks[[3]], sd)[-1])
    cur_PIN[is.na(cur_PIN)] <- 0
    cur_CD8a <- c(tapply(pancreasImages[[1]][,,4], pancreasMasks[[1]], sd)[-1],
                  tapply(pancreasImages[[2]][,,4], pancreasMasks[[2]], sd)[-1],
                  tapply(pancreasImages[[3]][,,4], pancreasMasks[[3]], sd)[-1])
    cur_CD8a[is.na(cur_CD8a)] <- 0
    cur_CDH <- c(tapply(pancreasImages[[1]][,,5], pancreasMasks[[1]], sd)[-1],
                 tapply(pancreasImages[[2]][,,5], pancreasMasks[[2]], sd)[-1],
                 tapply(pancreasImages[[3]][,,5], pancreasMasks[[3]], sd)[-1])
    cur_CDH[is.na(cur_CDH)] <- 0
    
    # SD
    expect_equal(as.numeric(cur_H3), counts(sce)["H3",])
    expect_equal(as.numeric(cur_CD99), counts(sce)["CD99",])
    expect_equal(as.numeric(cur_PIN), counts(sce)["PIN",])
    expect_equal(as.numeric(cur_CD8a), counts(sce)["CD8a",])
    expect_equal(as.numeric(cur_CDH), counts(sce)["CDH",])
    
    expect_silent(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", basic_feature = "mad"))
    cur_H3 <- c(tapply(pancreasImages[[1]][,,1], pancreasMasks[[1]], mad)[-1],
                tapply(pancreasImages[[2]][,,1], pancreasMasks[[2]], mad)[-1],
                tapply(pancreasImages[[3]][,,1], pancreasMasks[[3]], mad)[-1])
    cur_H3[is.na(cur_H3)] <- 0
    cur_CD99 <- c(tapply(pancreasImages[[1]][,,2], pancreasMasks[[1]], mad)[-1],
                  tapply(pancreasImages[[2]][,,2], pancreasMasks[[2]], mad)[-1],
                  tapply(pancreasImages[[3]][,,2], pancreasMasks[[3]], mad)[-1])
    cur_CD99[is.na(cur_CD99)] <- 0
    cur_PIN <- c(tapply(pancreasImages[[1]][,,3], pancreasMasks[[1]], mad)[-1],
                 tapply(pancreasImages[[2]][,,3], pancreasMasks[[2]], mad)[-1],
                 tapply(pancreasImages[[3]][,,3], pancreasMasks[[3]], mad)[-1])
    cur_PIN[is.na(cur_PIN)] <- 0
    cur_CD8a <- c(tapply(pancreasImages[[1]][,,4], pancreasMasks[[1]], mad)[-1],
                  tapply(pancreasImages[[2]][,,4], pancreasMasks[[2]], mad)[-1],
                  tapply(pancreasImages[[3]][,,4], pancreasMasks[[3]], mad)[-1])
    cur_CD8a[is.na(cur_CD8a)] <- 0
    cur_CDH <- c(tapply(pancreasImages[[1]][,,5], pancreasMasks[[1]], mad)[-1],
                 tapply(pancreasImages[[2]][,,5], pancreasMasks[[2]], mad)[-1],
                 tapply(pancreasImages[[3]][,,5], pancreasMasks[[3]], mad)[-1])
    cur_CDH[is.na(cur_CDH)] <- 0
    
    # MAD
    expect_equal(as.numeric(cur_H3), counts(sce)["H3",])
    expect_equal(as.numeric(cur_CD99), counts(sce)["CD99",])
    expect_equal(as.numeric(cur_PIN), counts(sce)["PIN",])
    expect_equal(as.numeric(cur_CD8a), counts(sce)["CD8a",])
    expect_equal(as.numeric(cur_CDH), counts(sce)["CDH",])
    
    expect_silent(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", basic_feature = "mad"))
    cur_H3 <- c(tapply(pancreasImages[[1]][,,1], pancreasMasks[[1]], mad)[-1],
                tapply(pancreasImages[[2]][,,1], pancreasMasks[[2]], mad)[-1],
                tapply(pancreasImages[[3]][,,1], pancreasMasks[[3]], mad)[-1])
    cur_H3[is.na(cur_H3)] <- 0
    cur_CD99 <- c(tapply(pancreasImages[[1]][,,2], pancreasMasks[[1]], mad)[-1],
                  tapply(pancreasImages[[2]][,,2], pancreasMasks[[2]], mad)[-1],
                  tapply(pancreasImages[[3]][,,2], pancreasMasks[[3]], mad)[-1])
    cur_CD99[is.na(cur_CD99)] <- 0
    cur_PIN <- c(tapply(pancreasImages[[1]][,,3], pancreasMasks[[1]], mad)[-1],
                 tapply(pancreasImages[[2]][,,3], pancreasMasks[[2]], mad)[-1],
                 tapply(pancreasImages[[3]][,,3], pancreasMasks[[3]], mad)[-1])
    cur_PIN[is.na(cur_PIN)] <- 0
    cur_CD8a <- c(tapply(pancreasImages[[1]][,,4], pancreasMasks[[1]], mad)[-1],
                  tapply(pancreasImages[[2]][,,4], pancreasMasks[[2]], mad)[-1],
                  tapply(pancreasImages[[3]][,,4], pancreasMasks[[3]], mad)[-1])
    cur_CD8a[is.na(cur_CD8a)] <- 0
    cur_CDH <- c(tapply(pancreasImages[[1]][,,5], pancreasMasks[[1]], mad)[-1],
                 tapply(pancreasImages[[2]][,,5], pancreasMasks[[2]], mad)[-1],
                 tapply(pancreasImages[[3]][,,5], pancreasMasks[[3]], mad)[-1])
    cur_CDH[is.na(cur_CDH)] <- 0
    
    # MAD
    expect_equal(as.numeric(cur_H3), counts(sce)["H3",])
    expect_equal(as.numeric(cur_CD99), counts(sce)["CD99",])
    expect_equal(as.numeric(cur_PIN), counts(sce)["PIN",])
    expect_equal(as.numeric(cur_CD8a), counts(sce)["CD8a",])
    expect_equal(as.numeric(cur_CDH), counts(sce)["CDH",])
    
    expect_silent(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", basic_feature = "q05", basic_quantiles = 0.5))
    cur_H3 <- c(tapply(pancreasImages[[1]][,,1], pancreasMasks[[1]], median)[-1],
                tapply(pancreasImages[[2]][,,1], pancreasMasks[[2]], median)[-1],
                tapply(pancreasImages[[3]][,,1], pancreasMasks[[3]], median)[-1])
    cur_CD99 <- c(tapply(pancreasImages[[1]][,,2], pancreasMasks[[1]], median)[-1],
                  tapply(pancreasImages[[2]][,,2], pancreasMasks[[2]], median)[-1],
                  tapply(pancreasImages[[3]][,,2], pancreasMasks[[3]], median)[-1])
    cur_PIN <- c(tapply(pancreasImages[[1]][,,3], pancreasMasks[[1]], median)[-1],
                 tapply(pancreasImages[[2]][,,3], pancreasMasks[[2]], median)[-1],
                 tapply(pancreasImages[[3]][,,3], pancreasMasks[[3]], median)[-1])
    cur_CD8a <- c(tapply(pancreasImages[[1]][,,4], pancreasMasks[[1]], median)[-1],
                  tapply(pancreasImages[[2]][,,4], pancreasMasks[[2]], median)[-1],
                  tapply(pancreasImages[[3]][,,4], pancreasMasks[[3]], median)[-1])
    cur_CDH <- c(tapply(pancreasImages[[1]][,,5], pancreasMasks[[1]], median)[-1],
                 tapply(pancreasImages[[2]][,,5], pancreasMasks[[2]], median)[-1],
                 tapply(pancreasImages[[3]][,,5], pancreasMasks[[3]], median)[-1])
    
    # Median
    expect_equal(as.numeric(cur_H3), counts(sce)["H3",])
    expect_equal(as.numeric(cur_CD99), counts(sce)["CD99",])
    expect_equal(as.numeric(cur_PIN), counts(sce)["PIN",])
    expect_equal(as.numeric(cur_CD8a), counts(sce)["CD8a",])
    expect_equal(as.numeric(cur_CDH), counts(sce)["CDH",])
    
    # Shape features
    expect_silent(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", feature_types = c("basic", "shape"), 
                                        shape_feature = c('area', 'perimeter', 'radius.mean', 'radius.sd', 'radius.max', 'radius.min')))
    expect_equal(names(colData(sce)), c("ImageNb", "object_id", "s.area", "s.perimeter", "s.radius.mean", "s.radius.sd", "s.radius.min", "s.radius.max"))
    
    expect_equal(sce$s.perimeter[40:50], c(28, 39, 25, 10, 16, 15, 13, 22, 31, 37, 5))
    expect_equal(sce$s.radius.sd[40:50], c(0.6381883, 1.2634757, 0.5599216, 0.5707716, 0.6687795, 0.4334581, 0.7503223, 0.4679423, 0.7259625, 0.8618819, 0.2850134), tolerance = 0.00001)
    expect_equal(sce$s.radius.min[40:50], c(3.5959870, 3.1786599, 2.9772471, 0.7810250, 1.4710753, 1.2292726, 0.8770580, 2.8477191, 3.2325735, 3.9868300, 0.4472136), tolerance = 0.00001)
    expect_equal(sce$s.radius.max[40:50], c(6.123933, 8.641064, 5.209990, 2.570992, 3.628231, 2.996294, 3.328201, 4.788286, 6.002514, 7.190662, 1.264911), tolerance = 0.00001)

    # Moment features
    expect_silent(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", feature_types = c("basic", "moment"), 
                                        moment_feature = c('cx', 'cy', 'majoraxis', 'eccentricity', 'theta')))
    expect_equal(names(colData(sce)), c("ImageNb", "object_id", "m.cx", "m.cy", "m.majoraxis", "m.eccentricity", "m.theta"))
    
    expect_equal(sce$m.theta[40:50], c(0.60960841, -0.04246836, -0.68045282, 1.57079633, 0.89866183, -1.11289559, -1.10737273, -0.96767979, 1.53796956, -0.09403954, -1.24904577))
    
    # Haralick features
    expect_silent(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", feature_types = c("basic", "haralick"), 
                                        haralick_feature = c("asm.s1", "con.s1", "cor.s1", "var.s1", "idm.s1", "sav.s1", "sva.s1", "sen.s1", "ent.s1", "dva.s1", "den.s1", "f12.s1", "f13.s1", "asm.s2", "con.s2", "cor.s2", "var.s2", "idm.s2", "sav.s2", "sva.s2", "sen.s2", "ent.s2", "dva.s2", "den.s2", "f12.s2", "f13.s2")))
    
    features <- c("asm.s1", "con.s1", "cor.s1", "var.s1", "idm.s1", "sav.s1", "sva.s1", "sen.s1", "ent.s1", "dva.s1", "den.s1", "f12.s1", "f13.s1", "asm.s2", "con.s2", "cor.s2", "var.s2", "idm.s2", "sav.s2", "sva.s2", "sen.s2", "ent.s2", "dva.s2", "den.s2", "f12.s2", "f13.s2")
    features <- paste0("h.", features)
    features <- paste0(rep(channelNames(pancreasImages), each = length(features)), ".", features)
    features <- c("ImageNb", "object_id", features)
    
    expect_equal(names(colData(sce)), features)
    
    cur_basic <- computeFeatures.basic(pancreasMasks[[1]], pancreasImages[[1]][,,1])    
    cur_haralick <- computeFeatures.haralick(pancreasMasks[[1]], pancreasImages[[1]][,,1])
    cur_haralick <- cur_haralick[as.numeric(rownames(cur_basic)),]
    colnames(cur_haralick) <- paste0("H3.", colnames(cur_haralick))
    
    expect_equal(as.matrix(colData(sce)[sce$ImageNb == 1,colnames(cur_haralick)]), cur_haralick)
    
    cur_haralick <- computeFeatures.haralick(pancreasMasks[[1]], pancreasImages[[1]][,,2])
    cur_haralick <- cur_haralick[as.numeric(rownames(cur_basic)),]
    colnames(cur_haralick) <- paste0("CD99.", colnames(cur_haralick))
    
    expect_equal(as.matrix(colData(sce)[sce$ImageNb == 1,colnames(cur_haralick)]), cur_haralick)
    
    expect_silent(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", feature_types = c("basic", "haralick"),  haralick_scales = c(1,4),
                                        haralick_feature = c("asm.s1", "con.s1", "cor.s1", "var.s1", "idm.s1", "sav.s1", "sva.s1", "sen.s1", "ent.s1", "dva.s1", "den.s1", "f12.s1", "f13.s1", "asm.s4", "con.s4", "cor.s4", "var.s4", "idm.s4", "sav.s4", "sva.s4", "sen.s4", "ent.s4", "dva.s4", "den.s4", "f12.s4", "f13.s4")))
    
    features <- c("asm.s1", "con.s1", "cor.s1", "var.s1", "idm.s1", "sav.s1", "sva.s1", "sen.s1", "ent.s1", "dva.s1", "den.s1", "f12.s1", "f13.s1", "asm.s4", "con.s4", "cor.s4", "var.s4", "idm.s4", "sav.s4", "sva.s4", "sen.s4", "ent.s4", "dva.s4", "den.s4", "f12.s4", "f13.s4")
    features <- paste0("h.", features)
    features <- paste0(rep(channelNames(pancreasImages), each = length(features)), ".", features)
    features <- c("ImageNb", "object_id", features)
    
    expect_equal(names(colData(sce)), features)
    
    cur_basic <- computeFeatures.basic(pancreasMasks[[1]], pancreasImages[[1]][,,1])    
    cur_haralick <- computeFeatures.haralick(pancreasMasks[[1]], pancreasImages[[1]][,,1], haralick.scales = c(1,4))
    cur_haralick <- cur_haralick[as.numeric(rownames(cur_basic)),]
    colnames(cur_haralick) <- paste0("H3.", colnames(cur_haralick))
    
    expect_equal(as.matrix(colData(sce)[sce$ImageNb == 1,colnames(cur_haralick)]), cur_haralick)
    
    cur_haralick <- computeFeatures.haralick(pancreasMasks[[1]], pancreasImages[[1]][,,2], haralick.scales = c(1,4))
    cur_haralick <- cur_haralick[as.numeric(rownames(cur_basic)),]
    colnames(cur_haralick) <- paste0("CD99.", colnames(cur_haralick))
    
    expect_equal(as.matrix(colData(sce)[sce$ImageNb == 1,colnames(cur_haralick)]), cur_haralick)
    
    expect_silent(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", feature_types = c("basic", "haralick"), haralick_nbins = 16,
                                        haralick_feature = c("asm.s1", "con.s1", "cor.s1", "var.s1", "idm.s1", "sav.s1", "sva.s1", "sen.s1", "ent.s1", "dva.s1", "den.s1", "f12.s1", "f13.s1", "asm.s2", "con.s2", "cor.s2", "var.s2", "idm.s2", "sav.s2", "sva.s2", "sen.s2", "ent.s2", "dva.s2", "den.s2", "f12.s2", "f13.s2")))
    
    features <- c("asm.s1", "con.s1", "cor.s1", "var.s1", "idm.s1", "sav.s1", "sva.s1", "sen.s1", "ent.s1", "dva.s1", "den.s1", "f12.s1", "f13.s1", "asm.s2", "con.s2", "cor.s2", "var.s2", "idm.s2", "sav.s2", "sva.s2", "sen.s2", "ent.s2", "dva.s2", "den.s2", "f12.s2", "f13.s2")
    features <- paste0("h.", features)
    features <- paste0(rep(channelNames(pancreasImages), each = length(features)), ".", features)
    features <- c("ImageNb", "object_id", features)
    
    expect_equal(names(colData(sce)), features)
    
    cur_basic <- computeFeatures.basic(pancreasMasks[[1]], pancreasImages[[1]][,,1])    
    cur_haralick <- computeFeatures.haralick(pancreasMasks[[1]], pancreasImages[[1]][,,1], haralick.nbins = 16)
    cur_haralick <- cur_haralick[as.numeric(rownames(cur_basic)),]
    colnames(cur_haralick) <- paste0("H3.", colnames(cur_haralick))
    
    expect_equal(as.matrix(colData(sce)[sce$ImageNb == 1,colnames(cur_haralick)]), cur_haralick)
    
    cur_haralick <- computeFeatures.haralick(pancreasMasks[[1]], pancreasImages[[1]][,,2], haralick.nbins = 16)
    cur_haralick <- cur_haralick[as.numeric(rownames(cur_basic)),]
    colnames(cur_haralick) <- paste0("CD99.", colnames(cur_haralick))
    
    expect_equal(as.matrix(colData(sce)[sce$ImageNb == 1,colnames(cur_haralick)]), cur_haralick)
    
    expect_silent(sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb", feature_types = c("basic", "haralick"),
                                        haralick_feature = c("asm.s1", "ent.s2")))
    
    features <- c("asm.s1", "ent.s2")
    features <- paste0("h.", features)
    features <- paste0(rep(channelNames(pancreasImages), each = length(features)), ".", features)
    features <- c("ImageNb", "object_id", features)
    
    expect_equal(names(colData(sce)), features)
    
    cur_basic <- computeFeatures.basic(pancreasMasks[[1]], pancreasImages[[1]][,,1])    
    cur_haralick <- computeFeatures.haralick(pancreasMasks[[1]], pancreasImages[[1]][,,1])
    cur_haralick <- cur_haralick[as.numeric(rownames(cur_basic)),]
    colnames(cur_haralick) <- paste0("H3.", colnames(cur_haralick))
    
    expect_equal(as.matrix(colData(sce)[sce$ImageNb == 1,c("H3.h.asm.s1", "H3.h.ent.s2")]), cur_haralick[,c("H3.h.asm.s1", "H3.h.ent.s2")])
    
    cur_haralick <- computeFeatures.haralick(pancreasMasks[[1]], pancreasImages[[1]][,,2])
    cur_haralick <- cur_haralick[as.numeric(rownames(cur_basic)),]
    colnames(cur_haralick) <- paste0("CD99.", colnames(cur_haralick))
    
    expect_equal(as.matrix(colData(sce)[sce$ImageNb == 1,c("CD99.h.asm.s1", "CD99.h.ent.s2")]), cur_haralick[,c("CD99.h.asm.s1", "CD99.h.ent.s2")])
    
})


