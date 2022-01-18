test_that("compImage function works", {
    data("pancreasImages")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    # Mock up realistic spillover matrix
    sm_real <- matrix(c(1, 0.033, 0.01, 0.007, 0,
                   0.016, 1, 0.051, 0.01, 0,
                   0.004, 0.013, 1, 0.023, 0,
                   0.005, 0.008, 0.029, 1, 0.006,
                   0, 0, 0, 0.001, 1), byrow = TRUE,
                 ncol = 5, nrow = 5, 
                 dimnames = list(c("Dy161Di", "Dy162Di", "Dy163Di", "Dy164Di", "Ho165Di"),
                                 c("Dy161Di", "Dy162Di", "Dy163Di", "Dy164Di", "Ho165Di")))
    
    sm_strong <- matrix(c(1, 0.33, 0.1, 0.07, 0,
                   0.16, 1, 0.51, 0.1, 0,
                   0.04, 0.13, 1, 0.23, 0,
                   0.05, 0.08, 0.29, 1, 0.06,
                   0, 0, 0, 0.01, 1), byrow = TRUE,
                 ncol = 5, nrow = 5, 
                 dimnames = list(c("Dy161Di", "Dy162Di", "Dy163Di", "Dy164Di", "Ho165Di"),
                                 c("Dy161Di", "Dy162Di", "Dy163Di", "Dy164Di", "Ho165Di")))
    
    # Generate spillover images - strong spillover
    sm_images <- pancreasImages
    channelNames(sm_images) <- c("Dy161Di", "Dy162Di", "Dy163Di", "Dy164Di", "Ho165Di")
    cur_ch <- endoapply(sm_images, function(x){
        Image(apply(x, c(1,2), function(y){sum(y * sm_strong[,"Dy161Di"])}))
    })
    channelNames(cur_ch) <- "Dy161Di"
    setChannels(sm_images,  "Dy161Di") <- cur_ch
    
    cur_ch <- endoapply(sm_images, function(x){
        Image(apply(x, c(1,2), function(y){sum(y * sm_strong[,"Dy162Di"])}))
    })
    channelNames(cur_ch) <- "Dy162Di"
    setChannels(sm_images,  "Dy162Di") <- cur_ch
    
    cur_ch <- endoapply(sm_images, function(x){
        Image(apply(x, c(1,2), function(y){sum(y * sm_strong[,"Dy163Di"])}))
    })
    channelNames(cur_ch) <- "Dy163Di"
    setChannels(sm_images,  "Dy163Di") <- cur_ch
    
    cur_ch <- endoapply(sm_images, function(x){
        Image(apply(x, c(1,2), function(y){sum(y * sm_strong[,"Dy164Di"])}))
    })
    channelNames(cur_ch) <- "Dy164Di"
    setChannels(sm_images,  "Dy164Di") <- cur_ch
    
    cur_ch <- endoapply(sm_images, function(x){
        Image(apply(x, c(1,2), function(y){sum(y * sm_strong[,"Ho165Di"])}))
    })
    channelNames(cur_ch) <- "Ho165Di"
    setChannels(sm_images,  "Ho165Di") <- cur_ch
    
    # Visual inspection
    plotPixels(pancreasImages, colour_by = "H3")
    plotPixels(sm_images, colour_by = "Dy161Di")
    plotPixels(pancreasImages, colour_by = "CD99")
    plotPixels(sm_images, colour_by = "Dy162Di")
    plotPixels(pancreasImages, colour_by = "PIN")
    plotPixels(sm_images, colour_by = "Dy163Di")
    plotPixels(pancreasImages, colour_by = "CD8a")
    plotPixels(sm_images, colour_by = "Dy164Di")
    plotPixels(pancreasImages, colour_by = "CDH")
    plotPixels(sm_images, colour_by = "Ho165Di")
    
    expect_silent(cur_out <- compImage(sm_images, sm_strong))
    
    plotPixels(pancreasImages, colour_by = "H3")
    plotPixels(cur_out, colour_by = "Dy161Di")
    plotPixels(pancreasImages, colour_by = "CD99")
    plotPixels(cur_out, colour_by = "Dy162Di")
    plotPixels(pancreasImages, colour_by = "PIN")
    plotPixels(cur_out, colour_by = "Dy163Di")
    plotPixels(pancreasImages, colour_by = "CD8a")
    plotPixels(cur_out, colour_by = "Dy164Di")
    plotPixels(pancreasImages, colour_by = "CDH")
    plotPixels(cur_out, colour_by = "Ho165Di")
    
    expect_s4_class(cur_out, "CytoImageList")
    expect_equal(cur_out@int_metadata, sm_images@int_metadata)
    expect_equal(cur_out@elementType, sm_images@elementType)
    expect_equal(cur_out@elementMetadata, sm_images@elementMetadata)
    expect_equal(cur_out@metadata, sm_images@metadata)
    
    expect_equal(as.vector(as.array(cur_out$E34_imc)[1:5, 1:5, 1:5]),
                 c(2.19091247942997, 2.84356252686191, 3.3673723610464, 3.14572847700658, 
                   0.983622669305094, 0.222899337345514, 1.96485523758383, 0.975483504321522, 
                   3.0913608704726, 1.95984734333247, 1.2249119742494, 2.23030785595094, 
                   0.979925379267491, 1.09383112811161, 0, 0.984049800882084, 0, 
                   2.14700951449616, 4.44512235501792, 0, 1.95984282166521, 1.38952853167984, 
                   0, 0, 0, 2.33215967202339, 0.975802822921462, 0.976676447907313, 
                   4.35585101567448, 0, 2.22006136192933, 0.981105558617738, 1.00600490700374, 
                   4.79370751533438, 0, 2.75078164753138, 0, 0, 2.05644103864334, 
                   1.03150187424537, 0, 0.00524125399300493, 0, 1.08051748935461, 
                   0, 0, 0, 0.0117787905004508, 1.04062409980771, 0.00716183064089961, 
                   0.315055588402477, 0.495516534300958, 0.534568548281539, 0.432929892986981, 
                   0.149952238758362, 0, 0.304031094587488, 0.141009838484414, 0.415572390098853, 
                   0.298786945215061, 0.148287560046762, 0.340029345390463, 0.149403940521257, 
                   0.140104743598137, 0, 0.150029779212584, 0.0265316586792683, 
                   0.32730164973074, 0.709689680636647, 0, 0.298759226552687, 0.211820287223255, 
                   0.0596239391568057, 0.0065178690321914, 0.0362531125956509, 0.463300346038719, 
                   2.21539693219691, 0.349579083510564, 0.805179350618355, 0.0604482747329144, 
                   0.320710650782931, 0.260954614190242, 0.201364068183511, 0.862480359013045, 
                   0.12040521563426, 0.459879033299737, 0.136987121035636, 0.0601662943949444, 
                   0.355198424882429, 0.135970716885409, 0.0604301955082186, 1.07899076860329, 
                   0.131971334957697, 0.432448305042176, 0, 0.120501372281659, 0.0854354711719541, 
                   2.42625619641143, 1.22805737803821, 1.47511103753946, 0.968407615883399, 
                   8.42292868623672, 1.02649529960838, 10.066858921215, 4.64655872894952, 
                   19.6775324902741, 29.3898294072017, 64.7457048612308, 27.4107824928013, 
                   14.9365810323236, 43.3343222889591, 22.2768840994686, 13.1043679869514, 
                   18.3836179718671, 24.866554665624, 11.5088084603371, 9.79035659055431, 
                   5.89805598850556, 1.0225441754596, 1.02304083481394, 0.0124488120807903, 
                   0.00882620758183113, 2.58327387916262, 8.29000698128538, 3.21541266831867
                 ))
    expect_equal(as.vector(as.array(cur_out$G01_imc)[1:5, 1:5, 1:5]),
                 c(42.0433486268618, 24.8502064958084, 24.8565163775208, 23.8671586508943, 
                   14.7721370664372, 28.6796917290198, 11.0073560703932, 35.5807413129532, 
                   13.2198419408514, 17.6203309734446, 31.4676799126701, 7.45659914081183, 
                   34.2190530240665, 25.1107542789657, 4.37200470144071, 18.8829133949791, 
                   27.6000216524927, 15.8781630871283, 10.0466491497996, 9.65224123615268, 
                   22.899059097985, 12.303893872095, 23.2060242753981, 8.77680198517962, 
                   5.89315975153267, 0, 0, 0, 0.619127244662373, 0, 0.348106871518595, 
                   0, 2.10680147303142, 0, 0, 0.833162787045088, 0.86494544216698, 
                   3.35127485532916, 0.426882334812537, 0, 0, 0, 0.642341377953656, 
                   0.792242536829195, 0, 1.52849730269222, 0, 2.58823115117122, 
                   0.820931139788265, 4.93170399573359, 6.40910708649243, 3.78817906460042, 
                   3.78918735727637, 3.91057549150629, 2.25187411374158, 4.70671929783099, 
                   1.67797952314402, 5.80697075963436, 2.0152472586785, 5.8408594004552, 
                   5.15557595240238, 1.20879370192231, 6.62983347429976, 5.1893931619719, 
                   0.697369307145619, 2.87856100654853, 4.20738031548391, 2.59716510881473, 
                   1.63592663015692, 1.47148459297048, 3.73392140851757, 1.87561667178681, 
                   4.87563339745428, 1.45338125990253, 2.00152645281304, 2.58503700740846, 
                   1.52790760087721, 1.52812997871859, 1.60150205735751, 0.90825526154206, 
                   1.86947132164154, 0.676740701176881, 2.5520346900347, 0.812803682167594, 
                   1.13778267264453, 2.11385000387633, 0.593649704324864, 2.656750333753, 
                   1.67195081393407, 1.35262823322088, 1.16086990401864, 1.6969152206842, 
                   1.09741158045001, 0.747772090268841, 0.593138171029043, 1.66642452717102, 
                   0.756485711591332, 3.523064698679, 1.75021391363921, 3.51604880257827, 
                   1.45012215382787, 2.15096184304747, 27.7770251301588, 10.3339944985449, 
                   2.0863925689091, 1.16828396582613, 7.65895629731013, 17.9837249664863, 
                   3.4537407415594, 11.4679524138153, 13.259652431308, 9.29947105385261, 
                   18.5842933598285, 8.06174067801254, 13.4295392824079, 23.3694911214887, 
                   12.2861687101164, 15.8969482208778, 45.265771088098, 51.4460358755549, 
                   12.7150861312118, 3.47399960222705, 34.8979564003661, 44.1063254270049, 
                   35.2114611713016))
    expect_equal(as.vector(as.array(cur_out$J02_imc)[1:5, 1:5, 1:5]),
                 c(2.52784678643665, 3.94732294210906, 0.979922651558483, 1.96261804369919, 
                   0, 2.9364163551472, 0.979922387758177, 0.979429850891626, 0, 
                   2.18914847046554, 1.62612164288276, 0, 0.961153472366437, 4.0652818370046, 
                   3.77489600016522, 4.90353065425084, 0.983612646958199, 0.962254308232961, 
                   0, 0, 1.59825897003124, 1.92768351046888, 0, 1.95900496760123, 
                   3.94999964867716, 1.62368009253514, 1.24238459027986, 0, 1.14642573626392, 
                   0, 0, 0, 1.00567082449753, 0, 0, 2.03014661572905, 0, 2.06713820982705, 
                   0.929736789503776, 0.951016208482324, 0, 0.402927448305584, 2.34125536633104, 
                   1.01079722375315, 0, 0, 6.15442777891983, 1.00184231263536, 0.93038665503675, 
                   1.89779053433036, 0.384113779712536, 0.624755029821887, 0.149387219157034, 
                   0.300422289135498, 0, 0.478544611201744, 0.14938560201203, 0.141554676891216, 
                   0, 0.333739373864907, 0.228071540353203, 0, 0.118080595497426, 
                   0.650174305782268, 0.60204107347724, 0.747518169093167, 0.153922340441413, 
                   0.112954467352711, 0, 0, 0.243682224271373, 0.198025054353821, 
                   0, 0.3307945668948, 0.612578760433627, 0.386179011626153, 0.423332732907551, 
                   0.0602243011739832, 0.28398372778675, 0, 1.26519221435643, 0.0602299110854532, 
                   0.201987690381892, 0, 0.134512149512616, 0.385423862745542, 0, 
                   0.348307989854818, 0.387369654849142, 0.3718823511537, 0.301416227598408, 
                   0.118293397819711, 0.386542660274728, 0.133332889570057, 0, 0.0981159471522133, 
                   0.977870951031461, 0.132198214001544, 1.3332364528813, 0.514476313867775, 
                   5.81644358434911, 2.16234216238375, 4.10134153513466, 0.019278945158285, 
                   1.99819805919856, 10.0543473595612, 3.23064707160147, 0.0121083498717519, 
                   1.99813639102084, 13.6530706285659, 5.81433380729223, 2.06607194023602, 
                   11.0884858960676, 1.01940269858764, 2.02803233913579, 12.171453743601, 
                   26.8125907733524, 1.78907634890066, 8.99556792774407, 22.0620541905847, 
                   23.744224545368, 7.34059062830994, 1.08813304444263, 42.1869673155035, 
                   22.7064987774514))
    
    expect_false(isTRUE(all.equal(imageData(pancreasImages$E34_imc)[,,1], imageData(sm_images$E34_imc)[,,1],
                                  check.attributes = FALSE, tolerance = 0.1)))
    expect_equal(imageData(pancreasImages$E34_imc)[,,1], imageData(cur_out$E34_imc)[,,1],
                 check.attributes = FALSE, tolerance = 0.1)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$E34_imc)[,,2], imageData(sm_images$E34_imc)[,,2],
                                  check.attributes = FALSE, tolerance = 0.05)))
    expect_equal(imageData(pancreasImages$E34_imc)[,,2], imageData(cur_out$E34_imc)[,,2],
                 check.attributes = FALSE, tolerance = 0.05)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$E34_imc)[,,3], imageData(sm_images$E34_imc)[,,3],
                                  check.attributes = FALSE, tolerance = 0.5)))
    expect_equal(imageData(pancreasImages$E34_imc)[,,3], imageData(cur_out$E34_imc)[,,3],
                 check.attributes = FALSE, tolerance = 0.5)
    #expect_false(isTRUE(all.equal(imageData(pancreasImages$E34_imc)[,,4], imageData(sm_images$E34_imc)[,,4],
    #                              check.attributes = FALSE, tolerance = 0.5)))
    #expect_equal(imageData(pancreasImages$E34_imc)[,,4], imageData(cur_out$E34_imc)[,,4],
    #             check.attributes = FALSE, tolerance = 0.5)
    #expect_false(isTRUE(all.equal(imageData(pancreasImages$E34_imc)[,,5], imageData(sm_images$E34_imc)[,,5],
    #                              check.attributes = FALSE, tolerance = 0.001)))
    #expect_equal(imageData(pancreasImages$E34_imc)[,,5], imageData(cur_out$E34_imc)[,,5],
    #             check.attributes = FALSE, tolerance = 0.001)
    
    expect_false(isTRUE(all.equal(imageData(pancreasImages$G01_imc)[,,1], imageData(sm_images$G01_imc)[,,1],
                                  check.attributes = FALSE, tolerance = 0.1)))
    expect_equal(imageData(pancreasImages$G01_imc)[,,1], imageData(cur_out$G01_imc)[,,1],
                 check.attributes = FALSE, tolerance = 0.1)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$G01_imc)[,,2], imageData(sm_images$G01_imc)[,,2],
                                  check.attributes = FALSE, tolerance = 0.1)))
    expect_equal(imageData(pancreasImages$G01_imc)[,,2], imageData(cur_out$G01_imc)[,,2],
                 check.attributes = FALSE, tolerance = 0.1)
    #expect_false(isTRUE(all.equal(imageData(pancreasImages$G01_imc)[,,3], imageData(sm_images$G01_imc)[,,3],
    #                              check.attributes = FALSE, tolerance = 0.5)))
    #expect_equal(imageData(pancreasImages$G01_imc)[,,3], imageData(cur_out$G01_imc)[,,3],
    #             check.attributes = FALSE, tolerance = 0.5)
    #expect_false(isTRUE(all.equal(imageData(pancreasImages$G01_imc)[,,4], imageData(sm_images$G01_imc)[,,4],
    #                              check.attributes = FALSE, tolerance = 0.5)))
    #expect_equal(imageData(pancreasImages$G01_imc)[,,4], imageData(cur_out$G01_imc)[,,4],
    #             check.attributes = FALSE, tolerance = 0.5)
    #expect_false(isTRUE(all.equal(imageData(pancreasImages$G01_imc)[,,5], imageData(sm_images$G01_imc)[,,5],
    #                              check.attributes = FALSE, tolerance = 0.001)))
    #expect_equal(imageData(pancreasImages$G01_imc)[,,5], imageData(cur_out$G01_imc)[,,5],
    #             check.attributes = FALSE, tolerance = 0.001)
    
    expect_false(isTRUE(all.equal(imageData(pancreasImages$J02_imc)[,,1], imageData(sm_images$J02_imc)[,,1],
                                  check.attributes = FALSE, tolerance = 0.1)))
    expect_equal(imageData(pancreasImages$J02_imc)[,,1], imageData(cur_out$J02_imc)[,,1],
                 check.attributes = FALSE, tolerance = 0.1)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$J02_imc)[,,2], imageData(sm_images$J02_imc)[,,2],
                                  check.attributes = FALSE, tolerance = 0.5)))
    expect_equal(imageData(pancreasImages$J02_imc)[,,2], imageData(cur_out$J02_imc)[,,2],
                 check.attributes = FALSE, tolerance = 0.5)
    #expect_false(isTRUE(all.equal(imageData(pancreasImages$J02_imc)[,,3], imageData(sm_images$J02_imc)[,,3],
    #                              check.attributes = FALSE, tolerance = 0.5)))
    #expect_equal(imageData(pancreasImages$J02_imc)[,,3], imageData(cur_out$J02_imc)[,,3],
    #             check.attributes = FALSE, tolerance = 0.5)
    #expect_false(isTRUE(all.equal(imageData(pancreasImages$J02_imc)[,,4], imageData(sm_images$J02_imc)[,,4],
    #                              check.attributes = FALSE, tolerance = 0.5)))
    #expect_equal(imageData(pancreasImages$J02_imc)[,,4], imageData(cur_out$J02_imc)[,,4],
    #             check.attributes = FALSE, tolerance = 0.5)
    #expect_false(isTRUE(all.equal(imageData(pancreasImages$J02_imc)[,,5], imageData(sm_images$J02_imc)[,,5],
    #                              check.attributes = FALSE, tolerance = 0.001)))
    #expect_equal(imageData(pancreasImages$J02_imc)[,,5], imageData(cur_out$J02_imc)[,,5],
    #             check.attributes = FALSE, tolerance = 0.001)
    
    # weak spillover
    sm_images <- pancreasImages
    channelNames(sm_images) <- c("Dy161Di", "Dy162Di", "Dy163Di", "Dy164Di", "Ho165Di")
    cur_ch <- endoapply(sm_images, function(x){
        Image(apply(x, c(1,2), function(y){sum(y * sm_real[,"Dy161Di"])}))
    })
    channelNames(cur_ch) <- "Dy161Di"
    setChannels(sm_images,  "Dy161Di") <- cur_ch
    
    cur_ch <- endoapply(sm_images, function(x){
        Image(apply(x, c(1,2), function(y){sum(y * sm_real[,"Dy162Di"])}))
    })
    channelNames(cur_ch) <- "Dy162Di"
    setChannels(sm_images,  "Dy162Di") <- cur_ch
    
    cur_ch <- endoapply(sm_images, function(x){
        Image(apply(x, c(1,2), function(y){sum(y * sm_real[,"Dy163Di"])}))
    })
    channelNames(cur_ch) <- "Dy163Di"
    setChannels(sm_images,  "Dy163Di") <- cur_ch
    
    cur_ch <- endoapply(sm_images, function(x){
        Image(apply(x, c(1,2), function(y){sum(y * sm_real[,"Dy164Di"])}))
    })
    channelNames(cur_ch) <- "Dy164Di"
    setChannels(sm_images,  "Dy164Di") <- cur_ch
    
    cur_ch <- endoapply(sm_images, function(x){
        Image(apply(x, c(1,2), function(y){sum(y * sm_real[,"Ho165Di"])}))
    })
    channelNames(cur_ch) <- "Ho165Di"
    setChannels(sm_images,  "Ho165Di") <- cur_ch
    
    # Visual inspection
    plotPixels(pancreasImages, colour_by = "H3")
    plotPixels(sm_images, colour_by = "Dy161Di")
    plotPixels(pancreasImages, colour_by = "CD99")
    plotPixels(sm_images, colour_by = "Dy162Di")
    plotPixels(pancreasImages, colour_by = "PIN")
    plotPixels(sm_images, colour_by = "Dy163Di")
    plotPixels(pancreasImages, colour_by = "CD8a")
    plotPixels(sm_images, colour_by = "Dy164Di")
    plotPixels(pancreasImages, colour_by = "CDH")
    plotPixels(sm_images, colour_by = "Ho165Di")
    
    expect_silent(cur_out <- compImage(sm_images, sm_real))
    
    plotPixels(pancreasImages, colour_by = "H3")
    plotPixels(cur_out, colour_by = "Dy161Di")
    plotPixels(pancreasImages, colour_by = "CD99")
    plotPixels(cur_out, colour_by = "Dy162Di")
    plotPixels(pancreasImages, colour_by = "PIN")
    plotPixels(cur_out, colour_by = "Dy163Di")
    plotPixels(pancreasImages, colour_by = "CD8a")
    plotPixels(cur_out, colour_by = "Dy164Di")
    plotPixels(pancreasImages, colour_by = "CDH")
    plotPixels(cur_out, colour_by = "Ho165Di")
    
    expect_equal(as.vector(as.array(cur_out$E34_imc)[1:5, 1:5, 1:5]),
                 c(2.235732088208, 2.8854730513074, 3.4008971202628, 3.22373821331941, 
                   0.99875629977235, 0.253692906479931, 1.9899866179725, 0.994985844055508, 
                   3.17497668451463, 1.98999903490431, 1.26958066348013, 2.26461888137977, 
                   0.994999517625591, 1.12830055125045, 0, 0.999187892764767, 0, 
                   2.18004386171178, 4.48654633170962, 0, 1.98999903444506, 1.41090928609636, 
                   0, 0, 0, 2.27635725972987, 0.983536654362042, 1.01017745844559, 
                   4.22965774864491, 0, 2.12652417304427, 0.981840137820238, 0.982626055121661, 
                   4.64622303248461, 0, 2.65339987458059, 0, 0, 1.98777747278492, 
                   0.998675102237445, 0, 0.000152043848113654, 0, 1.13428804602046, 
                   0, 0, 0, 0.000341872622664964, 0.994300640920651, 0.000207852687758287, 
                   0.00401044534673232, 0.00569775197892415, 0.00579791443161271, 
                   0.00589961298032481, 0.00166447452795089, 0.000685582701755591, 
                   0.00344078386091881, 0.00178105607914726, 0.00586967037376447, 
                   0.00331642833497421, 0.00244548100062844, 0.00377409633100861, 
                   0.00165821514968374, 0.00212745809414315, 0.000123033207785647, 
                   0.00166519499322897, 0.000433320776903636, 0.00363314528274224, 
                   0.00762427538116815, 0, 0.0033164257341525, 0.00235134579664235, 
                   0.000974326774952282, 0.000558344737800897, 0.000592374024752764, 
                   0.00418079540176943, 1.75424265942173, 0.00320579318150167, 0.00724655246304433, 
                   0.000559497268857626, 0.00287675328206037, 0.00237814134379035, 
                   0.00182108288281706, 0.00775479818315889, 0.00111475141155682, 
                   0.00412372257298681, 0.00126855541881744, 0.000557341887479148, 
                   0.00318869498623407, 0.0012839373702388, 0.000559697880852737, 
                   0.994893113635612, 0.00122127291936576, 0.00397407497917223, 
                   0, 0.00111484096120764, 0.00079042222506761, 2.23703644397587, 
                   1.00203308281328, 1.36007926114486, 0.94052069249707, 8.39347037048068, 
                   0.998709574554686, 10.0159472939418, 4.63759651653449, 19.6516501414446, 
                   29.3541799448566, 64.6952676067429, 27.3474321805845, 14.9153508776922, 
                   43.2850985815283, 22.2495882662622, 13.0904049101193, 18.3539986915052, 
                   24.8453837997218, 11.4957671851282, 9.78412878891609, 5.88101486063863, 
                   0.98724778039971, 1.02243357424814, 8.40408149755324e-05, 5.95849365792035e-05, 
                   2.58081283588555, 8.27876255327549, 3.21293932270747))
    expect_equal(as.vector(as.array(cur_out$G01_imc)[1:5, 1:5, 1:5]),
                 c(42.6902716555488, 25.2325774210941, 25.2389765105437, 24.019300011956, 
                   14.9994361653353, 28.8571452110096, 11.1767245488137, 35.8236030829578, 
                   13.4232531878661, 17.8997645591454, 31.668499738626, 7.51323576733715, 
                   34.4749289437561, 25.2721198472437, 4.44267015124073, 19.1734587038404, 
                   28.0247010274348, 15.9825015057999, 10.1178914126346, 9.80074501376742, 
                   23.0577003552362, 12.4932135411493, 23.3953887314381, 8.84735000741003, 
                   6.01468246529686, 0, 0, 0, 1.14485794697834, 0, 0.99795214031261, 
                   0, 2.83640893095649, 0, 0, 1.52554132309874, 0.998481453383544, 
                   3.9887053367071, 0.986623734889973, 3.81026772555567e-05, 0, 
                   0, 0.981553798214287, 0.989221025852425, 0, 1.99016397437819, 
                   0, 2.99289079694945, 0.979366671206552, 4.82178678092487, 0.0711453189051755, 
                   0.042051261503576, 0.0420619303476064, 0.0402095420806812, 0.0249972567454924, 
                   0.0482617648937313, 0.018626531423736, 0.0601098401926974, 0.0223708599363171, 
                   2.97269661995508, 0.053016680681579, 0.0126565542527279, 1.05854865891909, 
                   1.04287799270615, 0.00784627083829624, 0.0319534626992128, 0.0467044668210082, 
                   0.026782644975436, 0.0170004272158127, 0.0163334053080955, 0.0387097265901905, 
                   0.0208205206888476, 1.04027034688854, 0.0153132034874762, 1.00945685933091, 
                   0.0239160163076971, 0.0141358295053574, 0.0141392606502316, 0.0149388416733132, 
                   0.00840300007696997, 0.0174623663222053, 0.00626139988272876, 
                   0.02373226972634, 0.00752008347796783, 0.0104987624561541, 0.0197169685636446, 
                   0.00549632590152791, 0.0246160037015254, 0.0155958574821157, 
                   1.00135878860339, 0.0107412511995018, 0.0156999774765707, 0.0102228948902351, 
                   0.00694452722726292, 0.00549028327864336, 0.0154867311808948, 
                   0.00699895095575971, 1.55874220880979, 1.00127400653121, 2.25824842385221, 
                   1.18416525673563, 1.99299636560437, 27.6037972637719, 10.1710154073478, 
                   1.99201059255902, 0.981371348224494, 7.58500209516143, 17.7325080787427, 
                   3.36833416001096, 11.306930171416, 13.0448936544829, 9.241135287295, 
                   18.319766677685, 7.87857536106283, 13.3935843076446, 23.2365467504627, 
                   12.1048442537788, 15.7818553856384, 45.1698441261234, 51.3546153062428, 
                   12.551750417766, 3.39435633606025, 34.6982531233785, 44.0187133913885, 
                   35.1092636191178))
    expect_equal(as.vector(as.array(cur_out$J02_imc)[1:5, 1:5, 1:5]),
                 c(2.56508757668402, 3.98794342886768, 0.994999517348545, 1.98998422231417, 
                   0, 2.98499406905562, 0.994999517321752, 0.998965790323043, 0, 
                   2.22282882676681, 1.66344320394516, 0, 0.994970471432098, 4.10237349346291, 
                   3.81052703435922, 4.97897782440174, 0.994994578657299, 0.9997970538085, 
                   0, 0, 1.62284417076302, 2.02274661580171, 0, 1.98997940497277, 
                   3.99951906226492, 1.60810063206105, 1.27719603994058, 0, 1.13955368237191, 
                   0, 7.5930059586011e-05, 0, 0.982412633292276, 0, 0, 1.9750448195261, 
                   0, 1.9949069988713, 0.981585765491606, 0.995150776784699, 0, 
                   0.407327229037369, 2.25651489186555, 0.978644885030787, 0, 0, 
                   5.91770134531288, 0.969982724585896, 0.92554417880411, 1.90268464362844, 
                   0.0044774159241067, 0.00681015336409907, 0.00165821358073044, 
                   0.003460233956606, 0, 0.0054149479304298, 0.00165821342899492, 
                   0.00178765787943071, 0, 0.00370444976340417, 0.00301859406798573, 
                   0, 0.00190591921529414, 0.00696456881149702, 0.00647939305632368, 
                   0.00829769970225082, 0.00171008221345617, 0.00194624707675346, 
                   0.000120562916143128, 0, 0.00270454925140364, 0.00410442932610487, 
                   0.000119494447738691, 0.00386758436768826, 0.00690664150005338, 
                   0.00350606796920903, 0.003878296226295, 0.00055739590856817, 
                   0.00258113037627808, 0, 1.00128777516598, 0.000557401133018344, 
                   0.00182342829861107, 0, 0.00124519546094502, 0.00347242964781346, 
                   0, 0.0031231542692791, 0.00356231491676516, 0.00341612641395389, 
                   0.00278925937351655, 0.00108150118653599, 0.00346233477680135, 
                   0.00125827795284414, 0, 0.000909010367863096, 0.00874398631336669, 
                   0.00124718768316171, 0.998013095160967, 0.00468901946933563, 
                   5.78753347378061, 2.12864791085128, 4.09272652677097, 0.000152430985151432, 
                   1.99701196100706, 10.0294488409872, 3.22254928193429, 0.000101148624870215, 
                   1.99695032943457, 13.6311571052785, 5.78891074649069, 2.06484555320168, 
                   11.0639917486163, 0.987404127464168, 1.99717932088095, 12.1333053770497, 
                   26.7880575465259, 1.76854339336152, 8.98395262825048, 22.0489585104764, 
                   23.7200423218009, 7.28891500810526, 1.08127019081719, 42.1436725368445, 
                   22.6568635847378))

    expect_false(isTRUE(all.equal(imageData(pancreasImages$E34_imc)[,,1], imageData(sm_images$E34_imc)[,,1],
                 check.attributes = FALSE, tolerance = 0.001)))
    expect_equal(imageData(pancreasImages$E34_imc)[,,1], imageData(cur_out$E34_imc)[,,1],
                 check.attributes = FALSE, tolerance = 0.001)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$E34_imc)[,,2], imageData(sm_images$E34_imc)[,,2],
                                  check.attributes = FALSE, tolerance = 0.05)))
    expect_equal(imageData(pancreasImages$E34_imc)[,,2], imageData(cur_out$E34_imc)[,,2],
                 check.attributes = FALSE, tolerance = 0.05)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$E34_imc)[,,3], imageData(sm_images$E34_imc)[,,3],
                                  check.attributes = FALSE, tolerance = 0.05)))
    expect_equal(imageData(pancreasImages$E34_imc)[,,3], imageData(cur_out$E34_imc)[,,3],
                 check.attributes = FALSE, tolerance = 0.05)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$E34_imc)[,,4], imageData(sm_images$E34_imc)[,,4],
                                  check.attributes = FALSE, tolerance = 0.05)))
    expect_equal(imageData(pancreasImages$E34_imc)[,,4], imageData(cur_out$E34_imc)[,,4],
                 check.attributes = FALSE, tolerance = 0.05)
    #expect_false(isTRUE(all.equal(imageData(pancreasImages$E34_imc)[,,5], imageData(sm_images$E34_imc)[,,5],
    #                              check.attributes = FALSE, tolerance = 0.001)))
    #expect_equal(imageData(pancreasImages$E34_imc)[,,5], imageData(cur_out$E34_imc)[,,5],
    #             check.attributes = FALSE, tolerance = 0.001)
    
    expect_false(isTRUE(all.equal(imageData(pancreasImages$G01_imc)[,,1], imageData(sm_images$G01_imc)[,,1],
                                  check.attributes = FALSE, tolerance = 0.001)))
    expect_equal(imageData(pancreasImages$G01_imc)[,,1], imageData(cur_out$G01_imc)[,,1],
                 check.attributes = FALSE, tolerance = 0.001)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$G01_imc)[,,2], imageData(sm_images$G01_imc)[,,2],
                                  check.attributes = FALSE, tolerance = 0.005)))
    expect_equal(imageData(pancreasImages$G01_imc)[,,2], imageData(cur_out$G01_imc)[,,2],
                 check.attributes = FALSE, tolerance = 0.005)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$G01_imc)[,,3], imageData(sm_images$G01_imc)[,,3],
                                  check.attributes = FALSE, tolerance = 0.05)))
    expect_equal(imageData(pancreasImages$G01_imc)[,,3], imageData(cur_out$G01_imc)[,,3],
                 check.attributes = FALSE, tolerance = 0.05)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$G01_imc)[,,4], imageData(sm_images$G01_imc)[,,4],
                                  check.attributes = FALSE, tolerance = 0.5)))
    expect_equal(imageData(pancreasImages$G01_imc)[,,4], imageData(cur_out$G01_imc)[,,4],
                 check.attributes = FALSE, tolerance = 0.5)
    #expect_false(isTRUE(all.equal(imageData(pancreasImages$G01_imc)[,,5], imageData(sm_images$G01_imc)[,,5],
    #                              check.attributes = FALSE, tolerance = 0.001)))
    #expect_equal(imageData(pancreasImages$G01_imc)[,,5], imageData(cur_out$G01_imc)[,,5],
    #             check.attributes = FALSE, tolerance = 0.001)
    
    expect_false(isTRUE(all.equal(imageData(pancreasImages$J02_imc)[,,1], imageData(sm_images$J02_imc)[,,1],
                                  check.attributes = FALSE, tolerance = 0.001)))
    expect_equal(imageData(pancreasImages$J02_imc)[,,1], imageData(cur_out$J02_imc)[,,1],
                 check.attributes = FALSE, tolerance = 0.001)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$J02_imc)[,,2], imageData(sm_images$J02_imc)[,,2],
                                  check.attributes = FALSE, tolerance = 0.005)))
    expect_equal(imageData(pancreasImages$J02_imc)[,,2], imageData(cur_out$J02_imc)[,,2],
                 check.attributes = FALSE, tolerance = 0.005)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$J02_imc)[,,3], imageData(sm_images$J02_imc)[,,3],
                                  check.attributes = FALSE, tolerance = 0.5)))
    expect_equal(imageData(pancreasImages$J02_imc)[,,3], imageData(cur_out$J02_imc)[,,3],
                 check.attributes = FALSE, tolerance = 0.5)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$J02_imc)[,,4], imageData(sm_images$J02_imc)[,,4],
                                  check.attributes = FALSE, tolerance = 0.05)))
    expect_equal(imageData(pancreasImages$J02_imc)[,,4], imageData(cur_out$J02_imc)[,,4],
                 check.attributes = FALSE, tolerance = 0.05)
    #expect_false(isTRUE(all.equal(imageData(pancreasImages$J02_imc)[,,5], imageData(sm_images$J02_imc)[,,5],
    #                              check.attributes = FALSE, tolerance = 0.001)))
    #expect_equal(imageData(pancreasImages$J02_imc)[,,5], imageData(cur_out$J02_imc)[,,5],
    #             check.attributes = FALSE, tolerance = 0.001)
    
    # on disk
    cur_images_ondisk <- CytoImageList(sm_images, on_disk = TRUE,
                                h5FilesPath = cur_path)
    
    cur_size <- file.info(paste0(cur_path, "/E34_imc.h5"))[,"size"]
    
    expect_s4_class(cur_images_ondisk$E34_imc, "DelayedArray")
    expect_s4_class(cur_images_ondisk$G01_imc, "DelayedArray")
    expect_s4_class(cur_images_ondisk$J02_imc, "DelayedArray")
    
    expect_silent(cur_out_2 <- compImage(cur_images_ondisk, sm_real)) 
    
    expect_s4_class(cur_out_2, "CytoImageList")
    expect_equal(cur_out_2@int_metadata, sm_images@int_metadata)
    expect_equal(cur_out_2@elementType, sm_images@elementType)
    expect_equal(cur_out_2@elementMetadata, sm_images@elementMetadata)
    expect_equal(cur_out_2@metadata, sm_images@metadata)
    
    expect_equal(as.array(cur_out$E34_imc),
                 as.array(cur_out_2$E34_imc))
    expect_equal(as.array(cur_out$G01_imc),
                 as.array(cur_out_2$G01_imc))
    expect_equal(as.array(cur_out$G01_imc),
                 as.array(cur_out_2$G01_imc))
    
    cur_size_2 <- file.info(paste0(cur_path, "/E34_imc.h5"))[,"size"]
    expect_gt(cur_size_2, cur_size)
    
    expect_s4_class(cur_out_2$E34_imc, "DelayedArray")
    expect_s4_class(cur_out_2$G01_imc, "DelayedArray")
    expect_s4_class(cur_out_2$J02_imc, "DelayedArray")
    
    # Check if second layer was created
    expect_true("E34_imc_comp" %in% h5ls(path(cur_out_2$E34_imc@seed))$name)
    expect_true("E34_imc_comp" %in% h5ls(path(cur_images_ondisk$E34_imc@seed))$name)
    expect_equal(h5ls(path(cur_out_2$E34_imc@seed))$name, 
                 c(".E34_imc_dimnames", "3", "E34_imc", "E34_imc_comp"))
    
    expect_silent(cur_out_3 <- compImage(cur_images_ondisk, sm_real)) 
    
    expect_equal(as.array(cur_out$E34_imc),
                 as.array(cur_out_3$E34_imc))
    expect_equal(as.array(cur_out$G01_imc),
                 as.array(cur_out_3$G01_imc))
    expect_equal(as.array(cur_out$G01_imc),
                 as.array(cur_out_3$G01_imc))
    
    expect_equal(h5ls(path(cur_out_3$E34_imc@seed))$name, 
                 c(".E34_imc_dimnames", "3", "E34_imc", "E34_imc_comp"))
    
    # Fail
    expect_error(compImage("test"),
                 regexp = "'object' not of type 'CytoImageList'",
                 fixed = TRUE)
    expect_error(compImage(pancreasImages),
                 regexp = "argument \"sm\" is missing, with no default",
                 fixed = TRUE)
    expect_error(compImage(pancreasImages, "test"),
                 regexp = "'sm' is not of type 'matrix'",
                 fixed = TRUE)
    expect_error(compImage(pancreasImages, matrix("test")),
                 regexp = "'sm' contains values outside 0-1 range.",
                 fixed = TRUE)
    expect_error(compImage(pancreasImages, sm_real),
                 regexp = "Not all 'channelNames(object)' are of the format (mt)(mass)Di.",
                 fixed = TRUE)
    sm <- sm_real
    colnames(sm) <- c(1,2,3,4,5)
    expect_error(compImage(sm_images, sm),
                 regexp = "'channelNames(object)' and 'colnames(sm)' do not match.",
                 fixed = TRUE)
    channelNames(sm_images) <- c(1,2,3,4,5)
    expect_error(compImage(sm_images, sm_real),
                 regexp = "Not all 'channelNames(object)' are of the format (mt)(mass)Di.",
                 fixed = TRUE)
})
