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
    
    out <- endoapply(sm_images, function(x){
        cur_out <- apply(x, c(1,2), function(y) return(y %*% sm_strong))
        cur_out <- aperm(cur_out, c(2,3,1))
        return(Image(cur_out))
    })
    channelNames(out) <- channelNames(pancreasImages)
    sm_images <- out
    
    # Visual inspection
    plotPixels(pancreasImages, colour_by = "H3")
    plotPixels(sm_images, colour_by = "H3")
    plotPixels(pancreasImages, colour_by = "CD99")
    plotPixels(sm_images, colour_by = "CD99")
    plotPixels(pancreasImages, colour_by = "PIN")
    plotPixels(sm_images, colour_by = "PIN")
    plotPixels(pancreasImages, colour_by = "CD8a")
    plotPixels(sm_images, colour_by = "CD8a")
    plotPixels(pancreasImages, colour_by = "CDH")
    plotPixels(sm_images, colour_by = "CDH")
    
    channelNames(sm_images) <- rownames(sm_strong)
    
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
                 c(2.23578691482544, 2.88552832603454, 3.40094327926636, 3.22383165359497, 
                   0.998766601085662, 0.253727495670314, 1.99001955986022, 0.9950097799301, 
                   3.17507600784301, 1.99001955986023, 1.26963245868682, 2.26464223861694, 
                   0.995009779930112, 1.12834095954895, 0, 0.999198198318478, 0, 
                   2.18006634712219, 4.4866042137146, 0, 1.99001955986023, 1.41092383861542, 
                   0, 0, 0, 2.27523970603943, 0.982835233211518, 1.00974380970001, 
                   4.22755718231201, 2.20721272071921e-16, 2.12543272972107, 0.98138463497162, 
                   0.982144415378575, 4.6439061164856, 4.60726071214195e-16, 2.65206265449524, 
                   4.44857520294786e-16, 7.58915550986695e-16, 1.98678028583527, 
                   0.998160183429719, 7.67151408905227e-16, 0, 0, 1.13381838798523, 
                   0, 0, 0, 0, 0.99363511800766, 0, 0, 0, 2.4701816282689e-16, 0, 
                   0, 0, 2.16337061759597e-16, 0, 0, 0, 2.12880592476332e-07, 0, 
                   0, 0, 0, 0, 0, 1.00299690807935e-16, 0, 0, 0, 0, 0, 6.32687268151533e-16, 
                   0, 6.10191405892687e-17, 1.74999046325684, 0, 1.64071826474853e-16, 
                   3.28572770608724e-17, 1.94439564737972e-16, 2.1319274800509e-16, 
                   3.85965625312858e-16, 2.56614558311194e-16, 7.53835593442659e-17, 
                   4.57368151775708e-16, 9.7717796656081e-17, 1.15749526429746e-16, 
                   6.67803442275272e-17, 6.75710029006214e-18, 1.50170020269713e-16, 
                   0.994114995002746, 3.25868869566352e-17, 0, 0, 0, 0, 2.23528671264648, 
                   0.999972105026245, 1.35901546478271, 0.940284013748169, 8.39323902130127, 
                   0.99849933385849, 10.0154972076416, 4.637526512146, 19.6513938903809, 
                   29.3538608551025, 64.6947784423828, 27.3468551635742, 14.9151773452759, 
                   43.2846260070801, 22.2493591308594, 13.0902843475342, 18.3537216186523, 
                   24.8451747894287, 11.4956560134888, 9.78407001495361, 5.88088750839234, 
                   0.986984312534332, 1.02242743968964, 0, 0, 2.58079719543457, 
                   8.27865314483643, 3.21291995048523))
    expect_equal(as.vector(as.array(cur_out$G01_imc)[1:5, 1:5, 1:5]),
                 c(42.6907119750976, 25.232837677002, 25.239236831665, 24.0195350646973, 
                   14.9995908737183, 28.857421875, 11.1768398284912, 35.8239707946777, 
                   13.4233913421631, 17.8999481201172, 31.6688098907471, 7.51331901550293, 
                   34.4753074645996, 25.2723693847656, 4.44271850585937, 19.173656463623, 
                   28.0249900817871, 15.9826612472534, 10.1179981231689, 9.8008460998535, 
                   23.0579395294189, 12.4933423995972, 23.3956642150879, 8.8474531173706, 
                   6.01483392715453, 0, 0, 3.00805682867537e-15, 1.14488792419434, 
                   0, 0.998183071613309, 0, 2.83587074279785, 6.13080783286931e-06, 
                   0, 1.52557253837585, 0.998160183429719, 3.98741555213928, 0.986646056175229, 
                   7.75992011180919e-16, 0, 0, 0.981460273265839, 0.988971889019013, 
                   1.80674989038669e-15, 1.98973262310028, 0, 2.99159264564514, 
                   0.97893738746643, 4.8189868927002, 0, 6.03217214339641e-17, 0, 
                   0, 0, 0, 0, 0, 0, 2.94078779220581, 1.20622252562452e-15, 9.94543109111735e-17, 
                   0.999847590923308, 0.999899148941042, 0, 5.3520040343311e-18, 
                   0, 0, 6.00943535474613e-16, 0, 7.71311780097397e-16, 0, 0.999503374099735, 
                   1.59382124839064e-15, 0.997151911258697, 0, 0, 0, 0, 0, 0, 0, 
                   0, 0, 2.44793419664594e-16, 0, 0, 0, 0, 0.998086810112, 1.2388428052818e-16, 
                   0, 4.89905647259838e-17, 2.91804912988823e-16, 6.46897126899015e-16, 
                   0, 0, 1.54041314125061, 0.994276285171509, 2.24675965309143, 
                   1.18235528469086, 1.99191880226135, 27.6025657653809, 10.1698713302612, 
                   1.99136519432068, 0.980086922645569, 7.58448457717896, 17.7307186126709, 
                   3.36774706840515, 11.305700302124, 13.0433864593506, 9.24070262908936, 
                   18.3178234100342, 7.87726354598999, 13.3933162689209, 23.2355976104736, 
                   12.103588104248, 15.78102684021, 45.1690864562988, 51.3538932800293, 
                   12.5505819320679, 3.39380836486816, 34.6967391967774, 44.0180168151856, 
                   35.1083717346192))
    expect_equal(as.vector(as.array(cur_out$J02_imc)[1:5, 1:5, 1:5]),
                 c(2.56513524055481, 3.98799896240234, 0.995009779930114, 1.99001955986023, 
                   0, 2.98502922058105, 0.995009779930114, 0.998989760875702, 0, 
                   2.22285175323486, 1.66348826885223, 0, 0.995009779930113, 4.10242557525635, 
                   3.81057667732239, 4.97902917861938, 0.995009779930107, 0.999840378761293, 
                   0, 0, 1.62286090850829, 2.02285480499268, 9.98057272913661e-16, 
                   1.99001955986022, 3.99958419799804, 1.60733664035797, 1.27663969993591, 
                   1.14893049049833e-16, 1.13901674747467, 0, 6.40914067195336e-16, 
                   1.12921868149168e-16, 0.981931209564209, 1.10347361417425e-16, 
                   4.38027567992899e-16, 1.97406804561615, 0, 1.99390268325806, 
                   0.981185019016266, 0.994735479354859, 8.63601561217332e-16, 0.407142639160159, 
                   2.255375623703, 0.978140294551849, 0, 1.53651146412609e-15, 5.91469812393188, 
                   0.969482600688934, 0.924965262413028, 1.90180563926697, 8.00042565079957e-16, 
                   2.38141574816903e-16, 2.43681164872445e-18, 1.30631671547082e-16, 
                   0, 3.41020437597507e-17, 0, 1.33738276886231e-16, 0, 0, 0, 0, 
                   0, 0, 0, 0, 0, 5.13395063957856e-16, 7.64635330341934e-17, 0, 
                   0, 1.25358166550498e-15, 2.17904946155624e-16, 1.44892840762785e-16, 
                   5.27977934518383e-16, 0, 0, 2.59678652173295e-17, 1.63205515154427e-17, 
                   0, 0.998832404613495, 4.04310704311885e-17, 2.62118005054629e-18, 
                   1.06167702530658e-17, 3.37113115874083e-17, 9.57085654436642e-17, 
                   0, 0, 0, 0, 1.21760103020968e-16, 2.49650223556376e-16, 0, 3.06703334435003e-17, 
                   0, 1.49992833090266e-16, 0, 0, 0.994928717613221, 0, 5.78729391098022, 
                   2.12839007377625, 4.09265995025635, 0, 1.99699997901917, 10.0292625427246, 
                   3.22248792648315, 0, 1.99693834781647, 13.6309814453125, 5.7886872291565, 
                   2.06483316421509, 11.0637636184692, 0.987166047096252, 1.99694669246674, 
                   12.1330223083496, 26.7878303527832, 1.76835513114929, 8.9838399887085, 
                   22.0488262176514, 23.7198314666748, 7.28843069076538, 1.08120548725128, 
                   42.1432800292969, 22.6564445495605))
    
    channelNames(cur_out) <- channelNames(pancreasImages)
    
    expect_false(isTRUE(all.equal(imageData(pancreasImages$E34_imc)[,,1], imageData(sm_images$E34_imc)[,,1])))
    expect_equal(pancreasImages$E34_imc, cur_out$E34_imc)
    expect_equal(pancreasImages$G01_imc, cur_out$G01_imc)
    expect_equal(pancreasImages$J02_imc, cur_out$J02_imc)

    
    # weak spillover
    sm_images <- pancreasImages
    
    out <- endoapply(sm_images, function(x){
        cur_out <- apply(x, c(1,2), function(y) return(y %*% sm_real))
        cur_out <- aperm(cur_out, c(2,3,1))
        return(Image(cur_out))
    })
    channelNames(out) <- channelNames(pancreasImages)
    sm_images <- out
    
    # Visual inspection
    plotPixels(pancreasImages, colour_by = "H3")
    plotPixels(sm_images, colour_by = "H3")
    plotPixels(pancreasImages, colour_by = "CD99")
    plotPixels(sm_images, colour_by = "CD99")
    plotPixels(pancreasImages, colour_by = "PIN")
    plotPixels(sm_images, colour_by = "PIN")
    plotPixels(pancreasImages, colour_by = "CD8a")
    plotPixels(sm_images, colour_by = "CD8a")
    plotPixels(pancreasImages, colour_by = "CDH")
    plotPixels(sm_images, colour_by = "CDH")
    
    channelNames(sm_images) <- rownames(sm_real)
    
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
                 c(2.23578691482544, 2.88552832603455, 3.40094327926636, 3.22383165359497, 
                   0.998766601085664, 0.25372749567032, 1.99001955986024, 0.99500977993013, 
                   3.17507600784302, 1.99001955986023, 1.26963245868683, 2.26464223861695, 
                   0.995009779930118, 1.12834095954895, 4.83596051887921e-15, 0.999198198318485, 
                   2.69332780168834e-15, 2.18006634712219, 4.4866042137146, 2.21770122912683e-16, 
                   1.99001955986023, 1.41092383861542, 4.55654073638636e-16, 3.32619462662008e-15, 
                   3.83754286353452e-16, 2.27523970603943, 0.982835233211518, 1.00974380970001, 
                   4.22755718231201, 0, 2.12543272972107, 0.981384634971619, 0.98214441537857, 
                   4.6439061164856, 6.63651264716718e-16, 2.65206265449524, 4.41551510765918e-16, 
                   2.20556729785805e-16, 1.98678028583527, 0.998160183429718, 1.09655057649601e-16, 
                   1.0327327628663e-17, 8.86478916216937e-16, 1.13381838798523, 
                   0, 0, 0, 0, 0.99363511800766, 0, 0, 1.22499415057453e-15, 1.11238882125062e-16, 
                   0, 0, 1.11200103223052e-16, 0, 1.09352185873552e-16, 0, 0, 2.12880593022593e-07, 
                   0, 0, 2.22308723148135e-16, 0, 0, 0, 0, 2.22459995021217e-16, 
                   0, 0, 0, 0, 0, 0, 0, 1.74999046325684, 4.15025262455971e-19, 
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.994114995002747, 0, 
                   1.48110139581551e-18, 0, 0, 0, 2.23528671264648, 0.999972105026245, 
                   1.35901546478271, 0.940284013748169, 8.39323902130127, 0.99849933385849, 
                   10.0154972076416, 4.637526512146, 19.6513938903809, 29.3538608551025, 
                   64.6947784423828, 27.3468551635742, 14.9151773452759, 43.2846260070801, 
                   22.2493591308594, 13.0902843475342, 18.3537216186523, 24.8451747894287, 
                   11.4956560134888, 9.78407001495361, 5.88088750839233, 0.986984312534332, 
                   1.02242743968964, 0, 9.22015961887504e-25, 2.58079719543457, 
                   8.27865314483642, 3.21291995048523))
    expect_equal(as.vector(as.array(cur_out$G01_imc)[1:5, 1:5, 1:5]),
                 c(42.6907119750977, 25.232837677002, 25.239236831665, 24.0195350646973, 
                   14.9995908737183, 28.857421875, 11.1768398284912, 35.8239707946777, 
                   13.4233913421631, 17.8999481201172, 31.6688098907471, 7.51331901550293, 
                   34.4753074645996, 25.2723693847656, 4.44271850585938, 19.1736564636231, 
                   28.0249900817871, 15.9826612472534, 10.117998123169, 9.80084609985353, 
                   23.057939529419, 12.4933423995972, 23.3956642150879, 8.84745311737061, 
                   6.01483392715455, 0, 0, 7.09533573928995e-15, 1.14488792419434, 
                   2.20676415100722e-16, 0.998183071613312, 8.8651733037851e-16, 
                   2.83587074279785, 6.13080783323267e-06, 2.16185774069926e-15, 
                   1.52557253837586, 0.998160183429719, 3.98741555213928, 0.986646056175232, 
                   9.14042689628838e-16, 7.09420260716262e-15, 1.77359768936059e-15, 
                   0.981460273265841, 0.988971889019014, 1.77030327197491e-15, 1.98973262310028, 
                   0, 2.99159264564515, 0.978937387466434, 4.8189868927002, 0, 0, 
                   0, 2.21960738102303e-16, 0, 1.11085698156659e-16, 0, 0, 0, 2.94078779220581, 
                   0, 1.10609433251125e-16, 0.99984759092331, 0.99989914894104, 
                   5.04325723449548e-16, 0, 0, 0, 1.09942229769164e-16, 0, 0, 0, 
                   0.999503374099731, 7.79541732183792e-16, 0.997151911258699, 0, 
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3.3343092841449e-16, 0, 0.998086810112, 
                   0, 0, 0, 0, 0, 0, 0, 1.54041314125061, 0.994276285171509, 2.24675965309143, 
                   1.18235528469086, 1.99191880226135, 27.6025657653809, 10.1698713302612, 
                   1.99136519432068, 0.980086922645569, 7.58448457717896, 17.7307186126709, 
                   3.36774706840515, 11.305700302124, 13.0433864593506, 9.24070262908936, 
                   18.3178234100342, 7.87726354598999, 13.3933162689209, 23.2355976104736, 
                   12.103588104248, 15.78102684021, 45.1690864562988, 51.3538932800293, 
                   12.5505819320679, 3.39380836486816, 34.6967391967773, 44.0180168151855, 
                   35.1083717346191))
    expect_equal(as.vector(as.array(cur_out$J02_imc)[1:5, 1:5, 1:5]),
                 c(2.56513524055481, 3.98799896240234, 0.995009779930116, 1.99001955986023, 
                   4.43552575162044e-16, 2.98502922058106, 0.995009779930115, 0.998989760875703, 
                   8.87549248467535e-16, 2.22285175323487, 1.66348826885224, 4.43540245825365e-16, 
                   0.995009779930115, 4.10242557525634, 3.81057667732239, 4.97902917861939, 
                   0.995009779930118, 0.999840378761293, 9.82461080743612e-16, 3.54887573951098e-15, 
                   1.6228609085083, 2.02285480499268, 9.54709748276902e-16, 1.99001955986023, 
                   3.99958419799805, 1.60733664035797, 1.27663969993591, 3.32189581133068e-16, 
                   1.13901674747467, 0, 1.04468528051004e-15, 3.32556811569418e-16, 
                   0.981931209564209, 0, 0, 1.97406804561615, 0, 1.99390268325806, 
                   0.981185019016266, 0.994735479354859, 1.77237813972365e-15, 0.407142639160156, 
                   2.255375623703, 0.978140294551849, 0, 4.42882072657039e-16, 5.91469812393188, 
                   0.969482600688934, 0.924965262413025, 1.90180563926697, 2.22353566301504e-16, 
                   0, 0, 1.3718357599068e-17, 0, 7.19271442227435e-16, 0, 0, 0, 
                   0, 0, 0, 0, 0, 1.1119013885545e-16, 0, 0, 0, 1.30570954634137e-17, 
                   0, 0, 0, 6.34017592606454e-18, 7.79541732183792e-16, 2.22008617562531e-16, 
                   0, 1.73307244964571e-18, 0, 6.63040732823723e-18, 0, 0.998832404613496, 
                   0, 1.73361070713585e-18, 0, 0, 0, 0, 0, 3.46614489929143e-18, 
                   0, 0, 0, 0, 9.24391799796069e-19, 0, 0, 0, 0, 0.994928717613221, 
                   0, 5.78729391098022, 2.12839007377625, 4.09265995025635, 0, 1.99699997901916, 
                   10.0292625427246, 3.22248792648315, 0, 1.99693834781647, 13.6309814453125, 
                   5.78868722915649, 2.06483316421509, 11.0637636184692, 0.987166047096252, 
                   1.99694669246674, 12.1330223083496, 26.7878303527832, 1.76835513114929, 
                   8.9838399887085, 22.0488262176514, 23.7198314666748, 7.28843069076538, 
                   1.08120548725128, 42.1432800292969, 22.6564445495605))

    channelNames(cur_out) <- channelNames(pancreasImages)
    
    expect_false(isTRUE(all.equal(imageData(pancreasImages$E34_imc)[,,1], imageData(sm_images$E34_imc)[,,1])))
    expect_equal(imageData(pancreasImages$E34_imc), imageData(cur_out$E34_imc))
    expect_equal(imageData(pancreasImages$G01_imc), imageData(cur_out$G01_imc))
    expect_equal(imageData(pancreasImages$J02_imc), imageData(cur_out$J02_imc))

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
    
    channelNames(cur_out_2) <- channelNames(cur_out)
    
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
    library(rhdf5)
    expect_true("E34_imc_comp" %in% h5ls(path(cur_out_2$E34_imc@seed))$name)
    expect_true("E34_imc_comp" %in% h5ls(path(cur_images_ondisk$E34_imc@seed))$name)
    expect_equal(h5ls(path(cur_out_2$E34_imc@seed))$name, 
                 c(".E34_imc_dimnames", "3", "E34_imc", "E34_imc_comp"))
    
    expect_silent(cur_out_3 <- compImage(cur_images_ondisk, sm_real)) 
    
    channelNames(cur_out_3) <- channelNames(cur_out)  
    
    expect_equal(as.array(cur_out$E34_imc),
                 as.array(cur_out_3$E34_imc))
    expect_equal(as.array(cur_out$G01_imc),
                 as.array(cur_out_3$G01_imc))
    expect_equal(as.array(cur_out$G01_imc),
                 as.array(cur_out_3$G01_imc))
    
    expect_equal(h5ls(path(cur_out_3$E34_imc@seed))$name, 
                 c(".E34_imc_dimnames", "3", "E34_imc", "E34_imc_comp"))
    
    # Overwrite
    cur_images_ondisk <- CytoImageList(sm_images, on_disk = TRUE,
                                       h5FilesPath = cur_path)
    
    expect_silent(cur_out_2 <- compImage(cur_images_ondisk, sm_real, overwrite = TRUE)) 
    expect_equal(h5ls(path(cur_out_2$E34_imc@seed))$name, 
                 c("E34_imc_comp"))
    
    channelNames(cur_out_2) <- channelNames(pancreasImages)
    
    expect_equal(as.array(cur_out$E34_imc),
                 as.array(cur_out_2$E34_imc))
    expect_equal(as.array(cur_out$G01_imc),
                 as.array(cur_out_2$G01_imc))
    expect_equal(as.array(cur_out$J02_imc),
                 as.array(cur_out_2$J02_imc))
    
    
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
