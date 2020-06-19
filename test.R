devtools::load_all("/Volumes/thoch/Git/cytomapper/")
data(pancreasSCE)
data(pancreasImages)
data(pancreasMasks)

cytomapperShiny(object = pancreasSCE, mask = pancreasMasks, image = pancreasImages,
                        cell_id = "CellNb", img_id = "ImageNb")

