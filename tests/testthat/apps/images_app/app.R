# Start cytomapperShiny only with SCE object
library(cytomapper)
data("pancreasSCE")
data("pancreasMasks")
data("pancreasImages")
cytomapperShiny(object = pancreasSCE, mask = pancreasMasks, 
                image = pancreasImages,
                cell_id = "CellNb", img_id = "ImageNb")
