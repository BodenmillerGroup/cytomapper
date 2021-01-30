# Start cytomapperShiny only with SCE object
library(cytomapper)
data("pancreasSCE")
data("pancreasMasks")
cytomapperShiny(object = pancreasSCE, mask = pancreasMasks, 
                cell_id = "CellNb", img_id = "ImageNb")
