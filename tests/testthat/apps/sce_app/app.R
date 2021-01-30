# Start cytomapperShiny only with SCE object
library(cytomapper)
data("pancreasSCE")
cytomapperShiny(object = pancreasSCE, cell_id = "CellNb", img_id = "ImageNb")
