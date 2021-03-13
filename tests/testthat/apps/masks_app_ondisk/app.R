library(cytomapper)
data("pancreasSCE")
data("pancreasMasks")

cur_path <- tempdir()
on.exit(unlink(cur_path))

cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)

cytomapperShiny(object = pancreasSCE, mask = cur_Masks, 
                cell_id = "CellNb", img_id = "ImageNb")
