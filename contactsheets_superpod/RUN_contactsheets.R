#contactsheets runner
#30apr2017
#v.7* (superpod edition)

source("contactsheets.R")

zca <- read.table("zca_finsheets.csv", header = TRUE, sep = ',')
zca$Match_Years[which(is.na(zca$Match_Years))] <- ""
#landscape
key_zca <- contactsheets(zca, "zcacropped", rows = 3, cols = 1, export = TRUE, verbose = TRUE, aspect = 1/2.7878, height = 8.5, width = 11)
#portrait
key_zca <- contactsheets(zca, "zcacropped", rows = 3, cols = 1, export = TRUE, verbose = TRUE, aspect = 1/2.7878)

write.table(key_zca, "zcakey.csv", sep = ',', row.names = FALSE)


#gma finsheets
gma <- read.table("gma_finsheets.csv", header = TRUE, sep = ',')
source("finsheets.R")
#landscape
key_gma <- contactsheets(gma, "gmacropped", rows = 2, cols = 3, export = TRUE, verbose = TRUE, width = 11, height = 8.5)
#portrait
key_gma <- contactsheets(gma, "gmacropped", rows = 3, cols = 2, export = TRUE, verbose = TRUE, width = 8.5, height = 11)

write.table(key_gma, "gmakey.csv", sep = ",", row.names = FALSE)


#full function definition
	# metadata <- gma
	# image_path <- "gmacropped"
	# rows = 3
	# cols = 2
	# textsize = 2
	# titlesize = 2.75
	# labelcolor = rgb(1, 1, 1)
	# byearcolor = rgb(1, 1, 1) 
	# marginbuffer = 0.2
	# label_y_nudge = 0
	# label_x_nudge = 0
	# byear_y_nudge = 0
	# byear_x_nudge = 0
	# export = FALSE
	# square = TRUE
	# verbose = TRUE
	# scaleimage = FALSE
	# aspect = 1
