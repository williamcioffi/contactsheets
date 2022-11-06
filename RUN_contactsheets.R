###
# example contact sheet run and debugging help
# wrc

# tag
md <- read.table("test/tag.csv", header = TRUE, sep = ',')
source("contactsheets.R")
key <- contactsheets(md, "test/tag/out/", rows = 4, cols = 3)

# biop
md <- read.table("test/biop.csv", header = TRUE, sep = ',')
source("contactsheets.R")
key <- contactsheets(md, "test/biop/", rows = 4, cols = 3)

	
	
### for debugging etc
# full list of parameters
# contactsheets <- function(metadata, image_path,
	# rows = 4,
	# cols = 3,
	# textsize = 2,
	# titlesize = 2.75,
	# labelcolor = rgb(1, 1, 1),
	# byearcolor = rgb(1, 1, 1), 
	# marginbuffer = 0.2,
	# label_y_nudge = 0,
	# label_x_nudge = 0,
	# byear_y_nudge = 0,
	# byear_x_nudge = 0,
	# export = TRUE,
	# verbose = TRUE
# )

# without the commas	
# metadata <- read.table("test/test.csv", header = TRUE, sep = ',')	
# image_path <- "test/photos/"
# rows = 4
# cols = 3
# textsize = 2
# titlesize = 2.75
# labelcolor = rgb(1, 1, 1)
# byearcolor = rgb(1, 1, 1)
# marginbuffer = 0.2
# label_y_nudge = 0
# label_x_nudge = 0
# byear_y_nudge = 0
# byear_x_nudge = 0
# export = TRUE
# verbose = TRUE
