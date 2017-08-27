#running contact sheets
#v.7
#30apr2017
	
	md <- read.table("test.csv", header = TRUE, sep = ',')
	source("contactsheets.R")
	
	key <- contactsheets(md, "testphotos", rows = 4, cols = 3)
	
#full list of parameters
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