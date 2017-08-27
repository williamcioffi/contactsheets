#contactsheets
#30apr2017
#v.7* (superpod edition)

###########################
####function definition####
###########################

contactsheets <- function(metadata, image_path,
	rows = 4,
	cols = 3,
	textsize = 2,
	titlesize = 2.75,
	labelcolor = rgb(1, 1, 1),
	byearcolor = rgb(1, 1, 1), 
	marginbuffer = 0.2,
	label_y_nudge = 0,
	label_x_nudge = 0,
	byear_y_nudge = 0,
	byear_x_nudge = 0,
	export = TRUE,
	aspect = 1,
	scaleimage = TRUE,
	verbose = FALSE,
	height = 11,
	width = 8.5
) {

###############################
####LIBRARIES AND CONSTANTS####
###############################

#load image libraries and tools
require(tiff)
require(png)
require(jpeg)
require(tools)
require(TeachingDemos)


#load support functions
source("contactsheets_supportfunctions.R")

#global constants
gPAGENUMBERSIZE <<- 2
gMARGINBUFFER <<- marginbuffer
gPAGEROWS <<- rows
gPAGECOLS <<- cols
gIMAGESPERPAGE <<- gPAGEROWS*gPAGECOLS
gTITLESIZE <<- titlesize
gSHADOWTEXTSCALE <<- .1
gLABELCHEAT <<- -0.05


#local constants
img_dir = image_path

#####################
####load metadata####
#####################
md <- metadata
names(md) <- tolower(names(md))

#if these are just one integer than added for everything
#otherwise they should be the length of nrow
md[, 'labelcolor'] <- labelcolor
md[, 'byearcolor'] <- byearcolor
md[, 'textsize'] <- textsize
md[, 'label_y_nudge'] <- label_y_nudge
md[, 'label_x_nudge'] <- label_x_nudge
md[, 'byear_y_nudge'] <- byear_y_nudge
md[, 'byear_x_nudge'] <- byear_x_nudge

set <- md$set
u_set <- unique(set)
nset <- length(u_set)

###################
####da megaloop####
###################

contactsheet_key <- data.frame(id = md$id, set = md$set)
contactsheet_key[, 'page'] <- NA

for(s in 1:nset) {

dese <- which(set == u_set[s])
md_cur <- md[dese, ]

#side doesn't need to be modified
side <- as.character(md_cur$side)

#make full file paths for images
fnames <- file.path(image_path, md_cur$filename)

#make labels
name <- as.character(md_cur$name)
name_prime <- titlecase_irish(tolower(name))
ids <- as.character(md_cur$id)
ids <- toupper(ids)
label <- name_prime

#assign sexes and if the sex is behavioral
sex_prime <- as.character(md_cur$match_years)

dese <- which(md_cur$biopsy == "YES")
dose <- which(nchar(sex_prime[dese]) > 0)
duse <- which(nchar(sex_prime[dese]) == 0)
sex_prime[dese[dose]] <- paste(sex_prime[dese[dose]], ", biop", sep = "")
sex_prime[dese[duse]] <- paste(sex_prime[dese[duse]], "biop", sep = "")
sex <- sex_prime

#detect fileformat
fformat <- tolower(as.character(md_cur$file_extension))
fformat[grep("tif", fformat)] <- "TIFF"
fformat[grep("jpe*g", fformat)] <- "JPEG"
fformat[grep("png", fformat)] <- "PNG"

#grab groups and set north south order
group <- as.character(md_cur$group)
grouporder <- md_cur$group_order

#name_prime is just used to alphabetized within a page
#id is just to use for the contactsheet_key
dat <- data.frame(fnames, label, sex, side, fformat, group, grouporder,
	name_prime,
	textsize = md_cur$textsize,
	labelcolor = md_cur$labelcolor,
	byearcolor = md_cur$byearcolor,
	byear_x_nudge = md_cur$byear_x_nudge,
	byear_y_nudge = md_cur$byear_y_nudge,
	label_x_nudge = md_cur$label_x_nudge,
	label_y_nudge = md_cur$label_y_nudge,
	id = md_cur$id,
	set = md_cur$set
)

#sort by group, id
oo <- order(dat$id)
dat <- dat[oo, ]
oo <- order(dat$grouporder)
dat <- dat[oo, ]

###################
####da miniloop####
###################

runningpagecount <- 0
u_groups <- unique(dat$group)
ngroups <- length(u_groups)

for(g in 1:ngroups) {
	dese <- which(dat$group == u_groups[g])
	dat_cur <- dat[dese, ]
	
	npages <- ceiling(nrow(dat_cur) / gIMAGESPERPAGE)
	pageid <- rep(1:npages, each = gIMAGESPERPAGE)[1:nrow(dat_cur)]
	u_pageid <- unique(pageid)
	
	# for(p in 1:npages) {
		# dese <- which(pageid == u_pageid[p])
		# oo <- order(dat_cur[dese, ]$name_prime)
		# dat_cur[dese, ] <- dat_cur[dese[oo], ]
	# }
	
	#add pages to contactsheet_key
	#this assumes that id-set combinations will always be unique
	desekeys <- which(paste(contactsheet_key$id, contactsheet_key$set) %in% paste(dat_cur$id, dat_cur$set))
	contactsheet_key[desekeys, ]$page <- pageid + runningpagecount
	
	runningpagecount <- drawpages(dat_cur, runningpagecount,
		u_groups[g],
		as.character(u_set[s]),
		export,
		aspect, scaleimage, verbose, height, width)
}

}

contactsheet_key

}