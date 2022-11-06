###
# contactsheets
# wrc

# original code from 2017 updated 2022



###function definition####
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
	verbose = FALSE
) {

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
gVERBOSE <<- verbose

#local constants
img_dir <- image_path

# load metadata
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



### da megaloop
contactsheet_key <- data.frame(
  id = vector(),
  name = vector(),
  sex = vector(),
  set = vector(),
  group = vector(),
  page = vector()
)

for(s in 1:nset) {

dese <- which(set == u_set[s])
md_cur <- md[dese, ]

# side doesn't need to be modified
side <- as.character(md_cur$side)

# make full file paths for images
fnames <- file.path(image_path, md_cur$filename)

# make labels
name <- as.character(md_cur$name)
name_prime <- titlecase_irish(tolower(name))
ids <- as.character(md_cur$id)
ids <- toupper(ids)
label <- paste(ids, name_prime, sep = " - ")

# make pyear
pyear_prime <- as.character(md_cur$photo_year)
pyear_prime <- substring(pyear_prime, 3, 4)
pyear <- paste("P", pyear_prime, sep = "")

# assign sexes and if the sex is behavioral
sex_prime <- as.character(md_cur$sex)
sex_prime[which(sex_prime == "UNKNOWN")] <- "U"
sex_prime[which(sex_prime == "FEMALE")] <- "F"
sex_prime[which(sex_prime == "MALE")] <- "M"
dese <- which(md_cur$sex_by_behavior == "YES")
sex <- sex_prime
sex[dese] <- paste(sex_prime[dese], "b", sep = "")

# assign byears and if the year is unknown
byear_prime <- as.character(md_cur$birthyear)
dese <- which(md_cur$birthyear_certainty == "NO")
byear <- byear_prime
byear[dese] <- paste("~", byear_prime[dese], sep = "")

# detect fileformat
fformat <- tolower(as.character(md_cur$file_extension))
fformat[grep("tif", fformat)] <- "TIFF"
fformat[grep("jpe*g", fformat)] <- "JPEG"
fformat[grep("png", fformat)] <- "PNG"

# grab groups and set north south order
group <- as.character(md_cur$group)
grouporder <- md_cur$group_order

# name_prime is just used to alphabetized within a page
# id is just to use for the contactsheet_key
dat <- data.frame(fnames, label, byear, pyear, side, sex, fformat, group, grouporder,
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

# sort by group, byear, label

oo <- order(dat$label)
dat <- dat[oo, ]
oo <- order(dat$byear)
dat <- dat[oo, ]
oo <- order(dat$grouporder)
dat <- dat[oo, ]



### da miniloop (inner)
runningpagecount <- 0
u_groups <- unique(dat$group)
ngroups <- length(u_groups)
skip_g <- FALSE
combine <- FALSE

for(g in 1:ngroups) {
	if(combine) {
		combine <- FALSE
		next
	}
	
	dese <- which(dat$group == u_groups[g])
	nphotos <- length(dese)
	
	if(g < ngroups) {
		nphotos_next <- length(which(dat$group == u_groups[g+1]))
	} else {
		nphotos_next <- 0
	}
	
  # warning:
  # this is only ok if there if there are enough photos per page
	combine <- switch(nphotos,
		any(nphotos_next %in% 1:6),
		any(nphotos_next %in% 1:6),
		any(nphotos_next %in% 1:6),
		any(nphotos_next %in% 1:3),
		any(nphotos_next %in% 1:3),
		any(nphotos_next %in% 1:3)
	)
	if(is.null(combine)) combine <- FALSE
	
	if(combine)
		dese <- which(dat$group %in% c(as.character(u_groups[g]), as.character(u_groups[g+1])))
	
	dat_cur <- dat[dese, ]
	
	nphotos <- nrow(dat_cur)
	npages <- ceiling(nphotos / gIMAGESPERPAGE)
	pageid <- rep(1:npages, each = gIMAGESPERPAGE)[1:nrow(dat_cur)]
	u_pageid <- unique(pageid)
	groupid <- dat_cur$group
	u_groupid <- unique(groupid)
	ngroupid <- length(u_groupid)
	
	# add pages to contactsheet_key
	
	contactsheet_tmp <- data.frame(
	  id = dat_cur$id, 
	  name = dat_cur$name_prime, 
	  sex = dat_cur$sex, 
	  set = dat_cur$set, 
	  group = dat_cur$group, 
	  page = pageid
	)
	
	contactsheet_key <- rbind(contactsheet_key, contactsheet_tmp)
	
	if(ngroupid == 1) {
		for(p in 1:npages) {
			dese <- which(pageid == u_pageid[p])
			oo <- order(dat_cur[dese, ]$label)
			dat_cur[dese, ] <- dat_cur[dese[oo], ]
		}
		runningpagecount <- drawpages(dat_cur, runningpagecount,
			u_groupid,
			as.character(u_set[s]),
			export
		)
	} else {
		for(p in 1:ngroupid) {
			dese <- which(groupid == u_groupid[p])
			oo <- order(dat_cur[dese, ]$label)
			dat_cur[dese, ] <- dat_cur[dese[oo], ]
		}
		runningpagecount <- drawcombopage(dat_cur, runningpagecount,
			u_groupid,
			as.character(u_set[s]),
			export
		)
	}
}
}
contactsheet_key
}
