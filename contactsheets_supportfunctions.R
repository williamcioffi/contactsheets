###
# extra functions for contactsheets
# wrc

# required packages
require(tiff)
require(png)
require(jpeg)
require(tools)
require(TeachingDemos)

#an empty plot on x = [0, 1], y = [0, 1]
stdplot <- function(...) {
	plot(
		0, 0, type = 'n', axes = FALSE, 
		xlim = c(0, 1), ylim = c(0, 1),
		xlab = "", ylab = "",
		...
	)
}

# load file with appropriate package
loadimagefile <- function(f, type) {
result = tryCatch({	
	switch(type,
		TIFF = tiff::readTIFF(f),
		JPEG = jpeg::readJPEG(f),
		PNG  = png::readPNG(f)
	)
}, error = function(e) {
	if(length(grep("unable to open", paste(e))) == 0) {
		e <- paste(e, "file: ", f, sep = "")
	}
	stop(e)
})
}

# returns titlecase and can handle some types of compound irish names
# this is very specific and should have more different kinds of names added
titlecase_irish <- function(s) {
	s_prime <- tools::toTitleCase(s)
	paste(toupper(substring(s_prime, 1, 1)), substring(s_prime, 2), sep = "")
}

# actually draw all the pages for a group
drawpages <- function(dat, runningpagecount, groupname, setname, export) {
	stdplot()
	dims <- par()$usr
	dev.off()
	xl <- dims[1]
	yb <- dims[3]
	xr <- dims[2]
	yt <- dims[4]
	
	sheet <- matrix(1:(gPAGEROWS*gPAGECOLS), gPAGEROWS, gPAGECOLS,
		byrow = TRUE
	)

	nfiles <- nrow(dat)
	npages <- ceiling(nfiles / gIMAGESPERPAGE)
	i <- 1
	
	for(p in 1:npages) {
		runningpagecount <- runningpagecount + 1
		if(export)
			pdf(paste(setname, "-", runningpagecount, ".pdf", sep = ""), width = 8.5, height = 11)
		par(oma = c(2, 0, 4, 0), mar = rep(gMARGINBUFFER, 4))
		layout(sheet)
		for(count in 1:gIMAGESPERPAGE) {
			if(i == nfiles && (count - 1) %% gPAGECOLS == 0) stdplot()
			displayimage(i, dat, xl, yb, xr, yt)
			i <- i + 1
			if(i > nfiles) break
		}
		
		mtext(paste(runningpagecount), side = 3, at = 0.95, line = 0.8, outer = TRUE, cex = gPAGENUMBERSIZE)
		mtext(groupname, side = 3, line = 0.8, outer = TRUE, cex = gTITLESIZE)
		
		if(export) dev.off()
	}
	
	runningpagecount
}



displayimage <- function(i, dat, xl, yb, xr, yt) {
	img <- loadimagefile(as.character(dat$fnames[i]), as.character(dat$fformat[i]))
	r <- as.raster(img[,,1:3])
		
if(gVERBOSE) {
print(as.character(dat$fnames[i]))
print(as.character(dat$fformat[i]))
}

	stdplot(asp = 1)
	rasterImage(r, xl, yb, xr, yt)
	xx <- xl
	yy <- yt
	xjus <- 0
	pad <- ""
	labpos <- 4
	
	if(dat$side[i] == "R") {
		xx <- xr
		xjus <- 1
		pad <- ""
		labpos <- 2
	}
		
	TeachingDemos::shadowtext(
		xx + dat$byear_x_nudge[i], yy + dat$byear_y_nudge[i] + gLABELCHEAT,
		paste("\n",
		  # "\n\n\n",
			# dat$byear[i], pad, "\n",
			dat$sex[i], ", ", dat$pyear[i], pad, sep = ""
		),
		pos = labpos,
		cex = dat$textsize[i], col = as.character(dat$byearcolor[i]),
		r = gSHADOWTEXTSCALE
	)
	
	TeachingDemos::shadowtext((xr-xl)/2 + dat$label_x_nudge[i], yb + dat$label_y_nudge[i],
		dat$label[i],
		bty = 'n', pos = 3, cex = dat$textsize[i], col = as.character(dat$labelcolor[i]),
		r = gSHADOWTEXTSCALE
	)
}




# draw a page with two groups on it (this is really only for male alliances)
drawcombopage <- function(dat, runningpagecount, groupname, setname, export) {
	runningpagecount <- runningpagecount + 1
	
	# get the actual dimensions of a standard plot
	stdplot()
	dims <- par()$usr
	dev.off()
	xl <- dims[1]
	yb <- dims[3]
	xr <- dims[2]
	yt <- dims[4]
	
	#set up the sheet
	sheet <- matrix(1:(gPAGEROWS*gPAGECOLS), gPAGEROWS, gPAGECOLS, byrow = TRUE)

	nfiles <- nrow(dat)
	npages <- ceiling(nfiles / gIMAGESPERPAGE) #this is always 1
	ngroups <- length(groupname) #this is always 2
	
	grouplabel1 <- groupname[1]
	grouplabel2 <- groupname[2]
	
	nphotos1 <- length(which(dat$group == grouplabel1))
	nphotos2 <- length(which(dat$group == grouplabel2))
	
	if(export)
		pdf(paste(setname, "-", runningpagecount, ".pdf", sep = ""), width = 8.5, height = 11)
	par(oma = c(2, 0, 4, 0), mar = rep(gMARGINBUFFER, 4))
	layout(sheet)
	
	i <- 1
	for(count in 1:nphotos1) {
		if(i == (nphotos1 + nphotos2) && (count - 1) %% gPAGECOLS == 0) stdplot()
		displayimage(i, dat, xl, yb, xr, yt)
		i <- i + 1
		if(i > nphotos1) break
	}
	
	leftover <- gPAGECOLS - (nphotos1 %% gPAGECOLS)
	if(leftover == 3) leftover = 0
	rowsused <- ceiling(nphotos1 / gPAGECOLS)
	rowsneed <- ceiling(nphotos2 / gPAGECOLS)
	pushrows <- gPAGEROWS - rowsused - rowsneed
	push <- pushrows*3 + leftover
	
	for(q in 1:push) stdplot()
	
	for(count in 1:nphotos2) {
		if(i == (nphotos1 + nphotos2) && (count - 1) %% gPAGECOLS == 0) stdplot()
		displayimage(i, dat, xl, yb, xr, yt)
		i <- i + 1
		if(i > (nphotos1 + nphotos2)) break
	}

	secondtitleline <- 0.8 - (pushrows + rowsused)*19.4
	mtext(paste(runningpagecount), side = 3, at = 0.95, line = 0.8, outer = TRUE, cex = gPAGENUMBERSIZE)
	mtext(groupname[1], side = 3, line = 0.8, outer = TRUE, cex = gTITLESIZE)
	mtext(groupname[2], side = 3, line = secondtitleline, outer = TRUE, cex = gTITLESIZE)

	if(export) dev.off()
	runningpagecount
}
