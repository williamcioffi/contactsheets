#contactsheets_supportfunctions
#30apr2017
#v.7* (superpod edition)

#required packages
require(tools)

#an empty plot on x = [0, 1], y = [0, 1]
stdplot <- function(...) {
	plot(
		0, 0, type = 'n', axes = FALSE, 
		xlim = c(0, 1), ylim = c(0, 1),
		xlab = "", ylab = "",
		...
	)
}

#load file with appropraite package
loadimagefile <- function(f, type) {
  switch(type,
    TIFF = readTIFF(f),
    JPEG = readJPEG(f),
    PNG  = readPNG(f)
  )
}

#returns titlecase and fixes dumb shit like o malley
titlecase_irish <- function(s) {
	s_prime <- toTitleCase(s)
	paste(toupper(substring(s_prime, 1, 1)), substring(s_prime, 2), sep = "")
}

#actually draw all the pages for a group
drawpages <- function(dat, runningpagecount, groupname, setname, export, aspect, scaleimage, verbose = FALSE, height, width) {
	#get the actual dimensions of a standard plot
	stdplot()
	dims <- par()$usr
	dev.off()
	xl <- dims[1]
	yb <- dims[3]
	xr <- dims[2]
	yt <- dims[4]
	
	#set up the sheet
	sheet <- matrix(1:(gPAGEROWS*gPAGECOLS), gPAGEROWS, gPAGECOLS,
		byrow = TRUE
	)

	nfiles <- nrow(dat)
	npages <- ceiling(nfiles / gIMAGESPERPAGE)
	i <- 1
	
	for(p in 1:npages) {
		runningpagecount <- runningpagecount + 1
if(export) {
		pdf(paste(setname, "-", runningpagecount, ".pdf", sep = ""), width = width, height = height)
}
		par(oma = c(2, 0, 4, 0), mar = rep(gMARGINBUFFER, 4))
		layout(sheet)
		for(count in 1:gIMAGESPERPAGE) {
			
			#center it if you're at the end and have one left over
			if(i == nfiles && (count - 1) %% gPAGECOLS == 0 & gPAGECOLS == 3) {
				stdplot()
			}
			
if(verbose) {
print(as.character(dat$fnames[i]))
print(as.character(dat$fformat[i]))
}
		  img <- loadimagefile(as.character(dat$fnames[i]), as.character(dat$fformat[i]))
			
			r <- as.raster(img[,,1:3])	
			if(scaleimage) {
				stdplot(asp = aspect)
				rasterImage(r, xl, yb, xr, yt)
			} else {
				plot(r)
			}
			
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
				
			shadowtext(
				xx + dat$byear_x_nudge[i], yy + dat$byear_y_nudge[i] + gLABELCHEAT,
				paste("\n\n\n",
					dat$id[i], pad, "\n",
					dat$sex[i], pad, sep =""
				),
				pos = labpos,
				cex = dat$textsize[i], col = as.character(dat$byearcolor[i]),
				r = gSHADOWTEXTSCALE	
			)
			
			shadowtext((xr-xl)/2 + dat$label_x_nudge[i], yb + dat$label_y_nudge[i],
				dat$label[i],
				bty = 'n', pos = 3, cex = dat$textsize[i], col = as.character(dat$labelcolor[i]),
				r = gSHADOWTEXTSCALE
			)
			
			i <- i + 1
			if(i > nfiles) {
				break
			}
		}
		mtext(paste(runningpagecount), side = 3, at = 0.95, line = 0.8, outer = TRUE, cex = gPAGENUMBERSIZE)
		mtext(groupname, side = 3, line = 0.8, outer = TRUE, cex = gTITLESIZE)
if(export) {
		dev.off()
}
	}
	
	runningpagecount
}