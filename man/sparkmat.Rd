\name{sparkmat}
\alias{sparkmat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draws a sparkmat}
\description{
Draws multiple time series (or sparklines) at given locations.
}
\usage{
sparkmat(x, locs = NULL, w = NULL, h = NULL, lcol = NULL,
         yscales = NULL, tile.shading = NULL, 
         tile.margin = unit(c(0, 0, 0, 0), "points"),
         tile.pars = NULL, just = c("right", "top"),
         new = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a list of data frames, all with the same dimensions, one for each panel of
	vertically aligned sparklines.}
  \item{locs}{a data frame with x-coordinates in the first variable and y-coordinates in the second variable, giving locations of each of the \code{length(x)} sparkline panels.}
  \item{w}{vector of unit widths (or native widths if not specified as units).}
  \item{h}{vector of unit heights (or native heights if not specified as units).}
  \item{lcol}{vector of \code{ncol(x[[1]])} line colors, one for each sparkline in each panel.}
  \item{yscales}{either a vector of length 2 giving the y-limits for all sparklines, or a list having the same length as the number of columns in \code{ss} (each component of which is a 2-vector giving scales for the individual sparklines). Defaults to \code{NULL}, in which case the scales for each sparkline are set to its minimum and maximum value within the panel.}
  \item{tile.shading}{vector of background shadings for the panels.}
  \item{tile.margin}{an outer margin around each tile (panel of sparklines). A 4-vector of units giving the bottom, left, top and right margins; defaults to
 \code{unit(c(0,0,0,0), 'points')}.}
  \item{tile.pars}{a list of graphics parameters describing the buffer area.  See \code{Details} for more information.}
  \item{just}{default is \code{c("right", "top")}; controls the justification
of the sparklines relative to the provided location coordinates.}
  \item{new}{defaults to \code{TRUE}, which creates a new, empty page; otherwise adds the sparkline to the existing plot.}
  \item{...}{for arguments to be passed through to \code{sparklines()}.}
}
\details{
  In all the cases where a list of graphics parameters is needed, the valid parameter names are the same as would be valid when passed to \link{gpar} in the appropriate call. That is, passing \code{list(fill = 'blue', col = 'red')} to \code{margin} gives a margin that is blue with a red border; but adding \code{fontface = 'bold'} will have no effect, just as it would have no effect in a call to \code{grid.rect()}.
}
\references{Tufte, E. R. (2006) \emph{Beautiful Evidence} Cheshire, Connecticut: Graphics Press.}
\author{John W. Emerson, Walton Green}
\seealso{\code{\link{ts.plot}}, \code{\link{sparkline}}, \code{\link{sparklines}}}
\examples{

# An example with a time series of energy consumption at Yale colleges.
data(YaleEnergy)
y <- YaleEnergy

# Need list of 12 data frames, each with one time series.

z <- list(data.frame(y[y$name==y$name[1],"ELSQFT"]),
          data.frame(y[y$name==y$name[2],"ELSQFT"]),
          data.frame(y[y$name==y$name[3],"ELSQFT"]),
          data.frame(y[y$name==y$name[4],"ELSQFT"]),
          data.frame(y[y$name==y$name[5],"ELSQFT"]),
          data.frame(y[y$name==y$name[6],"ELSQFT"]),
          data.frame(y[y$name==y$name[7],"ELSQFT"]),
          data.frame(y[y$name==y$name[8],"ELSQFT"]),
          data.frame(y[y$name==y$name[9],"ELSQFT"]),
          data.frame(y[y$name==y$name[10],"ELSQFT"]),
          data.frame(y[y$name==y$name[11],"ELSQFT"]),
          data.frame(y[y$name==y$name[12],"ELSQFT"]))

sparkmat(z, locs=data.frame(y$lon, y$lat), new=TRUE,
         w=0.002, h=0.0002, just=c("left", "top"))
grid.text(y[1:12,1], unit(y$lon[1:12]+0.001, "native"),
          unit(y$lat[1:12]+0.00003, "native"),
          just=c("center", "bottom"), gp=gpar(cex=0.7))
grid.text("Degrees Longitude", 0.5, unit(-2.5, "lines"))
grid.text("Degrees Latitude", unit(-4.5, "lines"), 0.5, rot=90)
grid.text("Monthly Electrical Consumption (KwH/SqFt)",
          0.5, 0.82, gp=gpar(cex=1, font=2))
grid.text("of Yale Residential Colleges",
          0.5, 0.77, gp=gpar(cex=1, font=2))
grid.text("July 1999 - July 2006",
          0.5, 0.72, gp=gpar(cex=1, font=2))

# An example with pressure and high cloud cover over a regular grid of the
# Americas, provided by NASA ().

runexample <- FALSE
if (runexample) {

data(nasa)

grid.newpage()
pushViewport(viewport(w = unit(1, "npc")-unit(2, "inches"),
                      h = unit(1, "npc")-unit(2, "inches")))
v <- viewport(xscale = c(-115, -55),
              yscale = c(-22.5, 37.5))
pushViewport(v)

y <- vector(mode="list", length=24*24)
locs <- as.data.frame(matrix(0, 24*24, 2))
tile.shading <- rep(0, 24*24)
for(i in 1:24) {     # Latitudes
  for(j in 1:24) {   # Longitudes
    y[[(i-1)*24+j]] <- as.data.frame(t(nasa$data[,,i,j]))
    locs[(i-1)*24+j,] <- c(as.numeric(dimnames(nasa$data)$lon[j]),
                           as.numeric(dimnames(nasa$data)$lat[i]))
    tile.shading[(i-1)*24+j] <- gray( 1-.5*(nasa$elev[i,j]/max(nasa$elev)) )
  }
}

yscales <- list(quantile(nasa$data["pressure",,,], c(0.01, 0.99), na.rm=TRUE),
                quantile(nasa$data["cloudhigh",,,], c(0.01, 0.99), na.rm=TRUE))

sparkmat(y, locs=locs, just='center', w=2.5, h=2.5,
         tile.shading=tile.shading, lcol=c(6,3), yscales=yscales,
         tile.margin = unit(c(2,2,2,2), 'points'), new=FALSE)

grid.xaxis(gp=gpar(fontface=2, fontsize=14))
grid.yaxis(gp=gpar(fontface=2, fontsize=14))
grid.rect()

grid.text("Degrees Latitude", x=unit(-0.75, "inches"), y=0.5, rot=90,
          gp=gpar(fontface=2, fontsize=14))
grid.text("Degrees Longitude", x=0.5, y=unit(-0.75, "inches"), rot=0,
          gp=gpar(fontface=2, fontsize=14))
grid.text("Grayscale shading reflects",
          x=unit(1, "npc")+unit(0.6, "inches"), y=0.5, rot=270,
          gp=gpar(fontface=2, fontsize=14))
grid.text("average elevation above sea level",
          x=unit(1, "npc")+unit(0.3, "inches"), y=0.5, rot=270,
          gp=gpar(fontface=2, fontsize=14))

grid.lines(nasa$coast[,1], nasa$coast[,2], default.units = 'native',
           gp = gpar(col = 'black', lwd = 1))

grid.text("Pressure",
          x=0.25, y=unit(1, "npc")+unit(1.25, "lines"),
          gp=gpar(fontface=2, fontsize=14))
grid.rect(x=0.25, y=unit(1, "npc") + unit(0.5, "lines"),
          width=0.4, height=unit(0.05, "inches"), gp=gpar(col=6, fill=6))
grid.text("High Cloud",
          x=0.75, y=unit(1, "npc")+unit(1.25, "lines"),
          gp=gpar(fontface=2, fontsize=14))
grid.rect(x=0.75, y=unit(1, "npc") + unit(0.5, "lines"),
          width=0.4, height=unit(0.05, "inches"), gp=gpar(col=3, fill=3))
}
}
\keyword{ts}
\keyword{ts}
