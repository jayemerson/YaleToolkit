\name{sparkline}
\alias{sparkline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draws a sparkline}
\description{
Draws a times series or `sparkline' in a compact iconic fashion suitable for inclusion in more complex graphics or text.}
\usage{
sparkline(s, times = NULL, ylim = NULL, buffer = unit(0, "lines"),
          margins = NULL, IQR = NULL, yaxis = FALSE, xaxis = FALSE,
          ptopts = list(points = NULL, labels = NULL, labels.ch = NULL,
          gp = NULL, just = NULL, pch = NULL), margin.pars = NULL,
          buffer.pars = NULL, frame.pars = NULL, line.pars = gpar(lwd = 1),
          main = NULL, sub = NULL, xlab = NULL, ylab = NULL, new = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{a vector or time series (class \code{"ts"} or \code{"zoo"}) giving the data to be plotted. If \code{s} is a time series, the \code{start}, \code{end}, and \code{frequency} found in \code{attributes(s)$tsp} are automatically converted into an argument to \code{times}.}
  \item{times}{the times at which to plot the data; if \code{NULL} (the default), equal spacing is assumed, equivalent to setting \code{times = 1:length(s)}.}
  \item{ylim}{the maximum and minimum value on the y-axis; if \code{NULL}, defaults to the actual maximum and minimum of the data.}
  \item{buffer}{a buffer above the maximum and below the minimum values attained by the sparkline. Defaults to \code{unit(0, 'lines')}.}
  \item{margins}{margins around the sparkline-plus-buffer area. NULL (the default) provides no margins; the value passed must be a 4-vector of units giving the bottom, left, top and right margins in that order.}
  \item{IQR}{a list of graphics parameters to shade or otherwise delineate the interquartile range of the sparkline. \code{NULL} (the default), does not show the IQR. See \code{Details} for more information.}
  \item{yaxis}{draws a vertical axis if \code{TRUE}; defaults to \code{FALSE} in which case no axis is drawn.}
  \item{xaxis}{\code{'interior'} draws a horizontal axis inside the plotting frame; \code{'exterior'} outside the plotting frame (in the margins); defaults to \code{FALSE}, in which case no axis is drawn.}
  \item{ptopts}{a list of graphics parameters describing the points on the sparkline that are plotted and labelled. In particular the first and last or minimum and maximum points are labeled if \code{ptopts$labels} is \code{'first.last'} or \code{'min.max'}. In addition to \code{labels}, other relevant parameters from \code{\link{gpar}} should be valid. See \code{Details} for more information.}
  \item{margin.pars}{a list of graphics parameters describing the margin area. See \code{Details} for more information.}
  \item{buffer.pars}{a list of graphics parameters describing the buffer area. See \code{Details} for more information.}
  \item{frame.pars}{a list of graphics parameters describing the exact area taken up by the plotted sparkline. See \code{Details} for more information.}
  \item{line.pars}{a list of graphics parameters describing the sparkline. See \code{Details} for more information.}
  \item{main}{a main title, above the sparkline.}
  \item{sub}{a subtitle, to the right of the sparkline.}
  \item{xlab}{a string to label the x-axis.}
  \item{ylab}{a string to label the y-axis.}
  \item{new}{defaults to \code{TRUE}, which creates a new, empty page; otherwise adds the sparkline to an existing plot.}
}
\details{
  In all the cases where a list of graphics parameters is needed, the valid parameter names are the same as would be valid when passed to \code{\link{gpar}} in the appropriate call. That is, passing \code{list(fill = 'blue', col = 'red')} to \code{margin} gives a margin that is blue with a red border; but adding \code{fontface = 'bold'} will have no effect, just as it would have no effect in a call to \code{grid.rect()}. In particular, note that \code{ptopts} takes the following non-standard parameters: \code{labels}, a vector indexing the points to label or the string \code{'min.max'} or \code{'first.last'}; \code{labels.ch}, a vector of strings giving the labels; and \code{points}, a vector indexing the points at which points should be plotted. Passing \code{'min.max'} or \code{'first.last'} to \code{ptopts$labels} overrides any values of \code{ptopts$labels.ch}.
}
\references{Tufte, E. R. (2006) {/it Beautiful Evidence} Cheshire, Connecticut: Graphics Press.}
\author{John W. Emerson, Walton Green}
\note{This is primarily intended to be called by other functions (\code{sparklines()} and \code{sparkmat()}), but it can also be used as an alternative to \code{ts.plot()}.  Thanks to Gabor Grothendieck for suggesting the generalization that provides support of \code{"zoo"} objects.} 

%Make other sections like Warning with \section{Warning }{....}

\seealso{\code{\link{ts.plot}}, \code{\link{sparklines}}, \code{\link{sparkmat}}}
\examples{

### sparkline examples
data(nhtemp)

## The default behaviour of sparkline

sparkline(nhtemp)

## Creating stand-alone plots

sparkline(rnorm(10),
                buffer = unit(1, "lines"),
                ptopts = 'first.last',
                margins = unit(c(1,1,1,1), 'inches'),
                yaxis = TRUE, xaxis=TRUE,
                IQR = gpar(fill = 'grey', col = 'grey'),
                main = "Ten Random Standard Normal Numbers",
                sub = '...plotted here')

data(YaleEnergy)
y <- YaleEnergy[YaleEnergy$name==YaleEnergy$name[2],]
sparkline(y$ELSQFT, times=y$year+y$month/12,
          xaxis=TRUE, yaxis=TRUE, main="Branford College Electrical Consumption",
          buffer=unit(1, "lines"), margins = unit(c(1, 1, 1, 1), 'inches'))

sparkline(Nile,
                buffer = unit(1, "lines"),
                ptopts = list(labels = 'min.max'),
                margin.pars = gpar(fill = 'lightblue'),
                buffer.pars = gpar(fill = 'lightgreen'),
                frame.pars = gpar(fill = 'lightyellow'),
                yaxis = TRUE, xaxis=TRUE,
                IQR = gpar(fill = 'grey', col = 'grey'),
                main="Nile Discharge between 1871 and 1970",
                sub='In what units?')

## Adding a sparkline to an existing plot

grid.newpage()
pushViewport(viewport(w = 0.8, h = 0.8))
sparkline(rnorm(10),
                buffer = unit(1, "lines"),
                margins = unit(c(4,4,4,4),'points'),
                ptopts = list(labels = 'min.max'),
                margin.pars = gpar(fill = 'lightblue'),
                buffer.pars = gpar(fill = 'lightgreen'),
                frame.pars = gpar(fill = 'lightyellow'),
                yaxis = TRUE, xaxis=TRUE,
                IQR = gpar(fill = 'grey', col = 'grey'),
                main="Title (plotted OUTSIDE the viewport)", new = FALSE)
popViewport()

}
\keyword{ts}
