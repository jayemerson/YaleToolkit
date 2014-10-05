\name{sparklines}
\alias{sparklines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draws a panel of vertically stacked sparklines}
\description{
Draws a panel of vertically stacked, aligned sparklines, or time series.
}
\usage{
sparklines(ss, times = NULL, overlap = FALSE, yscale = NULL,
           buffer = unit(0, "lines"), buffer.pars = NULL, IQR = NULL,
           ptopts = NULL, yaxis = TRUE, xaxis = "exterior",
           labeled.points = NULL, point.labels = NULL,
           label.just = c(1.2, 0.5), frame.pars = NULL,
           line.pars = gpar(lwd = 1), 
           outer.margin = unit(c(5, 4, 4, 2), "lines"), 
           outer.margin.pars = NULL, main = NULL, sub = NULL, 
           xlab = NULL, ylab = NULL, lcol = NULL, new = TRUE)
}
\arguments{
  \item{ss}{a data frame whose columns give the time series to be plotted}
  \item{overlap}{\code{FALSE} for stacked sparklines; \code{TRUE} for all plotted on the same y-axis.}
  \item{times}{the times at which to plot the data; if \code{NULL} (the default), equal spacing is assumed. All the sparklines must share the same \code{times} argument. If unaligned time series must be plotted, multiple calls to \code{sparklines()} are required.}
  \item{yscale}{either a vector of length 2 giving the y-limits for all sparklines, or a list having the same length as the number of columns in \code{ss} (each component of which is a 2-vector giving the associated sparkline scales). Defaults to \code{NULL}, in which case the scales for each sparkline are set to the sparkline's minimum and maximum values.}
  \item{buffer}{a buffer above the maximum and below the minimum values attained by the sparkline. Defaults to \code{unit(0, 'lines')}.}
  \item{buffer.pars}{a list of graphics parameters describing the buffer area.  See \code{Details} for more information.}
  \item{IQR}{a list of graphics parameters to shade or otherwise delineate the interquartile range of the sparkline. Defaults to \code{NULL}, in which case the IQR is not shown. See \code{Details} for more information.}
  \item{ptopts}{a list of graphics parameters describing the points on the sparkline that are plotted and labelled. In particular the first and last or minimum and maximum points are labeled if \code{ptopts\$labels} is \code{'first.last'} or \code{'min.max'}.}
  \item{yaxis}{draws a vertical axis if \code{TRUE}; defaults to \code{FALSE}, in which case no axis is drawn.}
  \item{xaxis}{\code{'interior'} draws horizontal axes inside the plotting frame (for each sparkline); \code{'exterior'} draws the common axis for all the sparklines outside the plotting frame; defaults to \code{FALSE} (no axis).}
  \item{labeled.points}{not implemented. See \code{ptopts}.}
  \item{point.labels}{not implemented. See \code{ptopts}.}
  \item{label.just}{not implemented. See \code{ptopts}.}
  \item{frame.pars}{a list of graphics parameters describing the exact area taken up by the plotted sparkline.  See \code{Details} for more information.}
  \item{line.pars}{a list of graphics parameters describing the sparkline.  See \code{Details} for more information.}
  \item{outer.margin}{a vector of 4 units (bottom, left, top, right) giving the outer margin sizes in order (around the entire panel of sparklines). Defaults to \code{unit(c(0,0,0,0), 'lines')}.}
  \item{outer.margin.pars}{a list of graphics parameters describing the outer margin.  See \code{Details} for more information.}
  \item{main}{a main title, above the stack of sparklines.}
  \item{sub}{a character vector the length of \code{length(ss)} providing titles for the individual sparklines, printed to the right of the sparklines.}
  \item{xlab}{a string providing the label for the common x-axis or (probably a useless feature) a character vector the length of \code{length(ss)} providing x-axis labels for the individual sparklines.}
  \item{ylab}{a character vector the length of \code{length(ss)} providing y-axis labels for the individual sparklines.}
  \item{lcol}{a vector of colors the same length as the number of columns in ss to color the line. As in base graphics, can be either a vector of strings giving the color names, a numeric vector referring to the current pallette, or the output of functions like \code{\link{hsv}} or \code{\link{rgb}}}
  \item{new}{defaults to \code{TRUE}, which creates a new, empty page; otherwise adds the sparkline to the existing plot.}
}
\details{
    In all the cases where a list of graphics parameters is needed, the valid parameter names are the same as would be valid when passed to \code{\link{gpar}} in the appropriate call. That is, passing \code{list(fill = 'blue', col = 'red')} to \code{margin} gives a margin that is blue with a red border; but adding \code{fontface = 'bold'} will have no effect, just as it would have no effect in a call to \code{\link{grid.rect}}.
}
\note{
  We do not support non-aligned time series plots such as \code{ts.plot(airmiles, Nile, nhtemp)}.
}
\references{Tufte, E. R. (2006) \emph{Beautiful Evidence} Cheshire, Connecticut: Graphics Press.}
\author{John W. Emerson, Walton Green}

\seealso{\code{\link{ts.plot}}, \code{\link{sparkline}}, \code{\link{sparkmat}}}

\examples{

### sparkline examples
data(beaver1)

## The default behaviour of sparklines
sparklines(beaver1)

sparklines(beaver1,
           outer.margin = unit(c(2,4,4,5), 'lines'),
           outer.margin.pars = gpar(fill = 'lightblue'),
           buffer = unit(1, "lines"),
           frame.pars = gpar(fill = 'lightyellow'),
           buffer.pars = gpar(fill = 'lightgreen'),
           yaxis = TRUE, xaxis=FALSE,
           IQR = gpar(fill = 'grey', col = 'grey'),
           main = 'Beaver 1')

data(YaleEnergy)
y <- YaleEnergy[YaleEnergy$name==YaleEnergy$name[2],]
sparklines(y[,c("ELSQFT", "STEAM")], times=y$year+y$month/12,
           main="Branford Electric and Steam Consumption")

## Adding a pair of sparklines to an existing plot

grid.newpage()
pushViewport(viewport(w = 0.8, h = 0.8))
sparklines(data.frame(x = rnorm(10), y = rnorm(10, mean=5)), new = FALSE)
popViewport()

grid.newpage()
pushViewport(viewport(w = 0.8, h = 0.8))
sparklines(data.frame(x = rnorm(10), y = rnorm(10, mean=2)),
                buffer = unit(1, "lines"),
                frame.pars = gpar(fill = 'lightyellow'),
                yaxis = TRUE, xaxis=FALSE,
                IQR = gpar(fill = 'grey', col = 'grey'), new = FALSE)
popViewport()

}
\keyword{ts}
