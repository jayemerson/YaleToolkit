\name{YaleEnergy}
\alias{YaleEnergy}
\docType{data}
\title{Monthly energy consumption of Yale residential colleges.}
\description{
  The data set contains monthly energy time series for Yale residential
college, from July 1999 through July 2006
}
\usage{data(YaleEnergy)}
\format{
  A data frame with 1020 observations on the following 18 variables.
  \describe{
    \item{\code{name}}{a factor with levels \code{BERKELEY} \code{BRANFORD} \code{CALHOUN} \code{DAVENPORT} \code{EZRA STILES} \code{JONATHAN EDWARDS} \code{MORSE} \code{PIERSON} \code{SAYBROOK} \code{SILLIMAN} \code{TIMOTHY DWIGHT} \code{TRUMBULL}}
    \item{\code{address}}{a factor with levels \code{189 ELM ST.} \code{205 ELM ST.} \code{241 ELM ST.} \code{242 ELM ST.} \code{248 YORK ST.} \code{261 PARK ST.} \code{302 YORK ST.} \code{345 TEMPLE ST.} \code{505 COLLEGE ST.} \code{70 HIGH ST.} \code{74 HIGH ST.}}
    \item{\code{gsf}}{gross square footage of the college}
    \item{\code{EL}}{electrical consumption in kilowatt hours}
    \item{\code{ELSQFT}}{electrical consumption per square foot}
    \item{\code{CHW}}{chilled water consumption in tons}
    \item{\code{SQFTCHW}}{square feet per ton of chilled water}
    \item{\code{STEAM}}{steam consumption in pounds}
    \item{\code{STEAMSQFT}}{steam per square foot}
    \item{\code{MBTU}}{million British Thermal Units (BTU) from chilled water and steam}
    \item{\code{MBTUSQFT}}{million BTUs per square foot}
    \item{\code{year}}{year of the record}
    \item{\code{month}}{month of the record}
    \item{\code{lon}}{degrees longitude of the college}
    \item{\code{lat}}{degrees latitude}
  }
}
\source{
  John W. Emerson, Yale University
}
\examples{
data(YaleEnergy)
whatis(YaleEnergy)

y <- YaleEnergy             # This is just for convenience.
esqft <- list(data.frame(y[y$name==y$name[1],"ELSQFT"]),
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

# The sparkmat() command does most of the work:
sparkmat(esqft, locs=data.frame(y$lon, y$lat), new=TRUE,
         w=0.002, h=0.0002, just=c("left", "top"))

# We'll add some text for a nice finished product:
grid.text(y[1:12,1], unit(y$lon[1:12]+0.001, "native"),
          unit(y$lat[1:12]+0.00003, "native"),
          just=c("center", "bottom"), gp=gpar(cex=0.7))
grid.text("Degrees Longitude", 0.5, unit(-2.5, "lines"))
grid.text("Degrees Latitude", unit(-4.5, "lines"), 0.5, rot=90)
grid.text("Monthly Electrical Consumption (KwH/SqFt) of Yale Colleges",
          0.5, 0.8, gp=gpar(cex=1, font=2))
grid.text("July 1999 - July 2006",
          0.5, 0.74, gp=gpar(cex=1, font=2))
}
\keyword{datasets}
