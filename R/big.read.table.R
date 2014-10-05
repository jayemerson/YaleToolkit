

big.read.table <- function(file, nrows=100000, sep=",",
                           header=TRUE, row.names=NULL,
                           cols=NULL, rowfilter=NULL,
                           as.is=TRUE, estimate=FALSE)
{  
  if (estimate) {
    nlines <- getnrows(file)
    x <- read.table(file, sep=sep, row.names=row.names,
                    nrows=min(nlines, 1000), header=header)
    if (!is.null(cols)) x <- x[,cols,drop=FALSE]
    cat("Estimated read size without row filtering:",
        floor(object.size(x)*nlines/nrow(x)/1e6), "MB\n")
    if (interactive()) {
      ANSWER <- readline("Continue with read (Y/n)? ")
      if (substring(ANSWER, 1, 1) != "Y") {
        warning("Terminated read.")
        return(NULL)
      }
    }
  }
  
  iter <- iread.table(file, header=header,
                      row.names=row.names, sep=sep,
                      nrows=nrows, as.is=as.is)
  
  ans <- foreach(x=iter, .combine=rbind) %do% {
    if (!is.null(rowfilter)) x <- rowfilter(x)
    if (!is.null(cols)) x <- x[,cols,drop=FALSE]
    gc()
    return(x)
  }
  
  return(ans)
}