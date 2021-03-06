% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_helpers.R
\name{to_pdf}
\alias{to_pdf}
\title{Function for repeatedly writing plots to PDFs}
\usage{
to_pdf(PLOT, w, h, mar = rep(1, 4))
}
\arguments{
\item{PLOT}{a function that produces a plot}

\item{w}{the requested width of the PDF}

\item{h}{the requested height of the PDF}

\item{mar}{the margins of the PDF, set by a call to par(mar=mar). Defaults to c(1,1,1,1)}
}
\value{
A function that takens optional arguments to the PLOT function and fname, the name of the PDF. One-off changes to w, h, and mar, are specified with width, height, and margin, respectively
}
\description{
Function for repeatedly writing plots to PDFs
}
