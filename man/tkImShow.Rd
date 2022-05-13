
\name{tkImShow}
\alias{tkImShow}
\title{Open and Display Image in a Tk Canvas}
\usage{
tkImShow(file, zoom = NULL, title = NULL)
}
\arguments{
\item{file}{path to image file}

\item{zoom}{the zoom factor (ratio), for zoom = 1 the image is shown with no zoom (original size), when zoom is < (>) than 1 the image is zoomed out (in). The default value of zoom is NULL.}

\item{title}{the window title}
}
\value{
The \code{tkwin} object returned by \code{tkImShow} is a toplevel window with a canvas that contains several variables (canvasAllowZoom, canvasScrollWidth) and \code{tkwin} objects (canvas, canvasScrollHorizontal, canvasScrollVertical) placed in the \code{env}, which could be used to implement further methods.
}
\description{
Open and display an image in a canvas that can be zoomed and panned using the mouse and keyboard shortcuts
}
\examples{
\dontshow{
library(testthat)
file_path <- system.file("img", "example.png", package = "tkImgR")
tt <- tkImShow(file_path)
expect_equal(class(tt), "tkwin")
Sys.sleep(0.25)
canvasLeft(tt)
Sys.sleep(0.25)
canvasControlLeft(tt)
expect_true(tkImgR:::.tkCanvasX(tt$env$canvas) < 10)
Sys.sleep(0.25)
canvasControlUp(tt)
expect_true(tkImgR:::.tkCanvasY(tt$env$canvas) < 10)
Sys.sleep(0.25)
zoomFactorprevious <- tt$env$zoomFactorCurrent
.zoomUp(tt)
Sys.sleep(0.25)
.zoomUp(tt)
zoomFactorCurrent <- tt$env$zoomFactorCurrent
Sys.sleep(0.25)
expect_true(zoomFactorCurrent > zoomFactorprevious)

canvasControlRight(tt)
Sys.sleep(0.25)
.zoomDown(tt)
Sys.sleep(0.25)
.zoomDown(tt)
Sys.sleep(0.25)
tcltk::tkdestroy(tt)

if (!identical(tcltk::tclRequire("Img", warn = FALSE),FALSE)){
file_path1 <- system.file("img", "example.jpg", package = "tkImgR")
tt <- tkImShow(file_path1)
Sys.sleep(0.25)
tcltk::tkdestroy(tt)
}
}

file_path <- system.file("img", "example.png", package = "tkImgR")
tt <- tkImShow(file_path)

if (!identical(tcltk::tclRequire("Img", warn = FALSE),FALSE)){
file_path1 <- system.file("img", "example.jpg", package = "tkImgR")
tt <- tkImShow(file_path1)
}
}
