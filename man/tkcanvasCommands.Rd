\name{canvasAddBinds}
\alias{canvasAddBinds}
\alias{canvasControlButton4}
\alias{canvasControlDown}
\alias{canvasControlUp}
\alias{canvasSpace}
\alias{canvasSpaceRelease}
\alias{canvasMotion}
\alias{canvasLeft}
\alias{canvasRight}
\alias{canvasUp}
\alias{canvasDown}
\alias{canvasControlRight}
\alias{canvasMouseWheel}
\alias{canvasControlLeft}
\alias{canvasControlMouseWheel}
\alias{canvasPlus}
\alias{canvasMinus}
\alias{canvasShiftButton4}
\alias{canvasShiftButton5}
\alias{canvasShiftMouseWheel}
\title{Commands to zoom and pan the image using the mouse or the keyboard (or by evoking directly the function)}
\usage{
canvasAddBinds(W)

canvasControlButton4(W)

canvasControlDown(W)

canvasControlUp(W)

canvasSpace(W, ...)

canvasSpaceRelease(W)

canvasMotion(W, ...)

canvasLeft(W)

canvasRight(W)

canvasUp(W)

canvasDown(W)

canvasControlRight(W)

canvasMouseWheel(W, ...)

canvasControlLeft(W)

canvasControlMouseWheel(W, ...)

canvasPlus(W, ...)

canvasMinus(W, ...)

canvasShiftButton4(W, ...)

canvasShiftButton5(W, ...)

canvasShiftMouseWheel(W, ...)
}
\arguments{
\item{W}{tktoplevel object with the canvas displaying the image}

\item{...}{further arguments.}
}
\value{
No return value, called for side effects
}
\description{
Functions to zoom and pan the canvas, and add the bind to the canvas.
}
\details{
These functions define the keyboard and mouse controls for the toplevel window.
}
\examples{
\dontrun{

file_path <- system.file("img", "example.png", package = "tkImgR")
tt <- tkImShow(file_path)
Sys.sleep(0.25)
canvasLeft(tt)
Sys.sleep(0.25)
canvasControlLeft(tt)
Sys.sleep(0.25)
canvasRight(tt)
Sys.sleep(0.25)
tcltk::tkdestroy(tt)
}

}
