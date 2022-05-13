#' Commands to zoom and pan the image using the mouse or the keyboard (or by evoking directly the function)
#'
#' Functions to zoom and pan the canvas, and add the bind to the canvas.
#'
#' @param W tktoplevel object with the canvas displaying the image
#' @param ... further arguments.
#' @details These functions define the keyboard and mouse controls for the toplevel window.
#'
#' @return No return value, called for side effects
#'
#' @examples
#' \dontshow{
#' file_path <- system.file("img", "example.png", package = "tkImgR")
#' tt <- tkImShow(file_path)
#' Sys.sleep(0.25)
#' canvasLeft(tt)
#' Sys.sleep(0.25)
#' canvasControlLeft(tt)
#' Sys.sleep(0.25)
#' canvasControlUp(tt)
#' Sys.sleep(0.25)
#' zoomFactorprevious <- tt$env$zoomFactorCurrent
#' .zoomUp(tt)
#' Sys.sleep(0.25)
#' .zoomUp(tt)
#'
#' zoomFactorCurrent <- tt$env$zoomFactorCurrent
#' Sys.sleep(0.25)
#' canvasControlRight(tt)
#' Sys.sleep(0.25)
#' .zoomDown(tt)
#' Sys.sleep(0.25)
#' .zoomDown(tt)
#' Sys.sleep(0.25)
#' tcltk::tkdestroy(tt)
#' }
#' \dontrun{
#' file_path <- system.file("img", "example.png", package = "tkImgR")
#' tt <- tkImShow(file_path)
#' Sys.sleep(0.25)
#' canvasLeft(tt)
#' Sys.sleep(0.25)
#' canvasControlLeft(tt)
#' Sys.sleep(0.25)
#' canvasRight(tt)
#' Sys.sleep(0.25)
#' tcltk::tkdestroy(tt)
#' }
#'
#' @export
#' @rdname tkcanvasCommands
canvasAddBinds <- function(W) {
  .canvasAddBinds(W)
}

#' @export
#' @rdname tkcanvasCommands
canvasControlButton4 <- function(W) {
  .canvasControlButton4(W)
}

#' @export
#' @rdname tkcanvasCommands
canvasControlDown <- function(W) {
  .canvasControlDown(W)
}

#' @export
#' @rdname tkcanvasCommands
canvasControlUp <- function(W) {
  .canvasControlUp(W)
}

#' @export
#' @rdname tkcanvasCommands
canvasSpace <- function(W, ...) {
  .canvasSpace(W, ...)
}

#' @export
#' @rdname tkcanvasCommands
canvasSpaceRelease <- function(W) {
  .canvasSpaceRelease(W)
}

#' @export
#' @rdname tkcanvasCommands
canvasMotion <- function(W, ...) {
  .canvasMotion(W, ...)
}

#' @export
#' @rdname tkcanvasCommands
canvasLeft <- function(W) {
  .canvasLeft(W)
}

#' @export
#' @rdname tkcanvasCommands
canvasRight <- function(W) {
  .canvasRight(W)
}

#' @export
#' @rdname tkcanvasCommands
canvasUp <- function(W) {
  .canvasUp(W)
}

#' @export
#' @rdname tkcanvasCommands
canvasDown <- function(W) {
  .canvasDown(W)
}

#' @export
#' @rdname tkcanvasCommands
canvasControlRight <- function(W) {
  .canvasControlRight(W)
}

#' @export
#' @rdname tkcanvasCommands
canvasMouseWheel <- function(W, ...) {
  .canvasMouseWheel(W, ...)
}

#' @export
#' @rdname tkcanvasCommands
canvasControlLeft <- function(W) {
  .canvasControlLeft(W)
}


#' @export
#' @rdname tkcanvasCommands
canvasControlMouseWheel <- function(W, ...) {
  .canvasControlMouseWheel(W, ...)
}

#' @export
#' @rdname tkcanvasCommands
canvasPlus <- function(W, ...) {
  .canvasPlus(W, ...)
}

#' @export
#' @rdname tkcanvasCommands
canvasMinus <- function(W, ...) {
  .canvasMinus(W, ...)
}

#' @export
#' @rdname tkcanvasCommands
canvasShiftButton4 <- function(W, ...) {
  .canvasShiftButton4(W, ...)
}

#' @export
#' @rdname tkcanvasCommands
canvasShiftButton5 <- function(W, ...) {
  .canvasShiftButton5(W, ...)
}

#' @export
#' @rdname tkcanvasCommands
canvasShiftMouseWheel <- function(W, ...) {
  .canvasShiftMouseWheel(W, ...)
}
