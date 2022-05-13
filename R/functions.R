# tcltk::.Tcl("package require autoscroll
# namespace import ::autoscroll::autoscroll")

#' @import tcltk
#' @import tcltk2
#' @import tkRplotR

#'
#' @title Open and Display Image in a Tk Canvas
#' @usage tkImShow(file, zoom = NULL, title = NULL)
#' @description Open and display an image in a canvas that can be zoomed and panned using the mouse and keyboard shortcuts
#' @param file path to image file
#' @param zoom the zoom factor (ratio), for zoom = 1 the image is shown with no zoom (original size), when zoom is < (>) than 1 the image is zoomed out (in). The default value of zoom is NULL.
#' @param title the window title
#' @return The \code{tkwin} object returned by \code{tkImShow} is a toplevel window with a canvas that contains several variables (canvasAllowZoom, canvasScrollWidth) and \code{tkwin} objects (canvas, canvasScrollHorizontal, canvasScrollVertical) placed in the \code{env}, which could be used to implement further methods.
#' @examples
#' \dontshow{
#' library(testthat)
#' file_path <- system.file("img", "example.png", package = "tkImgR")
#' tt <- tkImShow(file_path)
#' expect_equal(class(tt), "tkwin")
#' Sys.sleep(0.25)
#' canvasLeft(tt)
#' Sys.sleep(0.25)
#' canvasControlLeft(tt)
#' expect_true(tkImgR:::.tkCanvasX(tt$env$canvas) < 10)
#' Sys.sleep(0.25)
#' canvasControlUp(tt)
#' expect_true(tkImgR:::.tkCanvasY(tt$env$canvas) < 10)
#' Sys.sleep(0.25)
#' zoomFactorprevious <- tt$env$zoomFactorCurrent
#' .zoomUp(tt)
#' Sys.sleep(0.25)
#' .zoomUp(tt)
#' zoomFactorCurrent <- tt$env$zoomFactorCurrent
#' Sys.sleep(0.25)
#' expect_true(zoomFactorCurrent > zoomFactorprevious)
#'
#' canvasControlRight(tt)
#' Sys.sleep(0.25)
#' .zoomDown(tt)
#' Sys.sleep(0.25)
#' .zoomDown(tt)
#' Sys.sleep(0.25)
#' tcltk::tkdestroy(tt)
#'
#' if (!identical(tcltk::tclRequire("Img", warn = FALSE),FALSE)){
#' file_path1 <- system.file("img", "example.jpg", package = "tkImgR")
#' tt <- tkImShow(file_path1)
#' Sys.sleep(0.25)
#' tcltk::tkdestroy(tt)
#' }
#' }
#'
#' file_path <- system.file("img", "example.png", package = "tkImgR")
#' tt <- tkImShow(file_path)
#'
#' if (!identical(tcltk::tclRequire("Img", warn = FALSE),FALSE)){
#' file_path1 <- system.file("img", "example.jpg", package = "tkImgR")
#' tt <- tkImShow(file_path1)
#' }
#' @export
tkImShow <- function(file,
                     zoom = NULL,
                     title = NULL) {
  if (missing(file)) {
    file <- tk_choose.files(multi = FALSE)
    if (length(file) < 1) {
      return(message("Please supply a image file!!!"))
    }
  } else {
    if (!file.exists(file)) {
      return(message("Please supply a image file!!!"))
    }
  }

  if (is.null(title)) {
    title <- basename(file)
  }

  nextToplevelId <- .getNextToplevelId()
  on.exit(.delayGC())

  tkImage <- .tkimageRead(paste0("tkImgR_", nextToplevelId), file)

  imWidth <- .tkimageWidth(tkImage)
  imHeight <- .tkimageHeight(tkImage)

  tt <- .tkToplevelWithCanvas(alpha = 0)
  tt$env$rawImageName <- tkImage
  .getZoomScale(tt)

  if (is.null(zoom)) {
    zoom <- tt$env$zoomScale[round(length(tt$env$zoomScale) / 2)]
  } else {
    # correct the initial zoom value
    zoom <-
      tt$env$zoomScale[which.min(abs(tt$env$zoomScale - pmin(1, zoom)))]
  }
  #tt$env$ZOOM <- 1 / zoom
  tt$env$filePath <- file
  # .tkWmGeometry(tt,paste0(round(.tkDim()[1]/2),"x", round(.tkDim()[2]/2)))
  .canvasFillFirstTime(tt, pmax(1, zoom), pmax(1, 1 / zoom))
  .tkmaximize(tt)
  #Sys.sleep(0)
  tkconfigure(
    tt$env$canvas,
    yscrollcommand = function(...) {
      tkset(tt$env$canvasScrollVertical, ...)
      .canvasFill(tt)
    },
    xscrollcommand = function(...) {
      tkset(tt$env$canvasScrollHorizontal, ...)
      .canvasFill(tt)
    }
  )
  .Tcl("update idletasks")
  .canvasAddBinds(tt)
  tcl("wm", "attribute", tt, alpha = 1)
  tktitle(tt) <- paste0(title, " [", round(zoom, 2), "x]")
  tkbind(tt, "<Destroy>", "")
  tkbind(tt, "<Destroy>", function(W) {
    toplevelID <- .getToplevelID(W)
    if (!toplevelID %in% .lsVariable()) {
      return()
    }
    tt <- getVariable(toplevelID)
    imageName <- tt$env$rawImageName
    if (tt$ID == getVariable("tkImgR_CurrentToplevelID")) {
      rmVariable("tkImgR_CurrentToplevelID")
    }
    rmVariable(tt$ID)
    .tkImageDeleteByPattern(imageName)
  })

  setVariable(tt$ID, tt)
  # tkconfigure(
  #   tt$env$canvas,
  #   yscrollcommand = function(...) {
  #     tkset(tt$env$canvasScrollVertical, ...)
  #     .canvasFill(tt)
  #   },
  #   xscrollcommand = function(...) {
  #     tkset(tt$env$canvasScrollHorizontal, ...)
  #     .canvasFill(tt)
  #   }
  # )
  tt
}


#'
#' @keywords internal
.argList2tcl <- function(argList) {
  addArgs <- ""
  # for (i in 1:length(argList)) {
  for (i in seq_along(argList)) {
    if (!is.null(argList[[i]])) {
      addArgs <-
        paste(addArgs, paste0("-",
                              names(argList[i]),
                              " ",
                              paste(argList[[i]],
                                    collapse = " ")))
    }
  }
  addArgs
}

#'
#' @keywords internal
.canvasAddBinds <- function(win) {
  tkbind(win, "<Left>", .canvasLeft)
  tkbind(win, "<Right>", .canvasRight)
  tkbind(win, "<Up>", .canvasUp)
  tkbind(win, "<Down>", .canvasDown)

  tkbind(win, "<Control-Left>", .canvasControlLeft)
  tkbind(win, "<Control-Right>", .canvasControlRight)
  tkbind(win, "<Control-Up>", .canvasControlUp)
  tkbind(win, "<Control-Down>", .canvasControlDown)

  tkbind(win, "<MouseWheel>", .canvasMouseWheel)
  tkbind(win, "<Button-4>", .canvasUp)
  tkbind(win, "<Button-5>", .canvasDown)

  tkbind(win, "<Control-MouseWheel>", .canvasControlMouseWheel)
  tkbind(win, "<Control-Button-4>", .canvasLeft)
  tkbind(win, "<Control-Button-5>", .canvasRight)

  tkbind(win, "<plus>", .canvasPlus)
  tkbind(win, "<minus>", .canvasMinus)
  tkbind(win, "<Key-KP_Add>", .canvasPlus)
  tkbind(win, "<Key-KP_Subtract>", .canvasMinus)

  tkbind(win, "<Shift-Button-4>", .canvasShiftButton4)
  tkbind(win, "<Shift-Button-5>", .canvasShiftButton5)
  tkbind(win, "<Shift-MouseWheel>", .canvasShiftMouseWheel)

  tkbind(win, "<KeyPress-space>", .canvasSpace)
  tkbind(win, "<KeyRelease-space>", .canvasSpaceRelease)
  tkbind(win$env$canvas, "<Motion>", .canvasMotion)
}

#'
#' @keywords internal
.canvasControlButton4 <- function(W) {
  W <- .getToplevel(W)
  zoom <- W$env$zoom
  Wc <- .getCanvasID(W)
  tkxview.moveto(Wc, mean(.tcl2num(tkxview(Wc))[1]) - 0.0025 / pmax(0.25, zoom))
  .canvasFill(W)
}

#'
#' @keywords internal
.canvasControlDown <- function(W) {
  .canvasFill(.getToplevel(W), y = 1, force = TRUE)
}

#'
#' @keywords internal
.canvasControlUp <- function(W) {
  .canvasFill(.getToplevel(W), y = 0, force = TRUE)
}

#'
#' @keywords internal
.canvasSpace <- function(W, x, y) {
  W <- .getToplevel(W)
  if (diff(as.numeric(tkget(W$env$canvasScrollHorizontal))) == 1 &
    diff(as.numeric(tkget(W$env$canvasScrollVertical))) == 1) {
    return()
  }
  Wc <- .getCanvasID(W$ID)
  tkconfigure(Wc, cursor = "hand2")
  tkscan.mark(Wc, x, y)
  .setTkVariable(W, "canvasSpace", TRUE)
}

#'
#' @keywords internal
.canvasSpaceRelease <- function(W) {
  tkconfigure(.getCanvasID(W), cursor = "cross")
  .setTkVariable(W, "canvasSpace", FALSE)
}

#'
#' @keywords internal
.canvasMotion <- function(W, x, y) {
  W <- .getToplevel(W)
  Wc <- .getCanvasID(W)
  if (.getTkVariable(W, "canvasSpace")) {
    # if (.isInsideCanvas(Wc, x, y)) {
    if (.getTkVariable(W, "canvasSpaceInvert")) {
      tkscan.dragto(Wc, x, y, -1)
    } else {
      tkscan.dragto(Wc, x, y, 1)
    }
    # }
  }
}

#'
#' @keywords internal
.canvasLeft <- function(W) {
  W <- .getToplevel(W)
  zoom <- W$env$zoom
  Wc <- .getCanvasID(W)
  tkxview.moveto(Wc, mean(.tcl2num(tkxview(Wc))[1]) - 0.0025 / pmax(0.25, zoom))
  .canvasFill(W)
}

#'
#' @keywords internal
.canvasRight <- function(W) {
  W <- .getToplevel(W)
  Wc <- .getCanvasID(W)
  zoom <- W$env$zoom
  tkxview.moveto(Wc, mean(.tcl2num(tkxview(Wc))[1]) + 0.0025 / pmax(0.25, zoom))
  .canvasFill(W)
}

#'
#' @keywords internal
.canvasUp <- function(W) {
  W <- .getToplevel(W)
  Wc <- .getCanvasID(W)
  zoom <- W$env$zoom
  tkyview.moveto(Wc, mean(.tcl2num(tkyview(Wc))[1]) - 0.0025 / pmax(0.25, zoom))
  .canvasFill(W)
}

#'
#' @keywords internal
.canvasDown <- function(W) {
  W <- .getToplevel(W)
  Wc <- .getCanvasID(W)
  zoom <- W$env$zoom
  tkyview.moveto(Wc, mean(.tcl2num(tkyview(Wc))[1]) + 0.0025 / pmax(0.25, zoom))
  .canvasFill(W)
}

#'
#' @keywords internal
.canvasControlRight <- function(W) {
  .canvasFill(.getToplevel(W), x = 1, force = TRUE)
}

#'
#' @keywords internal
.canvasMouseWheel <- function(W, D) {
  D <- as.numeric(D)
  if (D > 0) {
    .canvasUp(W)
  } else {
    .canvasDown(W)
  }
  #.canvasFill(W)
}

#'
#' @keywords internal
.canvasControlLeft <- function(W) {
  .canvasFill(.getToplevel(W), x = 0, force = TRUE)
}


#'
#' @keywords internal
.canvasControlMouseWheel <- function(W, D) {
  D <- as.numeric(D)
  W <- .getToplevel(W)
  Wc <- .getCanvasID(W)
  zoom <- W$env$zoom

  if (D > 0) {
    tkxview.moveto(Wc, mean(.tcl2num(tkxview(Wc))[1]) + 0.0025 / pmax(0.25, zoom))
  } else {
    tkxview.moveto(Wc, mean(.tcl2num(tkxview(Wc))[1]) - 0.0025 / pmax(0.25, zoom))
  }
}

#'
#' @keywords internal
.canvasPlus <- function(W, x, y) {
  if (.getTkVariable(W, "canvasAllowZoom")) {
    .zoomUp(W, x, y)
  }
}

#'
#' @keywords internal
.canvasMinus <- function(W, x, y) {
  if (.getTkVariable(W, "canvasAllowZoom")) {
    .zoomDown(W, x, y)
  }
}

#'
#' @keywords internal
.canvasShiftButton4 <- function(W, x, y) {
  if (.getTkVariable(W, "canvasAllowZoom")) {
    .zoomUp(W, x, y)
  }
}

#'
#' @keywords internal
.canvasShiftButton5 <- function(W, x, y) {
  if (.getTkVariable(W, "canvasAllowZoom")) {
    .zoomDown(W, x, y)
  }
}

#'
#' @keywords internal
.canvasShiftMouseWheel <- function(W, D, x, y) {
  D <- as.numeric(D)
  W <- .getToplevel(W)
  if (D > 0) {
    .zoomUp(W, x, y)
  } else {
    .zoomDown(W, x, y)
  }
}


#'
#' @keywords internal
.tkToplevelWithCanvas <- function(parent = .TkRoot, alpha = 1) {
  tcl(
    "wm",
    "attribute",
    tt <- tktoplevel(
      parent = parent,
      width = 1,
      height = 1,
      borderwidth = 0
    ),
    alpha = alpha
  )

  tt$env$canvasAllowZoom <- TRUE
  tt$env$canvasSpace <- FALSE
  tt$env$canvasSpaceInvert <- FALSE
  tt$env$canvasUpdate <- .tclVarGlobal(paste0("tkImgR_canvasUpdate", tt$ID), "")

  XSCR <- .tkDim()[1]
  YSCR <- .tkDim()[2]
  tt$env$canvas <- tkcanvas(tt)
  tt$env$canvasScrollHorizontal <-
    tkscrollbar(
      tt,
      orient = "horiz",
      relief = "flat",
      command = paste(
        tt$env$canvas$ID,
        "xview"
      )
    )
  tt$env$canvasScrollVertical <-
    tkscrollbar(
      tt,
      orient = "vertical",
      relief = "flat",
      command = paste(
        tt$env$canvas$ID,
        "yview"
      )
    )

  tkgrid(tt$env$canvas, row = 0, column = 0)
  tkgrid.rowconfigure(tt, 0, weight = 1)
  tkgrid.columnconfigure(tt, 0, weight = 1)
  tt$env$screenWidth <- XSCR
  tt$env$screenHeight <- YSCR
  tkgrid(tt$env$canvasScrollHorizontal,
    row = 1,
    column = 0,
    sticky = "we"
  )
  tkgrid(tt$env$canvasScrollVertical,
    row = 0,
    column = 1,
    sticky = "ns"
  )
  #tt$env$canvasScrollHorizontal <-
   tcl("autoscroll::autoscroll", tt$env$canvasScrollHorizontal)
  #tt$env$canvasScrollVertical <-
   tcl("autoscroll::autoscroll", tt$env$canvasScrollVertical)
  tt$env$zoom <- 0
  tt$env$currentImageName <- tclVar()

  tt$env$canvasScrollWidth <-
    .tcl2str(tkconfigure(tt$env$canvasScrollHorizontal, width = NULL))[4]

  if (alpha == 0) {
    return(tt)
  }

  tkwm.deiconify(tt)

  tkconfigure(
    tt$env$canvas,
    yscrollcommand = function(...) {
      tkset(tt$env$canvasScrollVertical, ...)
      .canvasFill(tt)
    },
    xscrollcommand = function(...) {
      tkset(tt$env$canvasScrollHorizontal, ...)
      .canvasFill(tt)
    }
  )
  tt
}

#
# @keywords internal
# getCurrentToplevelId <- function(parent) {
#    as.numeric(substr(parent$ID, 2, 6))
#  }

#'
#' @keywords internal
.getNextToplevelId <- local({
  k <- 1
  function(parent = .TkRoot,
           cancel = FALSE,
           activate = FALSE) {
    out <- parent$env$num.subwin + k
    if (cancel) {
      k <<- 0
      return(out)
    }
    if (activate) {
      k <<- 1
      return(parent$env$num.subwin + k)
    }
    return(out)
  }
})

#'
#' @keywords internal
.delayGC <- function(time = 50) {
  tcl("after", time, .TclCallback(gc))
}


#' @keywords internal
.tkimageCreate <- function(imageName, filePath="", type="photo") {
  # filePath <- tclvalue(.Tcl(paste0("string trimright [file join [file normalize \"", filePath ,"\"] { }]")))
  tcl("image", "create", type, imageName, file = filePath)
}


#' @keywords internal
.tkimageRead <- function(tkImageName,
                         filename,
                         from = NULL,
                         shrink = TRUE,
                         to = NULL,
                         new = TRUE) {
  if (shrink) {
    shrink <- ""
  } else {
    shrink <- NULL
  }
  argList <- list(
    "from" = from,
    "to" = to,
    "shrink" = shrink
  )
  addArgs <- .argList2tcl(argList)
  if (new) {
    tkImageName <- tkimage.create("photo", tkImageName)
  }
  .Tcl(
    paste0(
      tkImageName,
      " read [string trimright [file join [file normalize \"",
      filename,
      "\"] { }]] ",
      addArgs
    )
  )
  tkImageName
}


#' @keywords internal
.tkimageCopy <- function(imageName,
                         sourceImage,
                         new = FALSE,
                         from = NULL,
                         to = NULL,
                         shrink = c(TRUE, FALSE)[1],
                         zoom = 1,
                         subsample = 1,
                         compositingrule = c(NULL, "set", "overlay")[1]) {
  if (shrink) {
    shrink <- ""
  } else {
    shrink <- NULL
  }
  argList <- list(
    "from" = from,
    "to" = to,
    "shrink" = shrink,
    "zoom" = pmax(1, zoom),
    "subsample" = pmax(1, subsample),
    "compositingrule" = compositingrule
  )
  addArgs <- .argList2tcl(argList)
  if (new) {
    imageName <- tkimage.create("photo", imageName)
  }
  .Tcl(paste(imageName, "copy", sourceImage, addArgs))
  imageName
}


#' @keywords internal

.tkimageWrite <- function(im,
                          filename,
                          background = NULL,
                          from = 0,
                          format = "tiff",
                          grayscale = FALSE) {
  .Tcl(paste0(
    im,
    " write ",
    filename,
    " -from ",
    paste(from, collapse = " "),
    # " -format tiff",
    " -grayscale"
  ))
}

#'
#' @keywords internal
.tkimageWidth <- function(im) {
  as.numeric(tkimage.width(im))
}

#'
#' @keywords internal
.tkimageHeight <- function(im) {
  as.numeric(tkimage.height(im))
}


#'
#' @keywords internal
.tkDim <- function(win = ".") {
  .tcl2int(tkwm.maxsize(win))
}

#'
#' @keywords internal
.canvasFillFirstTime <- function(tt,
                                 zoom = 1,
                                 subsample = 1) {
  tt$env$currentImageZoom <- zoom
  tt$env$currentImageSubsample <- subsample
  tt$env$zoomFactorCurrent <-
    which.min(abs(tt$env$zoomScale - zoom / subsample))
  tt$env$zoom <- zoom / subsample
  #tt$env$ZOOM <- subsample / zoom
  tt$env$currentImageName <- .tkimageCreate("tkImageBlank", "")
  tt$env$currentImageWidth <-
    .tkimageWidth(tt$env$rawImageName) * zoom / subsample
  tt$env$currentImageHeight <-
    .tkimageHeight(tt$env$rawImageName) * zoom / subsample
  .canvasFill(
    W = tt,
    zoom = zoom,
    subsample = subsample,
    force = TRUE
  )
}

#'
#' @keywords internal
.canvasFill <- function(W,
                        zoom = NULL,
                        subsample = NULL,
                        x = NULL,
                        y = NULL,
                        force = FALSE) {
  zoomPrevious <- W$env$currentImageZoom
  subsamplePrevious <- W$env$currentImageSubsample
  currentX <- W$env$currentImageSegmentX
  currentY <- W$env$currentImageSegmentY
  if (is.null(zoom)) {
    zoom <- zoomPrevious
  }
  if (is.null(subsample)) {
    subsample <- subsamplePrevious
  }
  if (any(c(zoom != zoomPrevious |
    subsample != subsamplePrevious | force))) {
    .getCenterSegments(W, zoom / subsample)
  }
  if (is.null(x)) {
    x <- mean(.tcl2num(tkxview(W$env$canvas)))
  }
  if (is.null(y)) {
    y <- mean(.tcl2num(tkyview(W$env$canvas)))
  }
  xStart <- W$env$xStart
  xEnd <- W$env$xEnd
  xCenter <- W$env$xCenter

  yStart <- W$env$yStart
  yEnd <- W$env$yEnd
  yCenter <- W$env$yCenter
  nX <-
    .getNextCanvasSegment2Fill(x, currentX, currentX, zoom / subsample, xCenter)
  nY <-
    .getNextCanvasSegment2Fill(y, currentY, currentY, zoom / subsample, yCenter)

  if (any(
    c(
      currentX != nX,
      currentY != nY,
      zoom != zoomPrevious,
      subsample != subsamplePrevious,
      force
    )
  )) {
    from <-
      as.integer(c(xStart[nX], yStart[nY], xEnd[nX], yEnd[nY]))
    to <- from * zoom / subsample

    imTmp <- .tkimageCopy(
      paste0(
        W$env$rawImageName,
        ".zoom",
        zoom,
        ".subsample",
        subsample,
        ".x",
        nX,
        ".y",
        nY
      ),
      W$env$rawImageName,
      new = TRUE,
      zoom = zoom,
      subsample = subsample,
      from = from
    )

    canvasImageCurrent <-
      tcl(W$env$canvas,
          "create",
          "image",
          to[1],
          to[2],
          image = imTmp,
          anchor = "nw")


    imWidth <- .tkimageWidth(W$env$rawImageName) * zoom / subsample
    imHeight <- .tkimageHeight(W$env$rawImageName) * zoom / subsample
    tkconfigure(
      W$env$canvas,
      width = imWidth,
      height = imHeight,
      scrollregion = paste(0,
                           0,
                           imWidth,
                           imHeight)
    )

    if (!force) {
      xCenter1 <- c(0, W$env$xCenter)
      x <- pmax(x, xCenter1[nX])
      x <- pmin(x, xCenter1[nX + 1])
    }
    tkyview.moveto(W$env$canvas, y - diff(.tcl2num(tkyview(W$env$canvas))) /
                     2)
    tkxview.moveto(W$env$canvas, x - diff(.tcl2num(tkxview(W$env$canvas))) /
                     2)

    tkdelete(W$env$canvas, W$env$canvasImageCurrent)
    if (tclvalue(W$env$currentImageName) != tclvalue(imTmp)) {
      tkimage.delete(W$env$currentImageName)
    }

    W$env$canvasImageCurrent <- canvasImageCurrent
    W$env$currentImageSegmentX <- nX
    W$env$currentImageSegmentY <- nY
    W$env$zoom <- zoom / subsample
    #W$env$ZOOM <- subsample / zoom
    W$env$currentImageZoom <- zoom
    W$env$currentImageSubsample <- subsample
    W$env$currentImageName <- imTmp
    W$env$currentImageWidth <-
      .tkimageWidth(W$env$rawImageName) * zoom / subsample
    W$env$currentImageHeight <-
      .tkimageHeight(W$env$rawImageName) * zoom / subsample
    tclvalue(W$env$canvasUpdate) <- zoom / subsample
    setVariable("tkImgR_CurrentToplevelID", W$ID)
    .updateWindow(W)
  }
  .Tcl("update idletasks")
}

#'
#' @keywords internal
.tcl2str <- function(x) {
  unlist(strsplit(tclvalue(x), split = " "))
}

#'
#' @keywords internal
.getZoomScale <- function(tt,
                          imWidth = NULL,
                          imHeight = NULL) {
  if (is.null(imWidth)) {
    imWidth <- .tkimageWidth(tt$env$rawImageName)
  }
  if (is.null(imHeight)) {
    imHeight <- .tkimageHeight(tt$env$rawImageName)
  }
  nFactors <- 13
  zoomFactors <- c(rep(1, 11), 2:5)[1:nFactors]
  subsampleFactors <-
    c(c(400, 200, 100, 50, 20, 10, 5, 4, 3, 2), rep(1, 5))[1:nFactors]

  maxSubsample <- .getZoomFactorMax(imWidth, imHeight)
  n <- pmax(1, max(which(subsampleFactors > maxSubsample)) - 1)
  tt$env$zoomFactors <- zoomFactors[n:nFactors]
  tt$env$subsampleFactors <- subsampleFactors[n:nFactors]
  tt$env$zoomScale <-
    zoomFactors[n:nFactors] / subsampleFactors[n:nFactors]
}

#'
#' @keywords internal
.getZoomFactorMax <- function(imWidth, imHeight) {
  pmax(1, round(max(c(
    imWidth / ((
      .tkDim()[1] #- getVariable("tkImgR_ToplevelGeometry")[3]
    )),
    imHeight / ((
      .tkDim()[2] #- getVariable("tkImgR_ToplevelGeometry")[4]
    ))
  ))))
}





#'
#' @keywords internal
.getCenterSegments <- function(tt, zoom, k = 6) {
  XSCR <- tt$env$screenWidth
  YSCR <- tt$env$screenHeight
  imWidth <- .tkimageWidth(tt$env$rawImageName)
  imHeight <- .tkimageHeight(tt$env$rawImageName)

  xStart <- 0
  xEnd <- imWidth * zoom
  if ((k * XSCR) < (zoom * imWidth)) {
    xStart <-
      unique(c(seq(0, zoom * imWidth - k * XSCR, XSCR), zoom * imWidth - k * XSCR))
    xEnd <- xStart + k * XSCR
  }
  tt$env$xStart <- round(xStart / zoom)
  tt$env$xEnd <- round(xEnd / zoom)
  tt$env$xCenter <- ((xStart + xEnd) / 2) / (imWidth * zoom)

  yStart <- 0
  yEnd <- imHeight * zoom
  if ((k * YSCR) < (zoom * imHeight)) {
    yStart <- unique(c(seq(0, zoom * imHeight - k * YSCR, YSCR), zoom * imHeight - k * YSCR))
    yEnd <- yStart + k * YSCR
  }

  # tt$env$xStart <- round(xStart / zoom)
  # tt$env$xEnd <- round(xEnd / zoom)
  # tt$env$xCenter <- ((xStart + xEnd) / 2) / (imWidth * zoom)

  tt$env$yStart <- round(yStart / zoom)
  tt$env$yEnd <- round(yEnd / zoom)
  tt$env$yCenter <- ((yStart + yEnd) / 2) / (imHeight * zoom)

  tt$env$zoom <- zoom
  #tt$env$ZOOM <- 1 / zoom
}

#'
#' @keywords internal
.tcl2num <- function(x) {
  as.numeric(unlist(strsplit(tclvalue(x), split = " ")))
}

#'
#' @keywords internal
.tcl2int <- function(x) {
  as.integer(unlist(strsplit(tclvalue(x), split = " ")))
}

#'
#' @keywords internal
.getNextCanvasSegment2Fill <- function(x, current, n, zoom, refill) {
    x <- pmin(1, x)
    current <- pmin(length(refill) - 1, current)
    n <- pmin(current, n)
    n <- which.min(abs(x - refill))
    n
  }


#'
#' @keywords internal
.updateWindow <- function(tt) {
  title <- .tcl2str(tktitle(tt))
  if (!is.null(zoom <- tt$env$zoom)) {
    tktitle(tt) <-
      paste0(strsplit(title, "[[]]")[[1]], " [", round(zoom, 2), "x]")
  }
  invisible(tt)
}


#'
#' @keywords internal
.tkmaximize <- function(win) {
  switch(Sys.info()[1],
         Darwin = {
           .Tcl(paste("wm state", win, "normal"))
           tkDim <- .tkDim()
           tcl("wm",
               "geometry",
               win,
               paste0(
                 round(tkDim[1] * 0.75),
                 "x",
                 round(tkDim[2] * 0.75),
                 "+",
                 round(tkDim[1] / 8),
                 "+",
                 round(tkDim[2] / 8)
               ))
         },
         Windows = {
           tkDim <- .tkDim()
           tcl("wm",
               "geometry",
               win,
               paste0(
                 round(tkDim[1] * 0.75),
                 "x",
                 round(tkDim[2] * 0.75),
                 "+",
                 round(tkDim[1] / 8),
                 "+",
                 round(tkDim[2] / 8)
               ))
           .Tcl("update idletask")
           .Tcl(paste("wm state", win, "zoomed"))
           },
         Linux = {
           tkDim <- .tkDim()
           tcl("wm",
               "geometry",
               win,
               paste0(
                 round(tkDim[1] * 0.75),
                 "x",
                 round(tkDim[2] * 0.75),
                 "+",
                 round(tkDim[1] / 8),
                 "+",
                 round(tkDim[2] / 8)
               ))
           .Tcl("update idletask")
           .Tcl(paste("wm attributes", win, "-zoomed 1"))
           }
         )
}


#'
#' @keywords internal
.tkWmGeometry <- function(W) {
  as.integer(strsplit(tclvalue(tcl(
    "wm", "geometry", W
  )), "[+x]")[[1]])
}


#'
#' @keywords internal
.canvasGetWidth <- function(canvas) {
  as.numeric(unlist(strsplit(tclvalue(
    tcl(canvas, "cget", width = NULL)
  ), " ")))
}


#'
#' @keywords internal
.canvasGetHeight <- function(canvas) {
  as.numeric(unlist(strsplit(tclvalue(
    tcl(canvas, "cget", height = NULL)
  ), " ")))
}


#'
#' @keywords internal
.isInsideCanvas <- function(canvas, x, y) {
  xRange <-
    as.numeric(tcl(canvas, "xview")) * .canvasGetWidth(canvas)
  yRange <-
    as.numeric(tcl(canvas, "yview")) * .canvasGetHeight(canvas)
  x <- .tkCanvasX(canvas, x)
  y <- .tkCanvasY(canvas, y)
  all(c(x > xRange[1], x < xRange[2], y > yRange[1], y < yRange[2]))
}

#' @export
#' @keywords internal
.zoomUp <- function(W, x = NULL, y = NULL) {
  tt <- .getToplevel(W)
  if (tt$env$canvasAllowZoom) {
    if (!is.null(x)) {
      x <- .tkCanvasX(tt$env$canvas, x) / (tt$env$currentImageWidth)
    }
    if (!is.null(y)) {
      y <- .tkCanvasY(tt$env$canvas, y) / (tt$env$currentImageHeight)
    }
    .canvasFillByZoomDelta(tt, delta = 1, x = x, y = y)
    # tclvalue(tt$env$canvasUpdate) <- tt$env$zoom
  }
}


#' @export
#' @keywords internal
.zoomDown <- function(W, x = NULL, y = NULL) {
  tt <- .getToplevel(W)
  if (tt$env$canvasAllowZoom) {
    if (!is.null(x)) {
      x <- .tkCanvasX(tt$env$canvas, x) / (tt$env$currentImageWidth)
    }
    if (!is.null(y)) {
      y <- .tkCanvasY(tt$env$canvas, y) / (tt$env$currentImageHeight)
    }
    .canvasFillByZoomDelta(tt, delta = -1, x = x, y = y)
    # tclvalue(tt$env$canvasUpdate) <- tt$env$zoom
  }
}

#'
#' @keywords internal
.getCanvasID <- function(W) {
  .getToplevel(W)$env$canvas
}

#'
#' @keywords internal
.tkCanvasX <- function(canvas, x = 0, gridspacing = NULL) {
  as.numeric(tclvalue(tkcanvasx(canvas, x, gridspacing)))
}

#'
#' @keywords internal
.tkCanvasY <- function(canvas, y = 0, gridspacing = NULL) {
  as.numeric(tclvalue(tkcanvasy(canvas, y, gridspacing)))
}

#'
#' @keywords internal
.canvasFillByZoomDelta <- function(W, x, y, delta = 0) {
  nMax <- length(W$env$zoomScale)
  zoomFactor <-
    pmin(nMax, pmax(1, W$env$zoomFactorCurrent + delta))
  zoom <- W$env$zoomFactors[zoomFactor]
  subsample <- W$env$subsampleFactors[zoomFactor]
  if (W$env$zoomFactorCurrent != zoomFactor) {
    .canvasFill(W,
      zoom,
      subsample,
      x = x,
      y = y,
      force = TRUE
    )
    if (zoom > 1) {
      tkconfigure(W$env$canvasScrollHorizontal, width = 0)
      tkconfigure(W$env$canvasScrollVertical, width = 0)
    } else {
    # scrollWidth <- ifelse(diff(.tcl2num(tkget(
    #   W$env$canvasScrollHorizontal
    # ))) == 1 | zoom > 1, 0, W$env$canvasScrollWidth)
    tkconfigure(W$env$canvasScrollHorizontal, width = W$env$canvasScrollWidth)
    # scrollWidth <- ifelse(diff(.tcl2num(tkget(
    #   W$env$canvasScrollVertical
    # ))) == 1 | zoom > 1, 0, W$env$canvasScrollWidth)
    tkconfigure(W$env$canvasScrollVertical, width = W$env$canvasScrollWidth)
     }
    W$env$zoomFactorCurrent <- zoomFactor
  }
}

#'
#' @keywords internal
.tkImageDeleteByPattern <- function(pattern = "tkImgR_") {
  for (iName in grep(pattern, .tcl2str(.Tcl("image names")), value = TRUE)) {
    tcl("image", "delete", iName)
  }
}

#'
#' @keywords internal
.getTkVariable <- function(W, varName) {
  get(varName, envir = .getToplevel(W)$env)
}

#'
#' @keywords internal
.setTkVariable <- function(W, varName, value = NULL) {
  assign(varName, value, envir = .getToplevel(W)$env)
}

#'
#' @keywords internal
#  addapted from tcltk2 package

.tclVarGlobal <- function(name, value = "") {
  name <- gsub("\\.", "_", name[1])
  .Tcl(paste("global", name))
  # if (!is.character(name))
  #   stop("'name' must be a character!")
  lst <- list(ID = name, env = new.env())
  assign(name, value, envir = lst$env)
  reg.finalizer(lst$env, function(env) tcl("unset", ls(env)))
  class(lst) <- "tclVar"
  tclvalue(lst) <- value
  lst
}
