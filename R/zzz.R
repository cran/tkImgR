
.onLoad <- function(libname, pkgname) {
  sysname <- Sys.info()[1]

  switch(
    sysname,
    Darwin = tcltk::addTclPath("/System/Library/Tcl"),
    Windows = tcltk::addTclPath(gsub("\\\\", "/", "C:/ActiveTcl/lib"))
  )
  didLoad <- tcltk::tclRequire("autoscroll", warn = FALSE)
  if (identical(didLoad, FALSE)) {
   # packageStartupMessage("...")
    tclfile <-
      file.path(
        find.package(package = pkgname, lib.loc = libname),
        "tcl",
        "autoscroll.tcl"
      )
    tcl("source", tclfile)
  }
}

.onAttach <- function(libname, pkgname) {
  sysname <- Sys.info()[1]
  if (as.numeric(tcltk::.Tcl("info tclversion")) < 8.5) {
    packageStartupMessage("\n *** tkImgR needs tcl/tk version 8.5 or newer ***\n")
  }

  ## load Img tk extension if available

  switch(
    sysname,
    Darwin = tcltk::addTclPath("/System/Library/Tcl"),
    Windows = tcltk::addTclPath(gsub("\\\\", "/", "C:/ActiveTcl/lib"))
  )

  didLoad <- tcltk::tclRequire("Img", warn = FALSE)

  if (identical(didLoad, FALSE)) {
    packageStartupMessage(
      "The Tk 'Img' extension cannot be loaded. Please install Tk 'Img' to support JPG, TIFF, BMP, and CR2 formats"
    )
  }
}
