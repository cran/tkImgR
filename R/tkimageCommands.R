#' Tk commands to deal with images
#'
#' These commands create, read, copy, write, and delete images using the 'tcltk' package.
#'
#' @param imageName Specifies the name for the image; if is NULL then Tk picks a name of the form image#, where # is an integer.
#' @param sourceImage The name (or the tcl object) of the image to be copied.
#' @param fileName The path for the image file.
#' @param ... Further arguments.
#'
#' @return tclObj with the image if the function is \code{tkimageCreate}, \code{tkimageRead}, and \code{tkimageCopy} or no value for \code{tkimageWrite} or \code{tkimageDelete}
#'
#' @examples
#' \dontshow{
#' library(testthat)
#' file_path <- system.file("img", "example.png", package = "tkImgR")
#' tkimageCreate("tkImage01")
#' expect_equal(tkImgR:::.tkimageWidth("tkImage01"), 0)
#' im1 <- tkimageCreate("tkImage01", file_path)
#' expect_equal(tkImgR:::.tkimageWidth(im1),10128)
#' expect_equal(tkImgR:::.tkimageHeight(im1),2824)
#' im2 <- tkimageRead("tkImage02", file_path)
#' c("tkImage01","tkImage02") %in% as.character(tcltk::.Tcl("image names"))
#' c(as.character(im1),as.character(im2)) %in% as.character(tcltk::.Tcl("image names"))
#'
#' file_path_crop_image <- file.path(tempdir(check = TRUE), "crop.png")
#' if (file.access(file_path_crop_image)==0){
#' tkimageWrite(im1, file_path_crop_image, from=c(0,1500))
#' im1_crop <- tkimageRead("tkImage01_crop", file_path_crop_image)
#'
#' print(tkimage.height(im1)) #2824
#' print(tkimage.height(im1_crop)) #1324 = 2824 - 1500
#' tkimageDelete(im1_crop)
#' }
#' #file_path_crop_image <- tempfile("crop", fileext = ".png")
#' #tkimageWrite(im1, file_path_crop_image, from=c(0,1500))
#' #im1_crop <- tkimageRead("tkImage01_crop", file_path_crop_image)
#'
#' tkimage.height(im1) #2824
#' #tkimage.height(im1_crop) #1324 = 2824 - 1500
#' #expect_equal(tkImgR:::.tkimageHeight(im1_crop),1324)
#' tkimageDelete("tkImage02")
#' c("tkImage01","tkImage02") %in% as.character(tcltk::.Tcl("image names"))
#' im3 <- tkimageCreate("tkImage03")
#' tkimageCopy(im3, "tkImage01")
#' c("tkImage01","tkImage03") %in% as.character(tcltk::.Tcl("image names"))
#' tkimageDelete(im1)
#' tkimageDelete(im3)
#' #tkimageDelete(im1_crop)
#' }

#' @export
#' @rdname tkimageCommands
#' @examples
#' #tkimageRead
#' file_path <- system.file("img", "example.png", package = "tkImgR")
#' im01 <- tkimageRead("tkImage01", file_path)
#' "tkImage01" %in% as.character(tcltk::.Tcl("image names"))
#' tkimageDelete(im01)
#'
tkimageRead <- function(imageName = NULL,
                        fileName, ...) {
  .tkimageRead(imageName, fileName, ...)
}

#' @rdname tkimageCommands
#' @export
#' @examples
#'
#' #tkimageCreate
#' file_path <- system.file("img", "example.png", package = "tkImgR")
#' im1 <- tkimageCreate("tkImage01")
#' tkimage.height(im1) #0
#' im1 <- tkimageCreate("tkImage01", file_path)
#' tkimage.height(im1) #2824
#' "tkImage01" %in% as.character(tcltk::.Tcl("image names"))
#' tkimageDelete(im1)
#'
tkimageCreate <- function(imageName = NULL, ...) {
  .tkimageCreate(imageName, ...)
}


#' @export
#' @rdname tkimageCommands
#' @examples
#'
#' #tkimageCopy
#' file_path <- system.file("img", "example.png", package = "tkImgR")
#' im1 <- tkimageCreate("tkImage01", file_path)
#' im3 <- tkimageCreate("tkImage03")
#' tkimageCopy(im3, "tkImage01")
#' c("tkImage01","tkImage03") %in% as.character(tcltk::.Tcl("image names"))
#' tkimageDelete(im1)
#' tkimageDelete(im3)
#'
tkimageCopy <- function(imageName,
                        sourceImage,
                        ...) {
  .tkimageCopy(imageName, sourceImage, ...)
}

#' @export
#' @rdname tkimageCommands
#' @examples
#'
#' #tkimageWrite
#' file_path <- system.file("img", "example.png", package = "tkImgR")
#' im1 <- tkimageCreate("tkImage01", file_path)
#' file_path_crop_image <- file.path(tempdir(check = TRUE), "crop.png")
#' #if is possible to write the file
#' if (file.access(file_path_crop_image)==0){
#'  tkimageWrite(im1, file_path_crop_image, from=c(0,1500))
#'  im1_crop <- tkimageRead("tkImage01_crop", file_path_crop_image)
#'  print(tkimage.height(im1)) #2824
#'  print(tkimage.height(im1_crop)) #1324 = 2824 - 1500
#'  tkimageDelete(im1_crop)
#' }
#'
tkimageWrite <- function(imageName,
                         fileName, ...) {
  invisible(.tkimageWrite(imageName, fileName, ...))

}

#' @export
#' @rdname tkimageCommands
#' @examples
#'
#' #tkimageDelete
#' file_path <- system.file("img", "example.png", package = "tkImgR")
#' im1 <- tkimageCreate("tkImage01", file_path)
#'  "tkImage01" %in% as.character(tcltk::.Tcl("image names"))
#'  tkimageDelete(im1)
#'   "tkImage01" %in% as.character(tcltk::.Tcl("image names"))
#'
tkimageDelete <- function(imageName) {
  invisible(tcl("image", "delete", imageName))
}
