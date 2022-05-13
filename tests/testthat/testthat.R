library(testthat)
library(tkImgR)
library(tcltk)

# test_check("tkImgR")
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

if (!identical(tclRequire("Img", warn = FALSE),FALSE)){
  file_path1 <- system.file("img", "example.jpg", package = "tkImgR")
  tt1 <- tkImShow(file_path1)
  Sys.sleep(0.25)
  tcltk::tkdestroy(tt1)
}


file_path <- system.file("img", "example.png", package = "tkImgR")
tkimageCreate("tkImage01")
expect_equal(tkImgR:::.tkimageWidth("tkImage01"), 0)
im1 <- tkimageCreate("tkImage01", file_path)
expect_equal(tkImgR:::.tkimageWidth(im1),10128)
expect_equal(.tkimageHeight(im1),2824)
im2 <- tkimageRead("tkImage02", file_path)
c("tkImage01","tkImage02") %in% as.character(tcltk::.Tcl("image names"))
c(as.character(im1),as.character(im2)) %in% as.character(tcltk::.Tcl("image names"))

file_path_crop_image <- file.path(tempdir(check = TRUE), "crop.png")
tkimageWrite(im1, file_path_crop_image, from=c(0,1500))
im1_crop <- tkimageRead("tkImage01_crop", file_path_crop_image)

tkimage.height(im1) #2824
tkimage.height(im1_crop) #1324 = 2824 - 1500
expect_equal(tkImgR:::.tkimageHeight(im1_crop),1324)
tkimageDelete("tkImage02")
c("tkImage01","tkImage02") %in% as.character(tcltk::.Tcl("image names"))
im3 <- tkimageCreate("tkImage03")
tkimageCopy(im3, "tkImage01")
c("tkImage01","tkImage03") %in% as.character(tcltk::.Tcl("image names"))
tkimageDelete(im1)
tkimageDelete(im3)
tkimageDelete(im1_crop)

