\name{tkimageRead}
\alias{tkimageRead}
\alias{tkimageCreate}
\alias{tkimageCopy}
\alias{tkimageWrite}
\alias{tkimageDelete}
\title{Tk commands to deal with images}
\usage{
tkimageRead(imageName, fileName, ...)

tkimageCreate(imageName, ...)

tkimageCopy(imageName, sourceImage, ...)

tkimageWrite(imageName, fileName, ...)

tkimageDelete(imageName)
}
\arguments{
\item{imageName}{The name of the image.}

\item{fileName}{The path for the image file.}

\item{...}{Further arguments.}

\item{sourceImage}{The name (or the tcl object) of the image to be copied.}
}
\value{
tclObj with the image if the function is \code{tkimageCreate}, \code{tkimageRead}, and \code{tkimageCopy} or no value for \code{tkimageWrite} or \code{tkimageDelete}
}
\description{
These commands create, read, copy, write, and delete images using the 'tcltk' package.
}
\examples{
\dontrun{

file_path <- system.file("img", "example.png", package = "tkImgR")
tkimageCreate("tkImage01")
im1 <- tkimageCreate("tkImage01", file_path)
im2 <- tkimageRead("tkImage02", file_path)
c("tkImage01","tkImage02") \%in\% as.character(tcltk::.Tcl("image names"))
c(as.character(im1),as.character(im2)) \%in\% as.character(tcltk::.Tcl("image names"))

file_path_crop_image <- file.path(tempdir(check = TRUE), "crop.png")
tkimageWrite(im1, file_path_crop_image, from=c(0,1500))
im1_crop <- tkimageRead("tkImage01_crop", file_path_crop_image)

tkimage.height(im1) #2824
tkimage.height(im1_crop) #1324 = 2824 - 1500

tkimageDelete("tkImage02")
c("tkImage01","tkImage02") \%in\% as.character(tcltk::.Tcl("image names"))
im3 <- tkimageCreate("tkImage03")
tkimageCopy(im3, "tkImage01")
c("tkImage01","tkImage03") \%in\% as.character(tcltk::.Tcl("image names"))
tkimageDelete(im1)
tkimageDelete(im3)
tkimageDelete(im1_crop)
}



}
