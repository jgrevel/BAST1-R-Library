
#' find hidden information in a watermarked image
#'
#' @param p
#'
#' @return returns a list, first element is TRUE or false indicating if the image checked was a watermark image (with info).
#' If FALSE then that is all that is in the list.
#' If TRUE, the second item in the list is a named character vector containing infomation for locating the original file and who made the watermark.

extract_details = function(p)
{
  orig_seed = .Random.seed
  on.exit({.Random.seed = orig_seed})
  set.seed(readBin(charToRaw('hide'),'integer'))
  r = as.raw(p[[1]][1:3,,])
  S = length(r)
  label = 'WM image'
  SP = seq_len(S)

  # read label
  ind = sample(SP,64)
  SP = SP[! SP %in% ind]
  b = r[ind] & as.raw(1)
  if(! identical( packBits(b) , charToRaw(label) ) )
  {
    return(list(isWM = FALSE))
  }

  # find string lengths
  ind = sample(SP,5*32)
  SP = SP[! SP %in% ind]
  b = r[ind] & as.raw(1)
  lens = packBits(b,'integer')

  # read strings
  ind = sample(SP,sum(lens)*8)
  b = r[ind] & as.raw(1)
  details = readChar(packBits(b),lens,TRUE)
  names(details)= c("fullpath", "WMcreator", "comp_used", "time_of_creation", "alternate_path")
  return(list(isWM = TRUE,details=details))
}

#' replace a watermarked image with original
#'
#' @param path character, path to a file or folder containing watermaked images
#' @param folder Logical, whether the path is a folder or a file
#'
#' @return no return, the function will replace watermarked images with originals when and if it finds them
#' @export
#'

unwatermark = function(path,folder=TRUE,verbose=FALSE)
{
  uw = function(fi)
  {
    p = magick::image_read(fi)
    res = extract_details(p)
    if(res$isWM)
    {
      if(file.exists(res$details['fullpath']))
      {
        p = magick::image_read(res$details['fullpath'])
        magick::image_write(p,fi,flatten=TRUE)
        cat(fi,'replaced.\n')
	if(verbose) print(res$details)
      }else if(file.exists(res$details['alternate_path']))
      {
        p = magick::image_read(res$details['alternate_path'])
        magick::image_write(p,fi,flatten=TRUE)
        cat(fi,'replaced.\n')
	if(verbose) print(res$details)
      }else
      {
        warning('Replacement for',fi,'could not be located, check details below')
        print(res$details)
      }
    }
  }
  if(folder)
  {
    pngs = dir(path,'.png$')
    path=paste0(gsub('[\\\\/]$','',path),'\\')
    for(png in pngs)
    {
      uw(paste0(path,png))
    }
  } else uw(path)
}

