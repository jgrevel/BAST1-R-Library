#library(magick)

#' Hide in infomation in an image
#'
#' @description This function is used by watermark to hide details of the original image and the creation of the watermarked version into the new file.
#' The purpose is to allow this to be extracted later for replacing images in office documents where they are stripped of structure and filename.
#'
#' @param p magick-image where details are to be hidden
#' @param path character string of file path containing original image
#'
#' @return returns a copy of p with the details imbedded into the image

add_details = function(p,path)
{
  orig_seed = .Random.seed
  on.exit({.Random.seed = orig_seed})
  set.seed(readBin(charToRaw('hide'),'integer'))
  pp=p[[1]]
  r = as.raw(pp[1:3,,])
  S = length(r)
  clear=as.raw(254)


  label = 'WM image'

  details = c(
    fullpath = normalizePath(path),     # in case relative paths have been used
    user = Sys.getenv('USERNAME'),
    comp = Sys.getenv('LOGONSERVER'),
    time = as.character(Sys.time(),usetz=TRUE)      # date, time, user and computer may help if path cannot be found later, as it gives a
                                                    # guide to which lab books to check.
  )
  if(grepl('^C:[\\\\/]',details['fullpath'])) # avoid local paths
  {
    reppath = paste0(details['comp'],gsub('^C:','',details['fullpath']))
    if(file.exists(reppath))
      details['fullpath']=reppath

  }
  alt = details['fullpath']
  names(alt)= 'alt_path'
  if(grepl('^\\\\',alt))              # computer name path doesn't always work, add alternative using IP address (mostly for Garrit or Joachim)
  {
    server = gsub('(^\\\\+)|((\\\\[^\\\\]+)+$)','',alt)
    ip = curl::nslookup(server,TRUE)
    alt = gsub(server,ip,alt)
  }
  details=c(details,alt)

  ds=paste(details,collapse='')
  lens = as.integer(nchar(details,'bytes'))
  if(S < 64 + 5*32 + sum(lens)*8){
    warning('image too small to add details of original location')
    return(p)
  }
  SP = seq_len(S)

  # add label
  ind = sample(SP,64)
  SP = SP[! SP %in% ind]
  r[ind] = r[ind] & clear
  r[ind] = r[ind] | rawToBits(charToRaw(label))

  # add string lengths
  ind = sample(SP,5*32)
  SP = SP[! SP %in% ind]
  r[ind] = r[ind] & clear
  r[ind] = r[ind] | intToBits(lens)

  # add strings
  ind = sample(SP,sum(lens)*8)
  r[ind] = r[ind] & clear
  r[ind] = r[ind] | rawToBits(charToRaw(ds))

  pp[1:3,,] = r
  p=magick::image_read(pp)
  return(p)
}

#' Create a copy of a plot with a watermark
#'
#' @description To avoid errors caused by mistakenly using graphs not intended publication, a copy of graphs with a watermark added should
#' be used in draft publications. This function is designed to make a copy of an image and add a watermark to it. The source can be any image
#' file accepted by \link[magick]{image_read}. The function will produce a .png file in the same location with filename identical accept for
#' addition of a prefix/suffix.
#'
#' @author Blesson Chacko and Aaron Hayman.
#' @param path file path of the plot image to have a watermark added to.
#' @param text text to display as the watermark.  Default is 'Preliminary for internal discussion.
#' @param colour colour to be used for watermark, can be partially transparent. Default is '#80808030', a translucent grey.
#' @param prefix A string to add to the start of the filename. Default is "W_".
#' @param suffix A string to add to the end of the filename. The default is empty, this may be used as an alternative to the prefix.
#' @param scale A number to rescale watermark. Default is one, which should take up as much space as possible.
#'
#' @export
#'
#' @examples{
#' \dontrun{
#'
#' files = dir()
#' files=files[grep('f_.+[.]png$',files)]
#' for(f in files) watermark(f)
#'}
#'}
#'
watermark=function(path,
                   text='Preliminary for internal discussion',
                   colour='#80808030',
                   prefix='W_',
                   suffix='',
                   scale=1
)
{
  p = magick::image_read(path)							# read file from path
  fol = gsub( '[^\\\\/]+$' , '', path)						# remove from path everything from end until (and including) / or \, this should be folder

  fname = gsub( '^.*[\\/]' , '', path)					# remove everything up to last / or \, leaves filename
  fname = gsub( '[.][^.]*$' , '', fname)					# remove extension from file name (from end to last .)
  outPath= paste0(fol, prefix, fname, suffix, '.png')			# out path includes prefix and suffix, will always be png.

  lines=strsplit(text,'\n')							# split text into lines
  iminf=magick::image_info(p)							# image info is wanted for height and width
  textW = 2*iminf$width / max(nchar(lines[[1]]))				# estimation for proportion of text width
  textH = 0.75*iminf$height / length(lines[[1]])				# estimation for proportion of text height
  size=min(c(textW,textH))*scale						# size of text estimated to fit within picture. Can have scale
  # adjusted if wrong. (Generally this should not be necessary.)
  p=magick::image_annotate(p,text,gravity='north',color=colour,size=size)
  p=magick::image_flatten(p)
  p = add_details(p,path)
  magick::image_write(p,outPath,flatten=TRUE)				# Add text and save to outPath
}

