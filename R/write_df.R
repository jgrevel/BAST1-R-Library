
#' Write data.frames to File, using defaults typical for NONMEM data.
#'
#' @author Aaron Hayman
#' @description This function is essencially a wrapper to write.table. I sets defaults to be more typical to the work done at BAST.
#' Such that the output file should be a .csv file without quotes or row names. Additionally, when quote is FALSE it also checks the
#' delimiter character is not found in any text strings. It can be set to replace these automatically with or without warnings.
#'
#' Much of this help document is shamelessly copied and pasted from the \link[utils]{write.table}'s documentation.
#'
#' @param x the object to be written, preferably a matrix or data frame. If not, it is attempted to coerce x to a data frame
#' @param file either a character string naming a file or a connection open for writing. "" indicates output to the console
#' @param append logical. Only relevant if file is a character string. If TRUE, the output is appended to the file. If FALSE, any existing file of the name is destroyed.
#' @param quote a logical value (TRUE or FALSE) or a numeric vector. If TRUE, any character or factor columns will be surrounded by double quotes. If a numeric vector, its elements are taken as the indices of columns to quote. In both cases, row and column names are quoted if they are written. If FALSE (default), nothing is quoted
#' @param sep the field separator string. Values within each row of x are separated by this string. This has comma "," as default.
#' @param rep is a replacement to be used when the separator character is found in one of the fields. If this is null then an error will be thrown if the seperator is found in a field.
#' @param warn sets whether a warning is issued when replacements are made.
#' @param eol the character(s) to print at the end of each line (row). For example, eol = "\\r\\n" will produce Windows' line endings on a Unix-alike OS, and eol = "\\r" will produce files as expected by Excel:mac 2004.
#' @param na the string to use for missing values in the data.
#' @param dec the string to use for decimal points in numeric or complex columns: must be a single character.
#' @param row.names either a logical value indicating whether the row names of x are to be written along with x, or a character vector of row names to be written.
#' @param col.names either a logical value indicating whether the column names of x are to be written along with x, or a character vector of column names to be written. See the section on 'CSV files' for the meaning of col.names = NA.
#' @param qmethod a character string specifying how to deal with embedded double quote characters when quoting strings. Must be one of "escape" (default for write.table), in which case the quote character is escaped in C style by a backslash, or "double" (default for write.csv and write.csv2), in which case it is doubled. You can specify just the initial letter.
#' @param fileEncoding character string: if non-empty declares the encoding to be used on a file (not a connection) so the character data can be re-encoded as they are written. See \link[base]{file}.
#' @param allowNAs logical: if set to FALSE will throw a warning (regardless of warn) if any NAs are found in the data.
#' @param allowEmptyCols logical: if set to FALSE will throw an error when a column is found to contain no data.
#'
#' @details see \link[utils]{write.table} for details
#'
#' @export
#'
#' @examples
#' \dontrun{
#' randomString=function(n)
#' {
#'   l=as.integer(rlnorm(n)*3)
#'   #ind = rep(seq_along(l),l)
#'   r=sample(as.raw(44:127),sum(l),TRUE)
#'   char = readChar(r,l)
#'   return(char)
#' }
#' set.seed(100)
#' d=data.frame(a=1:100,b=sample(letters,100,TRUE),c=runif(100),d=TRUE,e=NaN,f='Blah, di, and blah',g=randomString(100),stringsAsFactors = TRUE)
#' df=data.frame(a=1:100,b=sample(letters,100,TRUE),c=runif(100),d=TRUE,e=NaN,f='Blah, di, and blah',g=randomString(100),stringsAsFactors = FALSE)
#'
#' write_df(df,'d.csv',rep=';')
#' }
write_df=function(x, file = "", append = FALSE, quote = FALSE, sep = ",", rep=NULL,warn=TRUE,
                 eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE,
                 qmethod = c("escape", "double"), fileEncoding = "", allowNAs = FALSE, allowEmptyCols = FALSE)
{
  dfname = deparse(substitute(x))[1]
  if(identical(sep,rep)) stop("sep should not be the same as rep, see help... ?write_df")
  if(!quote)
  {
    if(col.names)
    {
      found = any(grepl(sep, colnames(x) ))
      if(found && is.null(rep))
      {
        stop(paste0('The seperator string "', sep, '" was found in column names of ', dfname,'. No replacement was suggested.'))
      }
      if(found && !is.null(rep))
      {
        colnames(x)=gsub(sep, rep, colnames(x) )
        if(warn)
        {
          warning(paste0('The seperator string "', sep, '" was found in column names of ', dfname,'. Replaced with "', rep, '".'))
        }
      }
    }
    if(row.names)
    {
      found = any(grepl(sep, rownames(x) ))
      if(found && is.null(rep))
      {
        stop(paste0('The seperator string "', sep, '" was found in row names of ', dfname,'. No replacement was suggested.'))
      }
      if(found && !is.null(rep))
      {
        rownames(x)=gsub(sep, rep, rownames(x) )
        if(warn)
        {
          warning(paste0('The seperator string "', sep, '" was found in row names of ', dfname,'. Replaced with "', rep, '".'))
        }
      }
    }


    for(i in seq_len(ncol(x)))
    {
      if(class(x[,i])=='factor')
      {
        found = any(grepl(sep,levels(x[,i])))
        if(found && is.null(rep))
        {
          stop(paste0('The seperator string "', sep, '" was found in ', dfname, '$', names(x)[i], '. No replacement was suggested.'))
        }
        if(found && !is.null(rep))
        {
          levels(x[,i])=gsub(sep,rep,levels(x[,i]))
          if(warn)
          {
            warning(paste0('The seperator string "', sep, '" was found in ', dfname, '$', names(x)[i], '. Replaced with "', rep, '".'))
          }
        }

      }else if(class(x[,i])=='character')
      {
        found = any(grepl(sep,x[,i]))
        if(found && is.null(rep))
        {
          stop(paste0('The seperator string "', sep, '" was found in ', dfname, '$', names(x)[i], '. No replacement was suggested.'))
        }
        if(found && !is.null(rep))
        {
          x[,i]=gsub(sep,rep,x[,i])
          if(warn)
          {
            warning(paste0('The seperator string "', sep, '" was found in ', dfname, '$', names(x)[i], '. Replaced with "', rep, '".'))
          }
        }

      }

    }
  }
  if(!allowNAs)
  {
    if(any(is.na(x)))
    {
      NAcols = colnames(x)[as.logical(colSums(is.na(x)))]
      NAcols = paste(NAcols,collapse=', ')
      warning(paste0('NAs were found in the following columns:\n',NAcols))
    }
  }
  if(!allowEmptyCols)
  {
    EmptyCols = colSums(is.na(x))==nrow(x)
    if(any(EmptyCols))
    {
      EmptyColNames=paste(colnames(x)[EmptyCols],collapse=', ')
      stop(paste0('The following columns contain no data:\n',EmptyColNames))
    }


  }
  write.table(x, file, append, quote, sep, eol, na, dec, row.names, col.names, qmethod, fileEncoding)
}


