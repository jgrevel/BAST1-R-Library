#' Fast reading of .sas7bdat files.
#'
#' @author Aaron Hayman
#' @param file a character string containing the path of the .sas7bdat file.
#' @param column_information a character string or object which will be assigned the column description information.
#' @return A data frame with containing the .sas7bdat data and column descriptions assigned to column_information object.
# @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @name read_sas7bdat
#' @examples
#' \donttest{
#' dat = read_TAB(p = "...\\data.sas7bdat",column_information = cols)
#' }



##################################################################################################
####                                     read_sas7bdat                                             ####
##################################################################################################
# reads .sas7bdat files about 10 times faster.
# returns dataframe with column descriptions as an attribute called column.decription
  read_sas7bdat = function(file,column_information)
  {
    if('sas7bdat'%in%installed.packages()[,'Package']==FALSE) install.packages("sas7bdat")

    # working functions

    '%:%'=function(a,b)			# a simplified less robust version of the one found in the BAST R functions
    {
      if(length(a)==0 | length(b)==0)
      {
        return(integer(0))
      }else
      {
        a=as.integer(a)
        b=as.integer(b)
        if(length(a)==1)a=rep(a,length(b))
        if(length(b)==1)b=rep(b,length(a))
        l=1L+b-a
        ll=sum(l)
        r=rep(a,l)+1:ll-rep(cumsum(c(0L,l[-length(l)])),l)-1L
        return(r)
      }
    }

    read_column_attributes_df = function (col_attr)	# modification of sas7bdat:::read_column_attributes
    {									# designed to cut out nested for loops and return data.frame insteac of list
      info=data.frame(offset=integer(0),length=integer(0),type=character(0),stringsAsFactors=FALSE)
      for (subh in col_attr) {
        i = 1:((subh$length - 20)/12)
        base <- 12 + (i - 1) * 12
        new=data.frame(
          offset = readBin(subh$raw[rep(base,each=4)+1:4], 'integer',
                           4*length(base),4),
          length = readBin(subh$raw[rep(base,each=4)+5:8], 'integer',
                           4*length(base),4),
          type = c("numeric", "character")[readBin(subh$raw[rep(base,each=2)+11:12], 'integer',
                                                   2*length(base),2)],
          stringsAsFactors=FALSE
        )
        info=rbind(info,new)
      }
      return(info)
    }
    readBin_n=function(raw,len)					# feed function to readBin for numeric data, takes care of columns using
    {								# less than 8 bytes per row.
      w=which(len<8)
      if(length(w)>0)
      {
        len=rep(len,length(raw)/sum(len))
        w=which(len<8)
        #cat(paste(len,collapse=','),'\n')
        ind2=1:length(raw)
        ind1=rep(1:length(len),len)
        diff=8L-len[w]
        add=raw(sum(diff))
        ind2=c(ind2,integer(sum(diff)))
        ind1=c(ind1,rep(w,diff))
        #cat(length(ind1),length(ind2),'\n')
        raw=c(raw,add)[order(ind1,ind2)]
        len[w]=8
      }

      readBin(raw,'numeric',length(raw),len)
    }


    #functions							# these functions are lifted from sas7bdat package environment
    check_magic_number=function (data)
      identical(data[1:length(MAGIC)], MAGIC)

    read_raw=function (buf, off, len)
      readBin(buf[(off + 1):(off + len)], "raw", len, 1)

    read_bin=function (buf, off, len, type)
      readBin(buf[(off + 1):(off + len)], type, 1, len)

    read_str=function (buf, off, len)
      read_bin(buf, off, len, "character")

    read_flo=function (buf, off, len)
      read_bin(buf, off, len, "double")

    read_int=function (buf, off, len)
      read_bin(buf, off, len, "integer")

    read_subheaders=function (page)
    {
      subhs <- list()
      subh_total <- 0
      if (!(page$type %in% PAGE_META_MIX_AMD))
        return(subhs)
      for (i in 1:page$subh_count) {
        subh_total <- subh_total + 1
        base <- 24 + (i - 1) * 12
        subhs[[subh_total]] <- list()
        subhs[[subh_total]]$page <- page$page
        subhs[[subh_total]]$offset <- read_int(page$data, base,
                                               4)
        subhs[[subh_total]]$length <- read_int(page$data, base +
                                                 4, 4)
        if (subhs[[subh_total]]$length > 0) {
          subhs[[subh_total]]$raw <- read_raw(page$data, subhs[[subh_total]]$offset,
                                              subhs[[subh_total]]$length)
          subhs[[subh_total]]$signature <- read_raw(subhs[[subh_total]]$raw,
                                                    0, 4)
        }
      }
      return(subhs)
    }

    get_subhs=function (subhs, signature)
    {
      keep <- sapply(subhs, function(subh) {
        identical(subh$signature, signature)
      })
      subhs[keep]
    }

    read_column_names=function (col_name, col_text)
    {
      names <- list()
      name_count <- 0
      for (subh in col_name) {
        for (i in 1:((subh$length - 20)/8)) {
          name_count <- name_count + 1
          names[[name_count]] <- list()
          base <- 12 + (i - 1) * 8
          txt <- read_int(subh$raw, base, 2)
          off <- read_int(subh$raw, base + 2, 2) + 4
          len <- read_int(subh$raw, base + 4, 2)
          names[[name_count]]$name <- read_str(col_text[[txt +
                                                           1]]$raw, off, len)
        }
      }
      return(names)
    }

    read_column_labels_formats=function (col_labs, col_text)
    {
      if (length(col_labs) < 1)
        return(NULL)
      labs <- list()
      for (i in 1:length(col_labs)) {
        labs[[i]] <- list()
        base <- 34
        txt <- read_int(col_labs[[i]]$raw, base, 2)
        off <- read_int(col_labs[[i]]$raw, base + 2, 2) + 4
        len <- read_int(col_labs[[i]]$raw, base + 4, 2)
        if (len > 0)
          labs[[i]]$format <- read_str(col_text[[txt + 1]]$raw,
                                       off, len)
        base <- 40
        txt <- read_int(col_labs[[i]]$raw, base, 2)
        off <- read_int(col_labs[[i]]$raw, base + 2, 2) + 4
        len <- read_int(col_labs[[i]]$raw, base + 4, 2)
        if (len > 0)
          labs[[i]]$label <- read_str(col_text[[txt + 1]]$raw,
                                      off, len)
      }
      return(labs)
    }


    #data								# these data sets are lifted from sas7bdat package environment
    KNOWNHOST=sas7bdat:::KNOWNHOST
    PAGE_META_MIX_AMD=sas7bdat:::PAGE_META_MIX_AMD
    SUBH_ROWSIZE=sas7bdat:::SUBH_ROWSIZE
    SUBH_COLSIZE=sas7bdat:::SUBH_COLSIZE
    SUBH_COLTEXT=sas7bdat:::SUBH_COLTEXT
    SUBH_COLATTR=sas7bdat:::SUBH_COLATTR
    SUBH_COLNAME=sas7bdat:::SUBH_COLNAME
    SUBH_COLLABS=sas7bdat:::SUBH_COLLABS
    PAGE_ANY=sas7bdat:::PAGE_ANY
    PAGE_MIX=sas7bdat:::PAGE_MIX
    PAGE_MIX_DATA=sas7bdat:::PAGE_MIX_DATA
    MAGIC=sas7bdat:::MAGIC

    #main function
    # dependence on chron package has been removed

    if (inherits(file, "connection") && isOpen(file, "read")) {
      con <- file
      close_con <- FALSE
    }else if (is.character(file)) {
      con <- file(file, "rb")
      close_con <- TRUE
    }else {
      stop("invalid 'file' argument")
    }
    header <- readBin(con, "raw", 288, 1)
    if (length(header) < 288)
      stop("header too short (not a sas7bdat file?)")
    if (!check_magic_number(header))
      stop(paste("magic number mismatch", BUGREPORT))
    align1 <- read_raw(header, 32, 1)
    if (identical(align1, as.raw(51))) {
      align1 <- 4
    }else {
      align1 <- 0
    }
    align2 <- read_raw(header, 35, 1)
    if (identical(align2, as.raw(51))) {
      align2 <- 4
    }else {
      align2 <- 0
    }
    endianess <- read_raw(header, 37, 1)
    if (identical(endianess, as.raw(1))) {
      endianess <- "little"
    }else {
      endianess <- "big"
      stop("big endian files are not supported")
    }
    winunix <- read_str(header, 39, 1)
    if (identical(winunix, "1")) {
      winunix <- "unix"
    }else if (identical(winunix, "2")) {
      winunix <- "windows"
    } else {
      winunix <- "unknown"
    }
    timestamp <- read_flo(header, 164 + align1, 8)
    timestamp <- as.POSIXct(timestamp,origin='1960-01-01')		# POSIXct is now used instead of chron function, result is different, but I think previously was incorrect
    header_length <- read_int(header, 196 + align2, 4)
    header <- c(header, readBin(con, "raw", header_length - 288,
                                1))
    if (length(header) < header_length)
      stop("header too short (not a sas7bdat file?)")
    page_size <- read_int(header, 200 + align2, 4)
    if (page_size < 0)
      stop(paste("page size is negative", BUGREPORT))
    page_count <- read_int(header, 204 + align2, 4)
    if (page_count < 1)
      stop(paste("page count is not positive", BUGREPORT))
    SAS_release <- read_str(header, 216 + align1 + align2, 8)
    SAS_host <- read_str(header, 224 + align1 + align2, 8)
    if (!(SAS_host %in% KNOWNHOST))
      stop(paste("unknown host", SAS_host, BUGREPORT))
    OS_version <- read_str(header, 240 + align1 + align2, 16)
    OS_maker <- read_str(header, 256 + align1 + align2, 16)
    OS_name <- read_str(header, 272 + align1 + align2, 16)
    pages <- list()
    for (page_num in 1:page_count) {
      pages[[page_num]] <- list()
      pages[[page_num]]$page <- page_num
      pages[[page_num]]$data <- readBin(con, "raw", page_size,
                                        1)
      pages[[page_num]]$type <- read_int(pages[[page_num]]$data,
                                         16, 2)
      if (pages[[page_num]]$type %in% PAGE_META_MIX_AMD)
        pages[[page_num]]$subh_count <- read_int(pages[[page_num]]$data,
                                                 20, 2)
    }
    subhs <- list()
    for (page in pages) subhs <- c(subhs, read_subheaders(page))
    row_size <- get_subhs(subhs, SUBH_ROWSIZE)
    if (length(row_size) != 1)
      stop(paste("found", length(row_size), "row size subheaders where 1 expected",
                 BUGREPORT))
    row_size <- row_size[[1]]
    row_length <- read_int(row_size$raw, 20, 4)
    row_count <- read_int(row_size$raw, 24, 4)
    col_count_p1 <- read_int(row_size$raw, 36, 4)
    col_count_p2 <- read_int(row_size$raw, 40, 4)
    row_count_fp <- read_int(row_size$raw, 60, 4)
    col_size <- get_subhs(subhs, SUBH_COLSIZE)
    if (length(col_size) != 1)
      stop(paste("found", length(col_size), "column size subheaders where 1 expected",
                 BUGREPORT))
    col_size <- col_size[[1]]
    col_count_6 <- read_int(col_size$raw, 4, 4)
    col_count <- col_count_6
    col_text <- get_subhs(subhs, SUBH_COLTEXT)
    if (length(col_text) < 1)
      stop(paste("no column text subheaders found", BUGREPORT))
    if ("SASYZCRL" == read_str(col_text[[1]]$raw, 16, 8))
      stop(paste("file uses unsupported CHAR compression"))
    col_attr <- get_subhs(subhs, SUBH_COLATTR)
    if (length(col_attr) < 1)
      stop(paste("no column attribute subheaders found", BUGREPORT))
    col_attr <- read_column_attributes_df(col_attr)					# col_attr is now a data.frame and not a list
    if (nrow(col_attr) != col_count) 							# updated to use col_attr as data.frame
      stop(paste("found", nrow(col_attr), "column attributes where", 		# updated to use col_attr as data.frame
                 col_count, "expected", BUGREPORT))
    col_name <- get_subhs(subhs, SUBH_COLNAME)
    if (length(col_name) < 1)
      stop(paste("no column name subheaders found", BUGREPORT))
    col_name <- read_column_names(col_name, col_text)
    if (length(col_name) != col_count)
      stop(paste("found", length(col_name), "column names where",
                 col_count, "expected", BUGREPORT))
    col_labs <- get_subhs(subhs, SUBH_COLLABS)
    col_labs <- read_column_labels_formats(col_labs, col_text)
    if (is.null(col_labs))
      col_labs <- list(length = col_count)
    if (length(col_labs) != col_count)
      stop(paste("found", length(col_labs), "column formats and labels",
                 col_count, "expected", BUGREPORT))
    col_expl=data.frame(									# easy format to View column labels and names
      names=as.character(unlist(col_name)),
      description=as.character(unlist(sapply(
        col_labs,
        function(x)
        {
          if(length(x$label)==0)return('') else return(x$label)
        }
      )))
    )
    col_info <- list()
    for (i in 1:col_count) col_info[[i]] <- c(col_name[[i]],
                                              col_attr[i,], col_labs[[i]])							# updated to use col_attr as data.frame
    for (page_num in 1:page_count) if (!(pages[[page_num]]$type %in%
                                         PAGE_ANY))
      stop(paste("page", page_num, "has unknown type:", pages[[page_num]]$type,
                 BUGREPORT))
    #    data <- list()										# this is no longer required, data will be
    #    for (col in col_info) if (col$length > 0) 						# created later, directly as a data.frame
    #        data[[col$name]] <- vector(col$type, length = row_count)
    #    row <- 0											# row is no longer counted


    if (close_con) 										# connection can be closed here
      close(con)
    numraw=raw(row_count*sum(col_attr$length[col_attr$type=='numeric']))	# empty raw vector for all numeric data
    charaw=raw(row_count*sum(col_attr$length[col_attr$type=='character']))	# empty raw vector for all string data
    npos=0											# count of cells filled from numraw
    cpos=0											# count of cells filled from charaw
    #iter=0

    for (page in pages) {
      #itime=Sys.time()
      #iter=iter+1

      if (!(page$type %in% PAGE_MIX_DATA))
        next
      if (page$type %in% PAGE_MIX) {
        row_count_p <- row_count_fp
        base <- 24 + page$subh_count * 12
        base <- base + base%%8
      } else {
        row_count_p <- read_int(page$data, 18, 4)
        base <- 24
      }
      if (row_count_p > row_count)
        row_count_p <- row_count
      base <- as.integer(base + (seq_len(row_count_p) - 1)*row_length)			# base is now a vector
      nums=col_attr[col_attr$type=='numeric',]					# col_attr of numeric cols
      num_index=rep(rep(base,each=nrow(nums))+					# position of all numeric raw data in current page
                      nums$offset,rep(nums$length,length(base)))+1%:%nums$length
      nraw=page$data[num_index]							# raw numeric data from current page
      numraw[npos+1:length(nraw)]=nraw						# raw data is put into numraw at current position
      npos=npos+length(nraw)								# position is updated

      char=col_attr[col_attr$type=='character',]				# col_attr of character cols
      char_index=rep(rep(base,each=nrow(char))+					# position of all character raw data in current page
                       char$offset,rep(char$length,length(base)))+1%:%char$length
      craw=page$data[char_index]							# raw character data from current page
      charaw[cpos+1:length(craw)]=craw						# raw data is put into charaw at current position
      cpos=cpos+length(craw)								# position is updated




      ##itertimei=as.numeric(Sys.time()-itime,units='secs')		# this section was used for ploting iteration times, for checking only
      ##		data[row+1:nrow(temp),]=temp
      ##		row=row+nrow(temp)
      #itertime=as.numeric(Sys.time()-itime,units='secs')
      #
      #if(iter==1)
      #{
      #	plot(iter,itertime,xlim=c(0,length(pages)),ylim=c(0,5*itertime))
      #	#plot(iter,object.size(data),xlim=c(0,length(pages)),ylim=c(0,25*object.size(data)))
      #}else{
      #	points(iter,itertime)
      #	#points(iter,object.size(data))
      #}
      #points(iter,itertimei,col='green')							# this is code from original function
      #       for (row in (row + 1):(row + row_count_p)) {
      #           for (col in col_info) {
      #               off <- base + col$offset
      #               if (col$length > 0) {
      #                 raw <- read_raw(page$data, off, col$length)
      #                 if (col$type == "numeric" && col$length < 8) {
      #                   raw <- c(as.raw(rep(0, 8 - col$length)),
      #                     raw)
      #                   col$length <- 8
      #                 }
      #                 data[[col$name]][row] <- readBin(raw, col$type,
      #                   1, col$length)
      #                 if (col$type == "character")
      #                   data[[col$name]][row] <- gsub("^ +| +$",
      #                     "", data[[col$name]][row])
      #               }
      #           }
      #           base <- base + row_length
      #      }
    }
    numbs=readBin_n(numraw,nums$length)		# number data is extracted from raw data
    numbs=matrix(numbs,ncol=nrow(nums),byrow=TRUE)				# aranged into columns
    chars=readChar(charaw,rep(char$length,row_count))			# character strings are extraccted from raw data
    chars=matrix(gsub("^ +| +$","", chars),ncol=nrow(char),byrow=TRUE)# aranged into columns

    if(ncol(chars)==0){ data=data.frame(numbs)
    }else if(ncol(numbs)==0) {data=data.frame(chars)
    }else data=cbind(data.frame(numbs),data.frame(chars))				# char cols and num cols are combined into a data.frame
    data=data[,order(c(which(col_attr$type=='numeric'),which(col_attr$type=='character')))] # columns are arranged into intended order
    names(data)=as.character(unlist(col_name))				# column names are added to data.frame

    if (nrow(data) != row_count)
      warning(paste("found", row, "records where", row_count,
                    "expected", BUGREPORT))

    attr(data, "column.info") <- col_info
    attr(data, "timestamp") <- timestamp
    attr(data, "SAS.release") <- SAS_release
    attr(data, "SAS.host") <- SAS_host
    attr(data, "OS.version") <- OS_version
    attr(data, "OS.maker") <- OS_maker
    attr(data, "OS.name") <- OS_name
    attr(data, "endianess") <- endianess
    attr(data, "winunix") <- winunix
    attr(data, "column.description") <- col_expl				# data.frame of column names and descriptions

    column_information=deparse(substitute(column_information))
    column_information=gsub('[\"\']','',column_information)
    if(column_information!='')
    {
      assign(column_information,col_expl,1)
      cat('>',column_information,'<- column descriptions\n')
    }else
    {
      cat("To access column descriptions use command:\n  >  attributes()$column.description\nwith object as argument\n")
    }
    return(data)
  }
# }



##############################################################
################## END OF read_sas7bdat function ##################
##############################################################
