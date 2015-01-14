
# workaround for broken list.dirs?? google pointed it out.

lsdir <- function(path, format = "basename", recursive = FALSE, all = FALSE) { 
  # list directories 
  # format is any part of "fullpath", "relative", or "basename" 

  # set a path if necessary 
  if (missing(path)) { 
    path <- "." 
  } 

  # recursion 
  if (recursive == FALSE) { 
    argRecursive <- "-maxdepth 1" 
  } else if (recursive) { 
    argRecursive <- "" 
  } 

  # piece together system command 
  execFind <- paste("find", path, argRecursive, 
    "-mindepth 1", "-type d", "-print", " | sort",  sep = " ") 

  # execute system command 
  tmp <- system(execFind, intern = TRUE) 

  # remove .hidden files if all == FALSE 
  if (all == FALSE) { 
    tmp <- tmp[grep("^\\..*", basename(tmp), invert = TRUE)] 
  } 

  # match format argument 
  format <- match.arg(tolower(format), c("fullpath", "relative", "basename")) 

  # format output based upon format argument 
  if (format == "basename") { 
    out <- basename(tmp) 
  } else if (format == "fullpath") { 
    out <- normalizePath(tmp) 
  } else { 
    out <- tmp 
  } 

  # clean up any duplicate "/" and return 
  return(gsub("/+", "/", out)) 
} 



