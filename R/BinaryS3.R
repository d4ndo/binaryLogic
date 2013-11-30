#' Binary Vector
#' 
#' @description Create objects of type "binary" vector.
#' @details No floating point supported.
#' @usage binary(n)
#' @param n length of vector.
#' @return a binary vector of length n
#' @seealso base::as.logical , base::is.logical, base::as.integer base::raw
#' @export
binary <- function(n) {
    x <- logical(length)
    class(x) <- c("binary", class(x))
    x
}

#' as Binary Vector
#' 
#' @description convert object to "binary".
#' @usage as.binary(x)
#' @param x object to convert.
#' @return a binary vector.
#' @seealso base::as.logical , base::is.logical, base::as.integer base::raw
#' @export
as.binary <- function(x){
    if(!inherits(x, "binary")) class(x) <- c("binary", class(x))
    x
}

#' is Binary Vector
#' 
#' @description test for object "binary".
#' @usage is.binary(x)
#' @param x object to test.
#' @return TRUE or FALSE.
#' @seealso base::as.logical , base::is.logical, base::as.integer base::raw
#' @export
is.binary <- function(x) {
    inherits(x, "binary")
}

print.binary <- function(x,...) {
    x <- ifelse(x, as.integer(1), as.integer(0))
    cat(x,"\n")
    #NextMethod(x,...)
    #cat("PRI")
}

#'+.bin' <- function(x,y) {
#    print("TEST")    
#}