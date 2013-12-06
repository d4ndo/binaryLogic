#' Binary Vector
#' 
#' @description Create objects of type "binary" vector.
#' @details The binary number is represented by a logical vector.
#' The Bit order usually follows the same endianness as the byte order.
#' No floating-point support.
#' Little Endian    (LSB) ---> (MSB)
#' Big Endian       (MSB) <--- (LSB)
#' @usage binary(n, signed=FALSE, littleEndian=FALSE)
#' @param n length of vector. Number of bits
#' @param signed  TRUE or FALSE. Unsigned by default. (two's complement)
#' @param littleEndian if TRUE. Big Endian if FALSE.
#' @return a binary vector of length n
#' @seealso base::as.logical , base::is.logical, base::raw
#' @export
binary <- function(n, signed=FALSE, littleEndian=FALSE) {
    if(missing(n)) stop("n is missing.")
    x <- logical(n)
    class(x) <- c("binary", "logical")
    attr(x, "signed") <- signed
    attr(x, "littleEndian") <- littleEndian
    if (signed) { x <- fillBits(x) }
    return(x)
}

#' as Binary Vector
#' 
#' @description convert object to "binary".
#' @usage as.binary(x, signed=FALSE, littleEndian=FALSE)
#' @param x object to convert.
#' @param signed  TRUE or FALSE. Unsigned by default. (two's complement) 
#' @param littleEndian if TRUE. Big Endian if FALSE.
#' @return a binary vector.
#' @seealso base::as.logical , base::is.logical, base::as.integer base::raw
#' @export
as.binary <- function(x, signed=FALSE, littleEndian=FALSE){
    if (missing(x)) stop("x is missing")
    if (!inherits(x, "binary")) {
        x <- as.logical(x)
        class(x) <- c("binary", "logical")
        attr(x, "signed") <- signed
        attr(x, "littleEndian") <- littleEndian
        if (signed) x <- fillBits(x)
    } else { 
        if (signed) {
            attr(x, "signed") <- TRUE
            x <- fillBits(x)
        } else {
            attr(x, "signed") <- FALSE
        }
        if (littleEndian) {
            if (!attributes(x)$littleEndian)
            {
                switchEndianess(x)
            }
        } else {
            if (attributes(x)$littleEndian)
            {       
                switchEndianess(x)
            }
        }
    }
    return(x)
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
    return(inherits(x, "binary"))
}

#' @export
print.binary <- function(x,...) {
    x <- ifelse(x, as.integer(1), as.integer(0))
    attributes(x) <- NULL
    print.default(x,...)
}

#' @export
'==.binary' <- function(x,y) {
    if (attributes(x)$littleEndian) x <- switchEndianess(x)
    if (attributes(y)$littleEndian) y <- switchEndianess(y)
  
    if (length(x) > length(y)) {
        delta <- bytesNeeded(length(x))
        if (attributes(y)$signed) {
            y <- fillBits(y, value=TRUE, size=delta)
        } else {
            y <- fillBits(y, value=FALSE, size=delta)
        }
    } else if (length(x) < length(y)) {
        delta <- bytesNeeded(length(y))
        if (attributes(x)$signed) {
            x <- fillBits(x, value=TRUE, size=delta)
        } else {
            x <- fillBits(x, value=FALSE, size=delta)
        }
    }
    return(all(!xor(x,y)))
}

#' @export
'!=.binary' <- function(x,y) {
    return(!(x==y))
}

#' @export
'!.binary' <- function(x) {
    signed <- attributes(x)$signed
    littleEndian <- attributes(x)$littleEndian
    
    ret <- NextMethod(.Generic)
    
    attr(ret, "signed") <- signed
    attr(ret, "littleEndian") <- littleEndian
    class(ret) <- c("binary", "logical")
    return(ret)
}

#' @export
'[.binary' <- function(x, i, j, drop=TRUE) {
    signed <- attributes(x)$signed
    littleEndian <- attributes(x)$littleEndian

    ret <- NextMethod(.Generic, drop=drop)

    attr(ret, "signed") <- signed
    attr(ret, "littleEndian") <- littleEndian
    class(ret) <- c("binary", "logical")
    return(ret)
}

#'%<<%.binary' <- function(x,y) {
#    print("TEST")
#    return(1)
#}