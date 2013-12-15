#' Binary Vector
#' 
#' @description Create objects of type "binary" vector.
#' @details The binary number is represented by a logical vector.
#' The Bit order usually follows the same endianess as the byte order.
#' How to read:
#' \itemize{
#' \item Little Endian    (LSB) ---> (MSB)
#' \item Big Endian       (MSB) <--- (LSB)
#' }
#' The »Big Endian« endianess stores its MSB at the 
#' lowest adress and the »Little Endian« endianess stores its MSB at the highest adress.
#' 
#' e.g. b <-binary(8).
#' \itemize{
#' \item »Little Endian« : MSB at b[1] and LSB at b[8].
#' \item »Big Endian« : LSB at b[1] and MSB at b[8].
#' }
#' Performance: This class is just not that great at heavy number crunching, 
#' but it brings some benefits. Especially if you like to work using vectors in R.
#' It is no problem to switch from logical to binary and vice versa.
#' No floating-point support.
#' @usage binary(n, signed=FALSE, littleEndian=FALSE)
#' @param n length of vector. Number of bits
#' @param signed  TRUE or FALSE. Unsigned by default. (two's complement)
#' @param littleEndian if TRUE. Big Endian if FALSE.
#' @return a binary vector of length n. By default filled with zeros(0).
#' @examples
#' b <- binary(8)
#' summary(b)
#' @seealso \link{as.binary} and \link{is.binary}. To convert a binary to raw please use \link{as.raw} (pay attention to the endianness).
#' @export
binary <- function(n, signed=FALSE, littleEndian=FALSE) {
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
#' @seealso \link{is.binary} and \link{binary}
#' @export
as.binary <- function(x, signed=FALSE, littleEndian=FALSE) {
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
#' @seealso \link{as.binary} and \link{binary}
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
summary.binary <- function(object, ...) {
    #I'm not sure if this is the way to do it. just printing a dataframe.
    l <- saveAttributes(object)

    tableHead <- character(3)
    signedness <- character(1)
    endianess <- character(1)
    size <- length(object)
    neg <- logical(1)
    
    tableHead <- c("Signedness", "Endianess", "value<0", "Size[Bit]", "Base10")
    if (l$signed) {
        signedness <- "signed"
        if (l$littleEndian) {
            if(object[length(object)]) neg <- TRUE else neg <- FALSE
        } else {
            if(object[1]) neg <- TRUE else neg <- FALSE
        }
    } else {
        signedness <- "unsigned"
        neg <- FALSE
    }
    if (l$littleEndian) { endianess <- "Little-Endian" } else { endianess <- "Big-Endian" }
    df <- data.frame(signedness, endianess, neg, size, bin2dec(object))
    colnames(df) <- tableHead
    print(df)
}

#' @export
as.raw.binary <- function(x) {
    l <- saveAttributes(x)
    x <- fillBits(x, size=bytesNeeded(length(x)))
    xx <- logical(0)

    if (l$littleEndian) {
        x <- as.logical(x)
        dim(x) <- c(4, (2 * bytesNeeded(length(x))))
        for(i in seq(1, (2*bytesNeeded(length(x)) - 1), by = 2)) xx <- c(xx, c(x[,i+1], x[,i]))
        x <- xx
    } else {
        x <- rev(x)
    }
    
    x <- packBits(x)
    if(!l$littleEndian) x <- rev(x)
    NextMethod(.Generic)
} #rseq = function(x,to=1) NROW(x):to

#' @export
as.integer.binary <- function(x, ...) {
    #test for .Machine$integer.max
    x <- bin2dec(x)
    NextMethod(.Generic, ...)
}

######## BINARY OPERATOR ########
# Group Generic "Ops"

Ops.binary <- function(e1, e2) {
    boolean <- switch(.Generic,  '+' =, '-' =, '*' =, '/' =, '^' =, '%%' =, '%/%' = TRUE, FALSE)
    if (boolean) {
        l1 <- saveAttributes(e1)
        ret <- NextMethod(.Generic)
        x <- loadAttributes(ret,l1)
        return(x)
    }
    boolean <- switch(.Generic,  '&' =, '|' = TRUE, FALSE)
    if (boolean) {
        l1 <- saveAttributes(e1)
        ret <- NextMethod(.Generic)
        x <- loadAttributes(ret, l1)
        return(x)
    }
    boolean <- switch(.Generic,  '!' = TRUE, FALSE)
    if (boolean) {
        l1 <- saveAttributes(e1)
        ret <- NextMethod(.Generic)
        x <- loadAttributes(ret,l1)
        return(x)
    }
}

#' @export
'+.binary' <- function(x,y) {
    binAdd(x, y)
}

#' @export
'-.binary' <- function(x,y) {
    binAdd(x, negate(y))
}

#' @export
'==.binary' <- function(x,y) {
    # attributes are saved @ group generic Ops.  
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
    # attributes are saved @ group generic Ops.
    !(x == y)
}

#' @export
'[.binary' <- function(x, i, j, drop=TRUE) {
    # this should not become a group generic. This function is an internal generic.
    l <- saveAttributes(x)
    
    ret <- NextMethod(.Generic, drop=drop)
    
    x <- loadAttributes(ret,l)
    return(x)
}

#c.binary <- function(...) {
#    l <- saveAttributes(...)
#    ret <- NextMethod(.Generic)
#    x <- loadAttributes(ret,l)
#    return(x)
#}

#' @export
rev.binary <- function(x) {
    # this should not become a group generic. This function is an internal generic.
    l <- saveAttributes(x)
    #x = x[rseq(x)] rseq = function(x,to=1) NROW(x):to
    x <- x[length(x):1]
    #ret <- NextMethod(.Generic)

    x <- loadAttributes(x,l)
    return(x)
}

#Helper function
saveAttributes <- function(x) {
    if(is.binary(x)) l <- list(class=c("binary","logical"),
                                     signed=attr(x,"signed"),
                                     littleEndian=attr(x,"littleEndian"))
    else l <- list(class=class(x))
    return(l)
}

#Helper function
loadAttributes <- function(x,l) {
    class(x) <- l$class
    attr(x, "signed") <- l$signed
    attr(x, "littleEndian") <- l$littleEndian
    return(x)
}

#'%<<%.binary' <- function(x,y) {
#    print("TEST")
#    return(1)
#}