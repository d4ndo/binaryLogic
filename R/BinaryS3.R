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
#' \item »Little Endian« : MSB is at b[1] and LSB is at b[8].
#' \item »Big Endian« : LSB is at b[1] and MSB is at b[8].
#' }
#' Additional information: A vector in GNU R starts at 1 and not 0 like in C.
#' No floating-point support.
#' @usage binary(n, signed=FALSE, littleEndian=FALSE)
#' @param n length of vector. Number of bits
#' @param signed  TRUE or FALSE. Unsigned by default. (two's complement)
#' @param littleEndian if TRUE. Big Endian if FALSE.
#' @return a binary vector of length n. By default filled with zeros(0).
#' @examples
#' b <- binary(8)
#' @seealso \link{as.binary} and \link{is.binary}. To convert a binary to raw please use \link{as.raw} (pay attention to the endianness).
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
#' @seealso \link{is.binary} and \link{binary}
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
as.raw.binary <- function(x){
    l <- saveAttributes(x)
    x <- fillBits(x, size=bytesNeeded(length(x)))
    rawbyte <- binary(8)
    if (l$littleEndian) {
        rawbyte <- x[4:1]
        rawbyte <- c(rawbyte, x[8:5])
    } else {
        rawbyte <- x[1:8]
    }
    #in raw mode it does not matter, if signed or not.
    temp <- paste0("0x", bin2dec(as.binary(rawbyte, signed=FALSE, littleEndian=FALSE), hex=TRUE))
    if( bytesNeeded(length(x)) > 1) {
        for(i in 1:(bytesNeeded(length(x))-1))
        {
            if (l$littleEndian) {
                rawbyte <- x[((i+1)*Byte()-4):(i*Byte()+1)]
                rawbyte <- c(rawbyte, x[((i+1)*Byte()):((i+1)*Byte()-3)])
            } else {
                rawbyte <- x[(i*Byte()+1):((i+1)*Byte())]
            }
            temp <- c(temp, paste0("0x", 
                            bin2dec(as.binary(rawbyte, 
                                            signed=FALSE, 
                                            littleEndian=FALSE)
                                    ,hex=TRUE)))
        }
    }
    x <- temp
    NextMethod(.Generic)
}

######## BINARY OPERATOR ########
# Group Generic "Ops"

Ops.binary <- function(e1, e2)
{
    boolean <- switch(.Generic,  '+' =, '-' =, '*' =, '/' =, '^' =, '%%' =, '%/%' = TRUE, FALSE)
    if(boolean) {
        l1 <- saveAttributes(e1)
        ret <- NextMethod(.Generic)
        x <- loadAttributes(ret,l1)
        return(x)
    }
    boolean <- switch(.Generic,  '&' =, '|' = TRUE, FALSE)
    if(boolean) {
        l1 <- saveAttributes(e1)
        ret <- NextMethod(.Generic)
        x <- loadAttributes(ret, l1)
        return(x)
    }
    boolean <- switch(.Generic,  '!' = TRUE, FALSE)
    if(boolean) {
        l1 <- saveAttributes(e1)
        ret <- NextMethod(.Generic)
        x <- loadAttributes(ret,l1)
        return(x)
    }
}

#' @export
'+.binary' <- function(x,y) {
    ret <- binAdd(x, y)
    return(ret)
}

#' @export
'-.binary' <- function(x,y) {
    ret <- binAdd(x, negate(y))
    return(ret)
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
    return(!(x == y))
}

#' @export
'[.binary' <- function(x, i, j, drop=TRUE) {
    # this should not become a group generic. This function is an internal generic.
    l <- saveAttributes(x)
    
    ret <- NextMethod(.Generic, drop=drop)
    
    x <- loadAttributes(ret,l)
    return(x)
}

#' @export
rev.binary <- function(x) {
    # this should not become a group generic. This function is an internal generic.
    l <- saveAttributes(x)
    
    x <- x[length(x):1]
    #ret <- NextMethod(.Generic)

    x <- loadAttributes(x,l)
    return(x)
}

#Helper function
saveAttributes <- function(x) {
    l <- list(class="binary", signed=attr(x,"signed"), littleEndian=attr(x,"littleEndian"))
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