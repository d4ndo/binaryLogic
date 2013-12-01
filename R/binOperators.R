#' Binary Addition (+)
#' 
#' @description Adds two binary numbers. (x + y)
#' @details Little-Endian is not supported at the moment. No floating point supported.
#' @usage binAdd(x, y, signed=FALSE, size=0)
#' @param x summand 1 (binary or logical vector)
#' @param y summand 2 (binary or logical vector)
#' @param signed TRUE or FALSE. Returns a signed number. see example
#' @param size in Byte. Is needed if signed is set. 0 = auto
#' @return The sum of x and y. Returns a binary/logical vector.
#' @examples
#' binAdd(as.logical(c(0,1)),as.logical(c(1,0)))
#' @seealso base::as.logical , base::is.logical, base::as.integer base::raw
#' @export
binAdd <- function(x, y, signed=FALSE, size=0) {
    if (length(x) >= length(y))
    {
        MAX <- length(x)
        a <- rep(FALSE,length(x)-length(y))
        y <- c(a,y)
    } else {
        MAX <- length(y)
        a <- rep(FALSE,length(y)-length(x))
        x <- c(a,x)
    }

    ret = logical(MAX)
    temp = logical(MAX+1)
    ret[MAX] <- xor(x[MAX],y[MAX])
    if(isTRUE(x[MAX]) & isTRUE(y[MAX])) temp[MAX+1] = TRUE

    if(MAX > 2)
    {
        for(i in (MAX-1):1)
        {
            ret[i] <- xor(x[i],y[i])
            ret[i] <- xor(ret[i],temp[i+2])
            if((isTRUE(x[i]) & isTRUE(y[i])) | 
                (isTRUE(x[i]) & isTRUE(temp[i+2])) | 
                (isTRUE(y[i]) & isTRUE(temp[i+2]))) temp[i+1] = TRUE
        }
    }
    if(temp[2] & !signed) ret <- c(T,ret)
    return(as.binary(ret))
}

#' Binary Negation (!)
#' 
#' @description Negates the binary number x. Negation x -> -x or -x -> x
#' @details No floating point supported.
#' @usage negate(x)
#' @param x The number to be negated. A binary or logical vector is expected.
#' @return The negated number of x. Returns a binary/logical vector with signed=TRUE
#' @examples
#' negate(as.binary(c(1,0,1)))
#' bin2dec(negate(dec2bin(-5, signed=TRUE)))
#' @seealso base::as.logical , base::is.logical, base::as.integer base::raw
#' @export
negate <- function(x) {
    if (missing(x)) stop("x is missing.")
    stopifnot(is.logical(x) | is.binary(x))
    if(length(x)%%Byte() != 0) {
        MAX <- (trunc((length(x)/Byte())) +1) * Byte()
        a <- rep(FALSE, MAX - length(x))
        x <- c(a,x)
    }
    x <- !x
    return(binAdd(x,TRUE))
}


#' Binary Left Shift (<<)
#' 
#' @description Logical left shift x << n
#' @usage shiftLeft(x, n)
#' @param x The binary number to shift. (binary or logical vector).
#' @param n The number of bits to shift.
#' @return Pushes 0's(FALSE) to the vector from right(LSB) to left(MSB).
#' Everything on right(MSB) side drops out. Returns a binary/logical vector
#' @examples
#' x <- as.binary(c(1,0,0,1,1,1,0,1)); x
#' shiftLeft(x,1)
#' shiftLeft(x,2)
#' @seealso base::as.logical , base::is.logical, base::as.integer base::raw
#' @export
shiftLeft <- function(x, n) {
    if (missing(x)) stop("x is missing.")
    stopifnot(is.logical(x) | is.binary(x))
    stopifnot(n > 0)
    if(n > length(x)) stop("n is larger than length of x")
    delta <- length(x) - n
    for(i in 1:n)
    {
        for(i in 1:length(x)){
        x[i] <- x[i+1]
        }
    }
    x[(delta+1):length(x)] <- FALSE
    return(as.binary(x))
}

#' Binary Right Shift (>>)
#' 
#' @description Logical right shift 1 >> n
#' @usage shiftRight(x, n)
#' @param x The binary number to shift. (binary or logical vector).
#' @param n The number of bits to shift.
#' @return Pushes 0's(FALSE) to the vector from left(MSB) to right(LSB).
#' Everything on right(LSB) side drops out. Returns a binary/logical vector
#' @examples
#' x <- as.binary(c(1,0,0,1,1,1,0,1)); x
#' shiftRight(x,1)
#' shiftRight(x,2)
#' @seealso base::as.logical , base::is.logical, base::as.integer base::raw
#' @export
shiftRight <- function(x, n) {
    if (missing(x)) stop("x is missing.")
    stopifnot(is.logical(x) | is.binary(x))
    stopifnot(n > 0)
    if(n > length(x)) stop("n is larger than length of x")
    delta <- length(x) - n
    x <- x[length(x):1]
    for(i in 1:n)
    {
        for(i in 1:length(x)){
        x[i] <- x[i+1]
        }
    }
    x[(delta+1):length(x)] <- FALSE
    x <- x[length(x):1]
    return(as.binary(x))
}

#' Rotate no carry ()
#' 
#' @description A circular shift
#' @usage rotate(x, n)
#' @param x The binary number to rotate. (binary or logical vector).
#' @param n The number of bits to rotate.
#' @return rotates the vector from left to right. 
#' The value from MSB is used to fill up the vector at LSB. Returns a binary/logical vector.
#' @examples
#' x <- as.binary(c(1,0,0,1,1,1,0,1)); x
#' rotate(x,1)
#' rotate(x,2)
#' @seealso base::as.logical , base::is.logical, base::as.integer base::raw
#' @export
rotate <- function(x, n) {
    if (missing(x)) stop("x is missing.")
    stopifnot(is.logical(x) | is.binary(x))
    stopifnot(n > 0)
    if(n > length(x)) stop("n is larger than length of x")
    delta <- length(x) - n

    tmp <- x[1:n]
    for(i in 1:n)
    {
        for(i in 1:length(x)){
        x[i] <- x[i+1]
        }
    }
    x[(delta+1):length(x)] <- tmp[1:n]
    return(as.binary(x))
}

#' Fill Bits (000..)
#' 
#' @description Fills the number with Bits to the size in Byte.
#' @details No floating point supported.
#' @usage fillBits(x, value=FALSE, littleEndian=FALSE, size=0)
#' @param x The binary number to fill with bits. (binary or logical vector).
#' @param value Fill with FALSE or fill with TRUE.
#' @param littleEndian if TRUE. Big Endian if FALSE.
#' @param size in Byte. 0 = auto (smallest possible byte).
#' @return binary number. A binary / logical vector with the desired size.
#' @examples
#' fillBits(as.binary(c(0,0)), value=TRUE)
#' fillBits(as.logical(c(1,1)), size=2)
#' fillBits(as.binary(c(TRUE,FALSE,TRUE)), littleEndian=TRUE, value=FALSE, size=2)
#' @seealso base::as.logical , base::is.logical, base::as.integer base::raw
#' @export
fillBits <- function(x, value=FALSE, littleEndian=FALSE, size=0) {
    if(missing(x)) stop("x is missing")
    stopifnot(is.logical(x) | is.binary(x))
    if(size == 0 & length(x)%%Byte() == 0) return(x)
    if(size > 0 & length(x) >= size*Byte()) return(x)

    if(size == 0) {
        append <- logical(((trunc((length(x)/Byte())) +1) * Byte()) - length(x))
    } else {
        append <- logical(size*Byte()-length(x))
    }
    append[1:length(append)] <- value

    if (littleEndian) {
        x <- c(x,append)
    } else {
        x <- c(append,x)
    }
    return(as.binary(x))
}