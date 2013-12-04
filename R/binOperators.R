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
#' @seealso base::as.logical , base::is.logical, base::raw
#' @export
negate <- function(x) {
    if (missing(x)) stop("x is missing.")
    stopifnot(is.binary(x))
    signed <- attributes(x)$signed
    if (!signed) warning("Trying to negate an unsigned digit. treated as signed value. Returns a signed value")
    littleEndian <- attributes(x)$littleEndian

    if(littleEndian) x <- rev(x)
    
    if (length(x)%%Byte() != 0) {
        MAX <- (trunc((length(x)/Byte())) +1) * Byte()
        a <- rep(FALSE, MAX - length(x))
        as.binary(a, littleEndian=littleEndian)
        # 'c.binary'(x,y) needs to be implemented
        x <- as.binary(c(a,x))
    }
    x <- !x
    x <- binAdd(x,as.binary(TRUE))

    if(littleEndian) x <- rev(x)
    attr(x,"signed") <- TRUE
    attr(x,"littleEndian") <- littleEndian
    class(x) <- c("binary", "logical")
    return(x)
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
    if (n > length(x)) stop("n is larger than length of x")
    delta <- length(x) - n
    for(i in 1:n)
    {
        for(i in 1:length(x)){
        x[i] <- x[i+1]
        }
    }
    x[(delta+1):length(x)] <- FALSE
    return(x)
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
#' @seealso binaryLogic::as.binary, binaryLogic::is.binary
#' @export
shiftRight <- function(x, n) {
    if (missing(x)) stop("x is missing.")
    stopifnot(is.logical(x) | is.binary(x))
    stopifnot(n > 0)
    if (n > length(x)) stop("n is larger than length of x")
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
    return(x)
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
    if (n > length(x)) stop("n is larger than length of x")
    delta <- length(x) - n

    tmp <- x[1:n]
    for(i in 1:n)
    {
        for(i in 1:length(x)){
        x[i] <- x[i+1]
        }
    }
    x[(delta+1):length(x)] <- tmp[1:n]
    return(x)
}

#' Fill Bits (000..)
#' 
#' @description Fills the number with Bits to the size in Byte.
#' @details No floating point supported.
#' @usage fillBits(x, value=FALSE, size=0)
#' @param x The binary number to fill with bits. (Any binary vector).
#' @param value Fill with FALSE or fill with TRUE.
#' @param size in Byte. 0 = auto (smallest possible byte).
#' @return binary number. A binary vector with the desired size.
#' @examples
#' fillBits(as.binary(c(1,1)), size=2)
#' fillBits(as.binary(c(1,0,1), littleEndian=TRUE), value=FALSE, size=2)
#' @seealso binaryLogic::as.binary, binaryLogic::is.binary
#' @export
fillBits <- function(x, value=FALSE, size=0) {
    if (missing(x)) stop("x is missing")
    stopifnot(is.binary(x))
    if (size == 0 & length(x)%%Byte() == 0) return(x)
    if (size > 0 & length(x) >= size*Byte()) return(x)

    if (size == 0) {
        append <- binary(((trunc((length(x)/Byte())) +1) * Byte()) - length(x))
    } else {
        append <- binary(size*Byte()-length(x))
    }
    append[1:length(append)] <- value

    if (attributes(x)$littleEndian) {
        x <- c(x,append)
    } else {
        x <- c(append,x)
    }
    return(x)
}

#' Switch Endianess.
#' 
#' @description switch little-endian to big-endian and vice versa.
#' @usage switchEndianess(x)
#' @param x binary number. Any binary vector. switchEndianess(y)
#' @return switch little-endian to big-endian and vice versa.
#' @examples
#' x <- as.binary(c(1,1,0,0), littleEndian=TRUE); print(x); attributes(x);
#' y <- switchEndianess(x); print(y); attributes(y);
#' @seealso binaryLogic::as.binary, binaryLogic::is.binary
#' @export
switchEndianess <- function(x) {
    if (missing(x)) stop("x is missing")
    stopifnot(is.binary(x))
    
    if (attributes(x)$littleEndian == FALSE) {
        attr(x, "littleEndian") <- TRUE
    } else {
        attr(x, "littleEndian") <- FALSE
    }
    
    return(rev(x))
}