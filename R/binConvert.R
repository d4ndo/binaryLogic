#' Converts a decimal or hex number to a binary digit.
#'
#' @description Converts a decimal or hex number to a binary number.
#' @details The binary number is represented by a logical vector.
#' The Bit order usually follows the same endianness as the byte order.
#' No floating-point support.
#' Little Endian    (LSB) ---> (MSB)
#' Big Endian       (MSB) <--- (LSB)
#' @usage dec2bin(num, littleEndian=FALSE, signed=FALSE, size=2)
#' @param num     integer (hex) or (decimal) number.
#' @param littleEndian if TRUE. Big Endian if FALSE.
#' @param signed  TRUE or FALSE. Unsigned by default. (two's complement)
#' @param size    Needed if signed is set. (by default 2 Byte)
#' @return The binary number. Returns a binary and logical vector.
#' @examples
#' dec2bin(0xfa)
#' dec2bin(3)
#' dec2bin(-1, signed=TRUE, size=1)
#' @seealso binaryLogic::as.binary, binaryLogic::is.binary, base::as.logical, base::is.logical, base::raw
#' @export
dec2bin <- function(num, littleEndian=FALSE, signed=FALSE, size=2) {
    if (missing(num)) stop("num is missing.")
    if (num < 0 & !signed) stop("Negative number is not possible with unsigned method.")
    if (signed & (((num > ((2^(size*Byte())/2)-1))) | (num < ((-1)*(2^(size*Byte())/2))))) {
        stop("Out of Range. Please increase the size[Byte]")
    }
  
    size <- size*Byte()
    neg = FALSE
    if(num < 0) neg = TRUE
    num = abs(num)
    rest <- num %% 2

    while((num <- num %/% 2) != 0)
    {
        rest <- c(num %% 2, rest)
    }
    if (signed)
    {
        a <- rep(TRUE, size)
        b <- rep(FALSE, size - length(rest))
        ret <- c(b, as.binary(rest))
        if (neg)
        {
            ret <- !ret
            ret <- binAdd(as.binary(ret, signed=TRUE), as.binary(TRUE))
        }
        if(littleEndian) ret <- rev(ret)
        return(as.binary(ret, signed=signed, littleEndian=littleEndian))
    }
    if(littleEndian) rest <- rest[length(rest):1]
 
    return(as.binary(rest, signed=signed, littleEndian=littleEndian))
}

#' Converts a binary number to a decimal or hex number.
#' 
#' @description Converts a binary number to a decimal or hex number.
#' @details No floating-point support.
#' Little Endian    (LSB) ---> (MSB)
#' Big Endian       (MSB) <--- (LSB)
#' @usage bin2dec(bin, hex=FALSE)
#' @param bin      binary number. Any logical or binary vector.
#' @param hex      TRUE or FALSE: if TRUE it returns an hex value. by default = FALSE
#' @return The decimal or hex number »integer«.
#' @examples
#' bin2dec(as.binary(c(1,0,1,1,1,0,1,1)))
#' bin2dec(as.binary(c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE)))
#' bin2dec(as.binary(c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE)), hex=TRUE)
#' @seealso binaryLogic::as.binary, binaryLogic::is.binary, base::as.logical, base::is.logical, base::raw
#' @export
bin2dec <- function(bin, hex=FALSE) {
    if (missing(bin)) stop("bin is missing.")
    stopifnot(is.binary(bin))
    signed <- attributes(bin)$signed
    littleEndian <- attributes(bin)$littleEndian
    
    if(!littleEndian) { bin <- rev(bin) }
    
    bin <- as.integer(bin)
    i = length(bin) - 1
    numeric = 0
    first <- TRUE

    if (signed)
    { 
        bin <- rev(bin) 
    }
    for(d in bin)
    {
        if ((signed) & (first)) {
            numeric <- (-1 * d * (2^i))
            i <- i - 1
        } else {
            numeric = (numeric + d * (2^i))
            i <- i - 1
        }
        first <- FALSE
    }
    if(hex) return(as.hexmode(numeric))
    else return(numeric)
}