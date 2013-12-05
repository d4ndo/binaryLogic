#' Binary prefix to Byte. (KiB,MiB,..)
#' 
#' @description Amount of Bytes needed to fit in n * KiB, MiB ..etc.
#' @details
#' KiB <- KibiByte
#' MiB <- MebiByte
#' GiB <- GibiByte
#' TiB <- TebiByte
#' PiB <- PebiByte
#' EiB <- ExiByte
#' ZiB <- ZebiByte
#' YiB <- YobiByte 
#' @usage binPrefix2Byte(n, prefix="KiB")
#' @param n vector of numeric values
#' @param prefix binary prefix * Byte. Expeting a »string«
#' @return The number of bytes fitting in n * binary prefix * Byte
#' @examples
#' binPrefix2Byte(c(0.5,1:10),"KiB")
#' dec2bin(binPrefix2Byte(1,"KiB")*Byte())
#' @export
binPrefix2Byte <- function(n, prefix="KiB") {
    if(missing(n)) stop("Missing n")
    stopifnot(all(is.numeric(n) | is.na(n)))
    stopifnot(all(n >= 0 | is.na(n)))
    ret <- NULL
    
    prefix <- switch(prefix,
                KiB = 2^10,
                MiB = 2^20,
                GiB = 2^30,
                TiB = 2^40,
                PiB = 2^50,
                EiB = 2^60,
                ZiB = 2^70,
                YiB = 2^80)
    
    ret = n * prefix
    if (any(is.null(ret))) stop("Unknown binary prefix")
    if (isTRUE(all.equal(ret, round(ret)))) {
        return(ret)
    } else {
        message("ceiling called: returns the smallest integer not less than the corresponding element of ret")
        return(ceiling(ret))
    }
}

#' Minimum number of »Byte« needed to hold n »Bit«
#' 
#' @description A simple helper function 
#' that returns the minimum number of Byte needed to hold the amount of n Bit.
#' @usage bytesNeeded(n)
#' @param n The number of Bit.
#' @return The number of minimum Byte needed to hold n Bit.
#' @export
bytesNeeded <- function(n) {
    if (n %% Byte() ==0)
    { 
        return (n %/% Byte())
    } else {
        return (n %/% Byte()+1)
    }
}

#' A simple helper function to return the size of one Byte
#' 
#' @description Used to increase readabilaty
#' @usage Byte()
#' @return The size of one Byte (8)
#' @export
Byte <- function() {
    return(8)
}
