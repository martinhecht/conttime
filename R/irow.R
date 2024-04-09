## Changelog:
# MH 0.0.2 2024-04-05, added  "|| (is.array(rowM) && length( dim(rowM) > 1 ) ==1)"
# MH 0.0.1 2022-05-31

## Documentation
#' @title
#' @description the irow operator puts the elements from the column vector back into a matrix
#' @param
#' @param
#' @param
#' @return

## Function definition
irow <- function( rowM ){
    if( ! ( !is.null(rowM) && ( is.matrix(rowM) || (is.array(rowM) && length( which( dim(rowM) > 1 ) ) == 1) ) && dim(rowM)[1]>0 && dim(rowM)[2]==1 ) ){
        M <- NULL
    } else {
        F <- sqrt( length( rowM ) )
        M <- matrix( NA, nrow=F, ncol=F )
        for ( r in (1:F) ) {
            for ( c in (1:F) ) {
                M[r,c] <- rowM[ c+F*(r-1) ]
            }
        }
    }
    return( M )
}

### development

# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    # "Dropbox/96_mutualism/mutualism/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("irow.R") ]
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }

### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
