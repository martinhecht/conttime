## Changelog:
# MH 0.0.1 2024-06-20: set up

## Documentation
#' @title
#' @description
#' @param
#' @param
#' @param
#' @return

## Function definition
is_positive_semi_definite <- function( m, tol=1e-10 ){

	eigenvalues <- eigen(m)$values
	ipsd <- all(eigenvalues >= -tol)
	
	return( ipsd )
}


### development

# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    # "Dropbox/96_mutualism/mutualism/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("is_positive_semi_definite.R") ]
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }

### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
