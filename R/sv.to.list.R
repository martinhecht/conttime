## Changelog:
# MH 2024-05-27: set up

## Documentation
#' @title
#' @description
#' @param
#' @param matr.suffix, e.g. ".sv" or ".temp"
#' @param
#' @return

## Function definition
sv.to.list <- function( matrs, matr.suffix, env ) {
	sv.list <- list()
	for( matr in matrs ){
		if( all( !is.na( eval(parse(text=paste0(matr,matr.suffix)),envir=env) ) ) ){
			sv.list <- c( sv.list, eval(parse(text=paste0("list('",matr,"'=",matr,matr.suffix,")")),envir=env) )
		} else {
			mtr <- eval(parse(text=paste0(matr,matr.suffix)),envir=env)
			for (i in 1:dim(mtr)[1]) {
				for (j in 1:dim(mtr)[2]) {
					if( !is.na( mtr[i,j] ) ){
						# !NA means it's a free parameter for which a start value is needed, which is set via the label
						lab <- NULL

						# label from par env
						if( !is.null( eval(parse(text=paste0(matr,".par")),envir=env) ) ){
							lab <- eval(parse(text=paste0(matr,".par[",i,",",j,"]")),envir=env)
						}
						if( is.null( lab ) ) stop( paste0( "when attempting to set starting values, could not determine label for ",matr,"[",i,",",j,"]. Use par.env to specifiy labels for free parameters." ) )
					
						# add parameter to the start value list
						sv.list <- c( sv.list, eval(parse(text=paste0("list('",lab,"'=",matr,matr.suffix,"[",i,",",j,"])")),envir=env) )
					}
				}
			}
		}
	}
	return( sv.list )
}


### development
# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    # "Dropbox/139_conttime/conttime/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("sv.to.list.R") ]
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }

### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
