## Changelog:
# MH 2024-04-29: set up

## Documentation
#' @title
#' @description
#' @param
#' @param
#' @param
#' @return


## Function definition
get.stan <- function( fit, stn, true.env=NULL, verbose=FALSE ){
	
	# require packages
	requireNamespace( "rstan" )
	
	# get estimates of model parameters
	est <- as.data.frame( summary(fit, pars = stn$model.parameters)$summary )
	est$par <- rownames(est)
	rownames(est) <- seq( along=rownames( est ) )
	
	# free/fixed values
	par.list <- sapply( stn$model.parameters, function( par ) get( par, envir=stn$par.env, inherits=FALSE ), simplify=FALSE )
	par.df <- array.to.df( par.list, stn$model.parameters, value.name="par.label.or.fixed.value" )
	par.df$free.or.fixed <- suppressWarnings( as.numeric( par.df$par.label.or.fixed.value ) )
	par.df$free.or.fixed[ !is.na( par.df$free.or.fixed ) ]  <- "fixed"
	par.df$free.or.fixed[ is.na( par.df$free.or.fixed ) ]  <- "free"
	
	# merge estimates
	df <- merge( par.df, est, by="par", sort=FALSE )

	if( !is.null( true.env ) ){
		# true values
		true.list <- sapply( stn$model.parameters, function( par ) get( par, envir=true.env, inherits=FALSE ), simplify=FALSE )
		true.df <- array.to.df( true.list, stn$model.parameters, value.name="true.value" )
		# merge estimates
		df <- merge( df, true.df, by="par", sort=FALSE )
		# bias
		# df$bias <- NA
		# df$bias[ df$free.or.fixed %in% "free" ] <- df$mean[ df$free.or.fixed %in% "free" ] - df$true.value[ df$free.or.fixed %in% "free" ]
		df$bias <- df$mean - df$true.value
	}
	
	# add first column with number
	df$no <- seq( along=rownames( df ) )
	vorn <- "no"
	df <- df[, c( vorn, colnames(df)[!colnames(df) %in% vorn] ) ]
	
	return( df )
}


### development
# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    # "Dropbox/139_conttime/conttime/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("get.stan.R") ]
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }


### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
