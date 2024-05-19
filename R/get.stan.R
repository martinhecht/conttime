## Changelog:
# MH 2024-04-29: set up

## Documentation
#' @title Get results
#' @description Get results from fitted Stan model
#' @param fit the results object from running the \code{stan()} function
#' @param stn the results object from running the \code{gen.stan()} function
#' @param true.env an environment containing the true values for the model parameters in the matrices A0, Achange, and Q0; used to calculate bias
#' @param transformed.parameters a logical value indicating whether the transformed parameters \code{c("lp__", "thetajp", "At", "Qt", "Ajp", "Qjp", "Astarjp", "Qstarjp", "Sigmawjp")} should be included in the returned results, or a character vector specifying which transformed parameters to include
#' @param sort.by.bias a logical value indicating whether to sort results by bias (if true values are provided in true.env)
#' @param verbose a logical value indicating whether to print detailed messages and progress updates during the execution of the function
#' @return A data frame with parameter estimates (and bias if true values are provided) is returned.


## Function definition
get.stan <- function( fit, stn, true.env=NULL, transformed.parameters=FALSE, sort.by.bias=TRUE, verbose=FALSE ){
	
	# require packages
	requireNamespace( "rstan" )
	
	## parameters to extract
	# model parameters
	pars.list <- stn$model.parameters
	
	# transformed parameters
	tf.pot <- c( "lp__", "thetajp", "At", "Qt", "Ajp", "Qjp", "Astarjp", "Qstarjp", "Sigmawjp" )
	if( is.character( transformed.parameters ) ){
		tf.list <- transformed.parameters[ transformed.parameters %in% tf.pot ]
		if( length( tf.list ) == 0 ) tf.list <- NULL
	} else if( is.logical( transformed.parameters ) ){
		if( transformed.parameters ) tf.list <- tf.pot else tf.list <- NULL
	} else {
		tf.list <- NULL
	}
	
	if( !is.null( tf.list ) ){
		# get all parameters
		all.par <- rownames( summary(fit)$summary )
		tf2.list <- do.call( "c", sapply( tf.list, function( x ) all.par[ grepl( x, all.par ) ], simplify=FALSE, USE.NAMES = FALSE ) )
		if( length( tf2.list ) > 0 ) pars.list <- c( pars.list, tf2.list )
	}

	# get estimates of model parameters
	# est <- as.data.frame( summary(fit, pars = stn$model.parameters)$summary )
	est <- as.data.frame( summary(fit, pars = pars.list)$summary )
	est$par <- rownames(est)
	rownames(est) <- seq( along=rownames( est ) )

	# free/fixed values
	par.list <- sapply( stn$model.parameters, function( par ) get( par, envir=stn$par.env, inherits=FALSE ), simplify=FALSE )
	par.df <- array.to.df( par.list, stn$model.parameters, value.name="par.label.or.fixed.value" )
	par.df$free.or.fixed <- suppressWarnings( as.numeric( par.df$par.label.or.fixed.value ) )
	par.df$free.or.fixed[ !is.na( par.df$free.or.fixed ) ]  <- "fixed"
	par.df$free.or.fixed[ is.na( par.df$free.or.fixed ) ]  <- "free"

	# merge estimates
	est$no <- seq( along=rownames( est ) )
	df <- merge( par.df, est, all.y=TRUE, by="par", sort=FALSE )
	df <- df[ order( df$no ), ]
	df$no <- NULL

	if( !is.null( true.env ) ){
		# true values
		true.list <- sapply( stn$model.parameters, function( par ) get( par, envir=true.env, inherits=FALSE ), simplify=FALSE )
		true.df <- array.to.df( true.list, stn$model.parameters, value.name="true.value" )
		# merge estimates
		df$no <- seq( along=rownames( df ) )
		df <- merge( df, true.df, all.x=TRUE, by="par", sort=FALSE )
		df <- df[ order( df$no ), ]
		df$no <- NULL		
		# bias
		# df$bias <- NA
		# df$bias[ df$free.or.fixed %in% "free" ] <- df$mean[ df$free.or.fixed %in% "free" ] - df$true.value[ df$free.or.fixed %in% "free" ]
		df$bias <- df$mean - df$true.value
		df$bias.percent <- NA
		if( any( logvec <- ( !is.na( df$true.value ) & df$true.value != 0 ) ) ) df$bias.percent[logvec] <- sign( df$bias[logvec] ) * ( abs( df$bias[logvec] ) / abs( df$true.value[logvec] ) ) * 100
	}
	
	# add first column with number
	df$no <- seq( along=rownames( df ) )
	vorn <- "no"
	df <- df[, c( vorn, colnames(df)[!colnames(df) %in% vorn] ) ]
	
	# sort by bias
	if( !is.null( df$bias ) & !is.null( df$bias.percent ) & sort.by.bias ){
		df <- df[ order( abs( df$bias.percent ), abs( df$bias ), decreasing=TRUE ), ]
	}
	
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

# require packages
# require( rstan )
# options( mc.cores = (parallel::detectCores()-1) )
# options( mc.cores = 1 ); rstan_options(auto_write = TRUE)
# require( "mvtnorm" )
# require( "expm" )

# design.env <- gen.design()
# data.env <- gen.data( design.env=design.env )
# stn <- gen.stan( data.env=data.env, syntax.dir="C:/users/martin/Desktop/temp" )

## fit
# fit <- stan( file=stn$syntax.path, model_name=stn$modle.name, data=stn$data, init=stn$init, chains=1, iter=10 )

# est <- get.stan( fit=fit, stn=stn, true.env=data.env )


### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
