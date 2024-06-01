## Changelog:
# MH 2024-05-19: set up

## Documentation
#' @title Set starting values
#' @description Set starting values for estimation with Stan
#' @param F number of processes (variables), must be >= 1
#' @param chains number of chains for estimation with Stan
#' @param start.values.env an environment containing start values for the elements of the matrices A0, Achange, and Q0; if \code{NULL}, default start values are used
#' @param jitter a logical value indicating whether start values should be jittered to provide different start values for each chain
#' @param jitter.env an environment containing information on how to jitter the elements of the matrices A0, Achange, and Q0; if \code{NULL}, default jitter routines are used
#' @param seed a number used as the seed for \code{set.seed(seed)} or the value "random" for the random generation of a seed
#' @param par.env an environment containing declarations of model parameters in the matrices A0, Achange, and Q0. This is used to determine and align whether a parameter is freely estimated (requiring a start value) or fixed (no start value needed). This is optional, as Stan will ignore start values for fixed parameters.
#' @param return.init.only a logical value that determines whether only the \code{init} object for function \code{stan} should be returned, or if additional information (such as the seed and starting values in the form of a data frame) should also be included
#' @param verbose a logical value indicating whether to print detailed messages and progress updates during the execution of the function
#' @return Either the \code{init} object for the \code{stan} function when \code{return.init.only=TRUE}, or a list containing \code{init}, \code{seed.jitter.sv}, and \code{sv} when \code{return.init.only=FALSE}.

## Function definition
start.values <- function( F=2, chains=1, start.values.env=NULL, jitter=TRUE, jitter.env=NULL, seed="random", par.env=NULL, return.init.only=TRUE, verbose=TRUE ){

	# require packages
	# requireNamespace( "matrixcalc" )

	## start values for parameters of mixed structures
	# https://discourse.mc-stan.org/t/how-to-define-initial-values-in-stan-in-r/16855
	# starting.values <- rep(NA,length(parameters.of.mixed.structures))
	# names( starting.values ) <- parameters.of.mixed.structures
	# starting.values[grepl("^lambda.*$", names( starting.values ) )] <- 1
	# starting.values[grepl("^sigmaeps[[:digit:]]+.*$", names( starting.values ) )] <- diag( keep.fixed$Sigmaeps )
	# starting.values[grepl("^sigmaepsA[[:digit:]]+.*$", names( starting.values ) )] <- diag( keep.fixed$SigmaepsA )
	# starting.values[grepl("^sigmaepsQ[[:digit:]]+.*$", names( starting.values ) )] <- diag( keep.fixed$SigmaepsQ )
	# MH 0.0.26 2024-05-04 variance of Q-covariance errors now 0,  something wrong with length of diag( keep.fixed$SigmaepsQ ), therefore set to 0
	# starting.values[grepl("^sigmaepsQ[[:digit:]]+.*$", names( starting.values ) )] <- 0.0025
	# starting.values[grepl("^sigmaepsmu[[:digit:]]+.*$", names( starting.values ) )] <- diag( keep.fixed$Sigmaepsmu )
	
	# if( length( parameters.of.mixed.structures ) > 0 ){		
		# eval(parse(text=paste0( "init_fun <- function(...) c( list(  ",paste( paste0( "'", parameters.of.mixed.structures, "'=",starting.values[parameters.of.mixed.structures],"" ), collapse=", " ),"  ) )" ))) # , 'Q0'=keep.fixed$Q0, 'Qchange'=keep.fixed$Qchange
		# eval(parse(text=paste0( "init_fun <- function(...) c( list(  ",paste( paste0( "'", parameters.of.mixed.structures, "'=1" ), collapse=", " ),"  ) )" )))
		# eval(parse(text=paste0( "init_fun <- function(...) c( list(  ",paste( paste0( "'", parameters.of.mixed.structures, "'=1" ), collapse=", " ),"  ), s.val )" )))
	# } else {
		# init_fun <- "random"
	# }

	# MH 0.0.30 2024-05-07, no epsAt
	# epsAt <- get('epsAt', envir=data.env, inherits=FALSE)
	# epsQt <- get('epsQt', envir=data.env, inherits=FALSE)
	# epsAt[] <- 0
	# epsQt[] <- 0
	# Q0 <- keep.fixed$Q0
	# Q0[] <- 0
	# diag( Q0 ) <- 1
	# MH 0.0.27 2024-05-04 Q0 is Q (not time-varying)
	# Qchange <- keep.fixed$Qchange
	# Qchange[] <- 0
	# diag( Qchange ) <- 10^-6
	# A0 <- keep.fixed$A0
	# A0[] <- 0
	# diag( A0 ) <- -0.75
	# Achange <- keep.fixed$Achange
	# Achange[] <- 0
	# if( between.mu ) {
		# Sigmamu <- keep.fixed$Sigmamu
		# Sigmamu[] <- 0
		# diag( Sigmamu ) <- 1
	# }
	# sigmaepsA.sv <- paste( paste0( "'", parameters.of.mixed.structures[grepl("^sigmaepsA.*$",parameters.of.mixed.structures)], "'=",starting.values[parameters.of.mixed.structures[grepl("^sigmaepsA.*$",parameters.of.mixed.structures)]],"" ), collapse=", " )
	# sigmaepsQ.sv <- paste( paste0( "'", parameters.of.mixed.structures[grepl("^sigmaepsQ.*$",parameters.of.mixed.structures)], "'=",starting.values[parameters.of.mixed.structures[grepl("^sigmaepsQ.*$",parameters.of.mixed.structures)]],"" ), collapse=", " )
	# sigmaepsmu.sv <- paste( paste0( "'", parameters.of.mixed.structures[grepl("^sigmaepsmu.*$",parameters.of.mixed.structures)], "'=",starting.values[parameters.of.mixed.structures[grepl("^sigmaepsmu.*$",parameters.of.mixed.structures)]],"" ), collapse=", " )
	# sigmaeps.sv <- paste( paste0( "'", parameters.of.mixed.structures[grepl("^sigmaeps[[:digit:]]+.*$",parameters.of.mixed.structures)], "'=",starting.values[parameters.of.mixed.structures[grepl("^sigmaeps[[:digit:]]+.*$",parameters.of.mixed.structures)]],"" ), collapse=", " )
	# lambda.sv <- paste( paste0( "'", parameters.of.mixed.structures[grepl("^lambda[[:digit:]]+.*$",parameters.of.mixed.structures)], "'=",starting.values[parameters.of.mixed.structures[grepl("^lambda[[:digit:]]+.*$",parameters.of.mixed.structures)]],"" ), collapse=", " )

	### start values ###
	
	# defaults
	# empty.str.env <- gen.empty.structures(env=data.env)
	# A0.sv <- get( "A0", envir=empty.str.env, inherits=FALSE )
	# Achange.sv <- get( "Achange", envir=empty.str.env, inherits=FALSE )
	# Q0.sv <- get( "Q0", envir=empty.str.env, inherits=FALSE )
	A0.sv <- Achange.sv <- Q0.sv <- matrix( as.numeric(NA), F, F )
	
	A0.sv[] <- 0
	diag( A0.sv ) <- -0.69
	# Achange.sv[] <- 0
	# MH 0.0.43
	Achange.sv[] <- 0
	diag(Achange.sv) <- 0
	Q0.sv[] <- 0.001
	diag( Q0.sv ) <- 0.5		
	
	# get elements of start.values.env and overwrite defaults
	if( !is.null( start.values.env ) ){
		if( "A0"      %in% ls(envir=start.values.env) )      A0.sv      <- get('A0'         , envir=start.values.env, inherits=FALSE)
		if( "Achange" %in% ls(envir=start.values.env) )      Achange.sv <- get('Achange'    , envir=start.values.env, inherits=FALSE)
		if( "Q0"      %in% ls(envir=start.values.env) )      Q0.sv      <- get('Q0'         , envir=start.values.env, inherits=FALSE)
		default.sv <- FALSE
	} else {
		default.sv <- TRUE
	}

	# put used start values on environment to return
	# if( is.null( start.values.env ) ){
		# used.start.values.env <- new.env()
		# svs <- c( "A0.sv", "Achange.sv", "Q0.sv" )
		# for( i in 1:length( svs ) ){
			# assign( svs[i], eval( parse( text=svs[i] ) ), envir = used.start.values.env, inherits = FALSE, immediate=TRUE )
		# }
	# } else {
		# used.start.values.env <- start.values.env
	# }

	## alignment with fixed values
	if( !is.null( par.env ) ){
		if( "A0"      %in% ls(envir=par.env) )      A0.par      <- get('A0'         , envir=par.env, inherits=FALSE) else A0.par <- NULL
		if( "Achange" %in% ls(envir=par.env) )      Achange.par <- get('Achange'    , envir=par.env, inherits=FALSE) else Achange.par <- NULL
		if( "Q0"      %in% ls(envir=par.env) )      Q0.par      <- get('Q0'         , envir=par.env, inherits=FALSE) else Q0.par <- NULL

		for (i in 1:dim(A0.sv)[1]) { # !!!dimensions, must match for all matrices
			for (j in 1:dim(A0.sv)[2]) { # !!!dimensions, must match for all matrices
				if( !is.null( A0.par )      && !is.na( suppressWarnings( as.numeric( A0.par[i,j] ) ) ) )      A0.sv[i,j] <- NA
				if( !is.null( Achange.par ) && !is.na( suppressWarnings( as.numeric( Achange.par[i,j] ) ) ) ) Achange.sv[i,j] <- NA
				if( !is.null( Q0.par )      && !is.na( suppressWarnings( as.numeric( Q0.par[i,j] ) ) ) )      Q0.sv[i,j] <- NA
			}
		}
	}

	# MH 0.0.41 2024-05-27 new complete matrix to check complete starting value matrix for positive definiteness
	### there will be problems when par.env is not set, ignore now, only relevant for manual call of start.values()
	Q0.sv.full <- Q0.sv
	for (i in 1:dim(Q0.sv.full)[1]) {
		for (j in 1:dim(Q0.sv.full)[2]) {
			if( !is.null( Q0.par )      && !is.na( suppressWarnings( as.numeric( Q0.par[i,j] ) ) ) ) Q0.sv.full[i,j] <- as.numeric( Q0.par[i,j] )
		}
	}
	# check Q0 and its Cholesky factor for positive definiteness
	if( !is_positive_definite( Q0.sv.full ) ) stop( "starting value Q0 matrix is not positive definite" )
	Q0Chol.sv.full <- chol(Q0.sv.full)
	if( !is_positive_definite( Q0Chol.sv.full ) ) stop( "cholesky factor of starting value Q0 matrix is not positive definite" )
	if( verbose ){
			cat( paste0( "just for interest:\n" ) )
			cat( paste0( "positive definite start values Q0 (with set values for fixed parameters):\n" ) )
			print( Q0.sv.full )
			cat( paste0( "positive definite start values cholesky Q0:\n" ) )
			print( Q0Chol.sv.full )
			# cat( paste0( "----------------------------------------------------------------------------\n" ) )
	}

	# A0.sv.full (full in the sense that fixed parameters are filled in
	A0.sv.full <- A0.sv
	for (i in 1:dim(A0.sv.full)[1]) {
		for (j in 1:dim(A0.sv.full)[2]) {
			if( !is.null( A0.par )      && !is.na( suppressWarnings( as.numeric( A0.par[i,j] ) ) ) ) A0.sv.full[i,j] <- as.numeric( A0.par[i,j] )
		}
	}


	## check total variance and cholesky factor
	# discrete-time autoregressive matrix
	delta <- 1
	Adelta <- expm( A0.sv.full * delta ) # TODO: add Achange
	# discrete-time process error covariance matrix
	Ah <- A0.sv.full %x% diag( F ) + diag( F ) %x% A0.sv.full
	# ( Qdelta <- irow( solve( Ah ) %*% ( expm( Ah * delta ) - diag( F^2 ) ) %*% row( Q ) ) )
	# asymptotic diffusion matrix
	Sigmaw.sv <- irow( -1*solve( Ah ) %*% row(Q0.sv.full) )
	if( !is_positive_definite( Sigmaw.sv ) ) stop( "starting value Sigmaw matrix is not positive definite" )
	SigmawChol.sv <- chol( Sigmaw.sv )
	if( !is_positive_definite( SigmawChol.sv ) ) stop( "cholesky factor of jittered starting value Sigmaw matrix is not positive definite" )
	if( verbose ){
			cat( paste0( "positive definite start values Sigmaw:\n" ) )
			print( Q0.sv.full )
			cat( paste0( "positive definite start values cholesky Sigmaw:\n" ) )
			print( Q0Chol.sv.full )
			cat( paste0( "----------------------------------------------------------------------------\n" ) )
	}

	# relevant matrices
	matrs <- c( "A0", "Achange", "Q0" )

	if( !jitter ){

		# MH 0.0.41 2024-05-27, full free matrices can go into as is, but for mixed matrices, only start values for elements can go in
		sv.list <- sv.to.list( matrs=matrs, matr.suffix=".sv", env=environment() )
		
		# init_fun <- eval(parse(text=paste0( "function(...) c( list( ",ifelse( between.mu, "'Sigmamu'=Sigmamu, ", "" ),'A0'=A0, 'Achange'=Achange, 'Q0'=Q0," ) )" ))) # ,sigmaepsQ.sv,", "  'epsQt'=epsQt, , 'Qchange'=Qchange, , ",sigmaepsmu.sv,", ",sigmaeps.sv,", ",lambda.sv," sigmaepsA.sv, ,'epsAt'=epsAt
		# init <- function(...) list( 'A0'=A0.sv, 'Achange'=Achange.sv, 'Q0'=Q0.sv, 'Q0Chol'=Q0Chol.sv ) # ,sigmaepsQ.sv,", "  'epsQt'=epsQt, , 'Qchange'=Qchange, , ",sigmaepsmu.sv,", ",sigmaeps.sv,", ",lambda.sv," sigmaepsA.sv, ,'epsAt'=epsAt
		# init <- function(...) list( 'A0'=A0.sv, 'Achange'=Achange.sv, 'q011'=1, 'q022'=1 ) # ,sigmaepsQ.sv,", "  'epsQt'=epsQt, , 'Qchange'=Qchange, , ",sigmaepsmu.sv,", ",sigmaeps.sv,", ",lambda.sv," sigmaepsA.sv, ,'epsAt'=epsAt
		init <- function(...) eval(parse(text="sv.list"))

		if( verbose ){
			cat( paste0( ifelse( default.sv, "default ", "" ),"start values ",ifelse( chains>1, paste0("(for all ",chains," chains)"), "(for the one and only chain)" ),":\n" ) )
			
			inits <- init()
			for( par in names( inits ) ){
				cat( paste0( par, ":\n" ) )
				print( inits[[par]] )
			}
		}
		
		# seed for jittering (for returning)
		seed.jitter.sv <- NULL
		
	} else {
		
		# jitter defaults
		# A0.jitter      <- get( "A0", envir=empty.str.env, inherits=FALSE )
		# Achange.jitter <- get( "Achange", envir=empty.str.env, inherits=FALSE )
		# Q0.jitter      <- get( "Q0", envir=empty.str.env, inherits=FALSE )
		A0.jitter <- Achange.jitter <- Q0.jitter <- matrix( as.numeric(NA), F, F )		
		
		A0.jitter[] <- Achange.jitter[] <- Q0.jitter[] <- "runif(1,-0.25,0.25)"
		# diag( A0.jitter ) <- "runif(1,-0.25,0.25)"
		# Achange.jitter[] <- "runif(1,-0.25,0.25)"
		# Q0.jitter[] <- "runif(1,-0.25,0.25)"
		# diag( Q0.jitter ) <- "runif(1,-0.25,0.25)"
		
		# get elements of jitter.env and overwrite defaults
		if( !is.null( jitter.env ) ){
			if( "A0"      %in% ls(envir=jitter.env) )      A0.jitter      <- get('A0'         , envir=jitter.env, inherits=FALSE)
			if( "Achange" %in% ls(envir=jitter.env) )      Achange.jitter <- get('Achange'    , envir=jitter.env, inherits=FALSE)
			if( "Q0"      %in% ls(envir=jitter.env) )      Q0.jitter      <- get('Q0'         , envir=jitter.env, inherits=FALSE)
		}

		# seed
		if( seed %in% "random" ){
			seed <- sample( 1:999999999, 1 )
		}
		set.seed( seed )

		# seed for jittering (for returning)
		seed.jitter.sv <- seed

		# generate jittered start values
		init <- sapply( 1:chains, function(x) NULL, simplify=FALSE )
		if( verbose ) cat( paste0( "trying to generate jittered start values for Q0\n" ) )
		for( i in 1:chains ){
			keep.trying <- TRUE
			tries.max <- 100
			try <- 1
			while( keep.trying && try <= tries.max ){
				if( verbose ) {
					if( try==1 ) cat( paste0( "chain ", i, " ." ) ) else cat( "." )
					flush.console()
				}
				
				A0.temp <- A0.sv
				Achange.temp <- Achange.sv
				Q0.temp <- Q0.sv
				for (j in 1:dim(A0.temp)[1]) { # !!!dimensions
					for (k in 1:dim(A0.temp)[2]) { # !!!dimensions
						A0.temp[j,k]      <- A0.temp[j,k] + eval(parse(text=A0.jitter[j,k]))
						Achange.temp[j,k] <- Achange.temp[j,k] + eval(parse(text=Achange.jitter[j,k]))
					}
				}
				for (j in 1:dim(Q0.temp)[1]) {
					for (k in 1:j) {
						Q0.temp[j,k]      <- Q0.temp[j,k] + eval(parse(text=Q0.jitter[j,k]))
						Q0.temp[k,j]      <- Q0.temp[j,k]
					}
				}				

	
				# MH 0.0.41 2024-05-27 new complete matrix to check complete starting value matrix for positive definiteness
				### there will be problems when par.env is not set, ignore now, only relevant for manual call of start.values()
				Q0.temp.full <- Q0.temp
				for (j in 1:dim(Q0.temp.full)[1]) {
					for (k in 1:dim(Q0.temp.full)[2]) {
						if( !is.null( Q0.par )      && !is.na( suppressWarnings( as.numeric( Q0.par[j,k] ) ) ) ) Q0.temp.full[j,k] <- as.numeric( Q0.par[j,k] )
					}
				}
				# check Q0 and its Cholesky factor for positive definiteness
				check1 <- is_positive_definite( Q0.temp.full )
				# if( !check1 ) stop( "jittered starting value Q0 matrix is not positive definite" )
				Q0Chol.temp.full <- chol(Q0.temp.full)
				check2 <- is_positive_definite( Q0Chol.temp.full )
				# if( !check2 ) stop( "cholesky factor of jittered starting value Q0 matrix is not positive definite" )
	
				## check total variance and cholesky factor
				# discrete-time autoregressive matrix
				delta <- 1
				Adelta <- expm( A0.temp * delta ) # TODO: add Achange
				# discrete-time process error covariance matrix
				Ah <- A0.temp %x% diag( F ) + diag( F ) %x% A0.temp
				# ( Qdelta <- irow( solve( Ah ) %*% ( expm( Ah * delta ) - diag( F^2 ) ) %*% row( Q ) ) )
				# asymptotic diffusion matrix
				Sigmaw.temp <- irow( -1*solve( Ah ) %*% row(Q0.temp.full) )
				# check3 <- is.positive.definite( Sigmaw.temp )
				# if( !check3 ) stop( paste0( "chain ",i,": jittered starting value Sigmaw matrix is not positive definite" ) )
				check3 <- is_positive_definite( Sigmaw.temp )
				# if( !check4 ) stop( paste0( "chain ",i,": jittered starting value Sigmaw matrix is not positive definite (test 2)" ) )
				SigmawChol.temp <- chol( Sigmaw.temp )
				check4 <- is_positive_definite( SigmawChol.temp )
				# if( !check5 ) stop( paste0( "chain ",i,": cholesky factor of jittered starting value Sigmaw matrix is not positive definite (test 2)" ) )
				# if( verbose ){
						# cat( paste0( "positive definite start values Sigmaw:\n" ) )
						# print( Q0.temp.full )
						# cat( paste0( "positive definite start values cholesky Sigmaw:\n" ) )
						# print( Q0Chol.temp.full )
						# cat( paste0( "----------------------------------------------------------------------------\n" ) )
				# }			
				
				# init[[i]] <- list( "A0"=A0.temp, "Achange"=Achange.temp, "Q0"=Q0.temp )
				init[[i]] <- sv.to.list( matrs=matrs, matr.suffix=".temp", env=environment() )
				
				if( all( c(check1,check2,check3,check4) ) ) {
					keep.trying <- FALSE
					if( verbose ) {
						cat( "success\n" )
						flush.console()
					}
				}
				try <- try+1
			}
			if( keep.trying ) stop( paste0( "did not find jittered start values for Q0 for chain ", i, " after ", tries.max, " tries." ) )
		}

		if( verbose ){
			cat( paste0( "jittered",ifelse( default.sv, " default", "" )," start values ",ifelse( chains>1, paste0("(different for each of the ",chains," chains)"), "(for the one and only chain)" ),":\n" ) )
			for( i in 1:chains ){
				cat( paste0( "chain ", i ,":\n" ) )
				for( par in names( init[[i]] ) ){
					cat( paste0( par, ":\n" ) )
					print( init[[i]][[par]] )
				}
			}
		}
	
	}

	if( return.init.only ){
		# return init object for Stan
		ret <- init
	} else {

		if( !jitter ){
			sv. <- convert.matrix.to.long( init() )
			sv <- merge( sv., data.frame( "chain"=1:chains ) )
		} else {
			sv.list <- sapply( init, function( lst ) { convert.matrix.to.long( lst ) }, simplify=FALSE )
			sv.list2 <- sapply( 1:length(init), function( chain ) { df <- sv.list[[chain]]; df$chain <- chain; df }, simplify=FALSE )
			sv <- do.call( "rbind", sv.list2 )
			# matrix name without ".sv"
			sv$matrix <- sub( ".sv", "", sv$matrix )
		}
		# column sort
		sv <- sv[, c( "chain", colnames( sv )[!colnames(sv) %in% "chain"] )]
		
		# return list
		ret <- list( "init"=init, "seed.jitter.sv"=seed.jitter.sv, "sv"=sv )
	}
	
	
	# return
	return( ret )
}


### development
# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile, "Dropbox/139_conttime/conttime/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("start.values.R") ]
# for( Rfile in Rfiles ) source( file.path( Rfiles.folder, Rfile ) )

### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
