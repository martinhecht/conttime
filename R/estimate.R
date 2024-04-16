## Changelog:
# MH 2024-04-16: set up

## Documentation
#' @title
#' @description
#' @param
#' @param
#' @param
#' @return


### development
user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
Rfiles.folder <- file.path( user.profile,
                                    "Dropbox/139_conttime/conttime/R" )
Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
Rfiles <- Rfiles[ !Rfiles %in% c("estimate.R") ]
for( Rfile in Rfiles ){
	source( file.path( Rfiles.folder, Rfile ) )
}

## Function definition
# estimate <- function( env=NULL, run.dir="C:/users/martin/Desktop/temp", verbose=FALSE ){
env=NULL
run.dir="C:/users/martin/Desktop/temp"

		
		### based on tvct_v1.pdf (2024-04-04)

		# require packages
		require( rstan )
		# options( mc.cores = (parallel::detectCores()-1) )
		options( mc.cores = 1 )
		rstan_options(auto_write = TRUE)
		
		# if not here, get empty structures and gen data
		if( is.null( env ) ) {
			env2 <- gen.empty.structures(I=4)
			env <- gen.data( env=env2 )
		}

		# put all elements from environment here
		names <- ls(envir=env)
		for( i in 1:length( names ) ){
			eval( parse( text=paste0( names[i], " <- get('", names[i], "', envir=env)" ) ) )
		}
		
		# Stan doesn't allow NA in data, replace by 999999
		yjp[ is.na( yjp ) ] <- 999999

		# Tests
		# Delta[2,1] <- "lambda21"
		# Delta[] <- "lambda"
		# Delta[] <- NA

		## determine structure type
		# free, fixed, mixed (free parameters and fixed values)
		probe.structures <- c("Delta")
		structure.type <- array( as.character(NA), dim=length(probe.structures) )
		names( structure.type ) <- probe.structures
		for( i in 1:length(structure.type) ){
			vals <- eval( parse( text=paste0( "matrix( ",probe.structures[i],", ncol=1 )[,1]" ) ) )
			if ( all ( is.numeric( vals ) ) ) type <- "fixed" else if ( all( is.na( suppressWarnings( as.numeric( vals ) ) ) ) | all( is.na( vals ) ) ) type <- "free" else type <- "mixed"
			structure.type[i] <- type
			type <- NULL
		}
		# structure dimensions (symbolic)
		structure.dim          <- c( "matrix[I,F]" )
		names( structure.dim ) <- c( "Delta"       )

		### stan syntax
		x <- paste0( "// ", date() )
		## data
		x <- c( x, paste0( "data {" ) )
		x <- c( x, paste0( "  int<lower=0> F; // number of processes" ) )
		x <- c( x, paste0( "  int<lower=0> I; // number of items" ) )
		x <- c( x, paste0( "  int<lower=0> N; // number of persons" ) )
		x <- c( x, paste0( "  int<lower=0> T; // number of maximum time points of persons" ) )
		x <- c( x, paste0( "  int<lower=0> Tunique; // number of all unique time points" ) )
		x <- c( x, paste0( "  array[N] int Tj; // number of individual time points" ) )
		x <- c( x, paste0( "  array[I,1,N,T] real yjp; // responses of persons per time points" ) )
		# fixed structures go in as data
		if( length( wfs <- which( structure.type %in% "fixed" ) ) > 0 ){
			for( st in names( structure.type[wfs] ) ){
				eval( parse( text=paste0( "x <- c( x, paste0( '  ",structure.dim[st]," ",st,";' ) )" ) ) )
			}
		}
		x <- c( x, paste0( "}" ) )
		# parameters
		x <- c( x, paste0( "parameters {" ) )
		x <- c( x, paste0( "  vector[F] mu;" ) )
		x <- c( x, paste0( "  cov_matrix[F] Sigma;" ) )
		x <- c( x, paste0( "  cov_matrix[I] Sigmaeps; // measurement error covariance matrix" ) )
		x <- c( x, paste0( "  array[F,1,N,T] real thetajp; // latent values of persons per time point" ) )
		# x <- c( x, paste0( "  real lambda21;" ) )
		# x <- c( x, paste0( "  real lambda42;" ) )
		# x <- c( x, paste0( "  matrix[I,F] Delta;" ) )
		x <- c( x, paste0( "}" ) )
		# transformed parameters
		# x <- c( x, paste0( "transformed parameters {" ) )
		# x <- c( x, paste0( "  matrix[I,F] Delta;" ) )
		# x <- c( x, paste0( "  Delta[1,1] <- 1;" ) )
		# x <- c( x, paste0( "  Delta[2,1] <- lambda21;" ) )
		# x <- c( x, paste0( "  Delta[3,1] <- 0;" ) )
		# x <- c( x, paste0( "  Delta[4,1] <- 0;" ) )
		# x <- c( x, paste0( "  Delta[1,2] <- 0;" ) )
		# x <- c( x, paste0( "  Delta[2,2] <- 0;" ) )
		# x <- c( x, paste0( "  Delta[3,2] <- 1;" ) )		
		# x <- c( x, paste0( "  Delta[4,2] <- lambda42;" ) )		
		# x <- c( x, paste0( "}" ) )		
		# model
		x <- c( x, paste0( "model {" ) )
		x <- c( x, paste0( "  // ####################################" ) )
		x <- c( x, paste0( "  for (j in 1:N){" ) )
		x <- c( x, paste0( "    for (p in 1:Tj[j]){" ) )
		x <- c( x, paste0( "      //  " ) )
		x <- c( x, paste0( "      to_vector( thetajp[,1,j,p] ) ~ multi_normal(mu,Sigma);" ) )
		x <- c( x, paste0( "    }" ) )
		x <- c( x, paste0( "  }" ) )
		x <- c( x, paste0( "  // ##### measurement model #################" ) )
		# x <- c( x, paste0( "  lambda21 ~ normal( 1, 0.25 );" ) )
		# x <- c( x, paste0( "  lambda42 ~ normal( 1, 0.25 );" ) )		
		x <- c( x, paste0( "  for (j in 1:N){" ) )
		x <- c( x, paste0( "    for (p in 1:Tj[j]){" ) )
		x <- c( x, paste0( "      // measurement model, Eq. 18/19 " ) )
		x <- c( x, paste0( "      to_vector( yjp[,1,j,p] ) ~ multi_normal( Delta*to_vector(thetajp[,1,j,p]),Sigmaeps);" ) )
		x <- c( x, paste0( "    }" ) )
		x <- c( x, paste0( "  }" ) )
		x <- c( x, paste0( "}" ) )
		# empty line
		x <- c( x, paste0( "" ) )
		
		## write file
		file.name <- "model"
		syntax.path <- file.path( run.dir, paste0( file.name, ".stan" ) )
		write( x, file=syntax.path, sep="\n" )
		# remove rds
		rds.path <- file.path( run.dir, paste0( file.name, ".rds" ) )
		invisible( if( file.exists( rds.path ) ) file.remove( rds.path ) )
		
		# data
		# dat <- list( "F"=F, "I"=I, "N"=N, "T"=T, "Tunique"=Tunique, "Tj"=Tj, "yjp"=yjp, "Delta"=Delta )
		dat <- list( "F"=F, "I"=I, "N"=N, "T"=T, "Tunique"=Tunique, "Tj"=Tj, "yjp"=yjp )
		# add fixed structures
		if( length( wfs ) > 0 ){
			for( st in names( structure.type[wfs] ) ){
				eval( parse( text=paste0( "dat$", st, " <- ", st ) ) ) 
			}
		}
		
		# https://discourse.mc-stan.org/t/how-to-define-initial-values-in-stan-in-r/16855
		# init_fun <- function(...) list("lambda21"=1,"lambda42"=1)
		
		# fit
		# fit <- stan( file = syntax.path, data = dat, chains = 1, iter = 10, init=init_fun )
		fit <- stan( file = syntax.path, data = dat, chains = 1, iter = 10, init=init_fun )
		
		print( fit )

		
		cat(x,sep="\n")


}


### development
user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
Rfiles.folder <- file.path( user.profile,
                                    "Dropbox/139_conttime/conttime/R" )
Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
Rfiles <- Rfiles[ !Rfiles %in% c("estimate.R") ]
for( Rfile in Rfiles ){
	source( file.path( Rfiles.folder, Rfile ) )
}

m <- estimate()

# ls( envir=m )

# while( TRUE ){
	# e <- gen.empty.structures()
	# e <- gen.empty.structures(T=5,N=8,F=2,I=11)
	# m <- gen.data(env=e)
	# print( get( "Tj", envir=m, inherits=FALSE ) ); flush.console()
# }

# ( Tj <- get( "Tj", envir=m, inherits=FALSE ) )
# ( yjp <- get( "yjp", envir=m, inherits=FALSE ) )
# get( "Delta", envir=m, inherits=FALSE )
# get( "Sigmaeps", envir=m, inherits=FALSE )
# get( "A0", envir=m, inherits=FALSE )
# get( "Achange", envir=m, inherits=FALSE )
# get( "Tj", envir=m, inherits=FALSE )
# get( "deltajp", envir=m, inherits=FALSE )
# get( "N", envir=m, inherits=FALSE )



### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
