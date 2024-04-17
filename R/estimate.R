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
# estimate <- function( data.env=NULL, run.dir="C:/users/martin/Desktop/temp", verbose=FALSE ){
data.env=NULL
run.dir="C:/users/martin/Desktop/temp"

		
		### based on tvct_v1.pdf (2024-04-04)

		# require packages
		require( rstan )
		# options( mc.cores = (parallel::detectCores()-1) )
		options( mc.cores = 1 )
		rstan_options(auto_write = TRUE)
		
		# if no data provided, get empty structures and gen data
		if( is.null( data.env ) ) {
			str.env <- gen.empty.structures(I=4)
			data.env <- gen.data( env=str.env )
		}
		# put all relevant data and model parameter elements from data environment here
		# names <- ls(envir=env)
		data.structures <- c("F", "I", "N", "T", "Tunique", "Tj", "yjp")
		model.parameters <- c( "Delta", "Sigmaeps" )
		names <- c( data.structures, model.parameters )
		for( i in 1:length( names ) ){
			eval( parse( text=paste0( names[i], " <- get('", names[i], "', envir=data.env)" ) ) )
		}
		# Stan doesn't allow NA in data, replace by 999999
		yjp[ is.na( yjp ) ] <- 999999

		# Tests
		### Delta[2,1] <- "lambda21"
		# Delta[2,1] <- NA
		# Delta[4,2] <- NA
		### Delta[] <- "lambda"
		### Delta[] <- NA
		### Sigmaeps[] <- "sigmaeps"
		
		# Defaults
		### TODO: ordentliche Defaults fuer Delta
		Delta[2,1] <- NA
		Delta[4,2] <- NA
		Sigmaeps[] <- NA
		Sigmaeps[ lower.tri( Sigmaeps ) ] <- 0
		Sigmaeps[ upper.tri( Sigmaeps ) ] <- 0

		## determine structure type: free, fixed, mixed (free parameters and fixed values)
		structure.type <- array( as.character(NA), dim=length(model.parameters) )
		names( structure.type ) <- model.parameters
		for( i in 1:length(structure.type) ){
			vals <- eval( parse( text=paste0( "matrix( ",model.parameters[i],", ncol=1 )[,1]" ) ) )
			if ( all ( is.numeric( vals ) & !is.na( vals ) ) ) type <- "fixed" else if ( all( is.na( suppressWarnings( as.numeric( vals ) ) ) ) | all( is.na( vals ) ) ) type <- "free" else type <- "mixed"
			structure.type[i] <- type
			type <- NULL
		}
		# structure dimensions (symbolic)
		structure.dim          <- c( "matrix[I,F]", "cov_matrix[I]" )
		names( structure.dim ) <- c( "Delta"      , "Sigmaeps"      )
		
		# parameter labels for each mixed structure
		par.lab.mixed.str          <- c( "lambda", "sigmaeps" )
		names( par.lab.mixed.str ) <- c( "Delta" , "Sigmaeps" )

		# get parameters of mixed structures
		parameters.of.mixed.structures <- character(0)
		if( length( wimis <- which( structure.type %in% "mixed" ) ) > 0 ){
			for( st in names( structure.type[wimis] ) ){
				# if NA in mixed structure, label with parameter name
				for( c in 1:eval(parse(text=paste0("ncol(",st,")"))) ) {
					for( r in 1:eval(parse(text=paste0("nrow(",st,")"))) ){
						if( !is.null( par.lab.mixed.str ) & (st %in% names( par.lab.mixed.str )) ) par.lab <- par.lab.mixed.str[st] else par.lab <- st
						eval(parse(text=paste0("if( is.na( ",st,"[",r,",",c,"] ) ) ",st,"[",r,",",c,"] <- '",par.lab,r,c,"'")))
					}
				}				
				# add parameters to vector
				parameters.of.mixed.structures <- c( parameters.of.mixed.structures, eval( parse( text=paste0( "matrix( ",st,", ncol=1 )[,1]" ) ) ) )
			}
		}
		if( length( parameters.of.mixed.structures ) > 0 ){
			parameters.of.mixed.structures <- unique( parameters.of.mixed.structures[ is.na( suppressWarnings( as.numeric( parameters.of.mixed.structures ) ) ) ] )
		}

		### stan syntax
		x <- paste0( "// ", date() )
		
		## data
		x <- c( x, paste0( "data {" ) )
		x <- c( x, paste0( "  int<lower=1> F; // number of processes" ) )
		x <- c( x, paste0( "  int<lower=1> I; // number of items" ) )
		x <- c( x, paste0( "  int<lower=1> N; // number of persons" ) )
		x <- c( x, paste0( "  int<lower=2> T; // number of maximum time points of persons" ) )
		x <- c( x, paste0( "  int<lower=2> Tunique; // number of all unique time points" ) )
		x <- c( x, paste0( "  array[N] int Tj; // number of individual time points" ) )
		x <- c( x, paste0( "  array[I,1,N,T] real yjp; // responses of persons per time points" ) )
		# fixed structures go in as data
		if( length( wifis <- which( structure.type %in% "fixed" ) ) > 0 ){
			for( st in names( structure.type[wifis] ) ){
				eval( parse( text=paste0( "x <- c( x, paste0( '  ",structure.dim[st]," ",st,";' ) )" ) ) )
			}
		}
		# end data
		x <- c( x, paste0( "}" ) )
		
		## parameters
		x <- c( x, paste0( "parameters {" ) )
		x <- c( x, paste0( "  vector[F] mu;" ) )
		x <- c( x, paste0( "  cov_matrix[F] Sigma;" ) )
		x <- c( x, paste0( "  array[F,1,N,T] real thetajp; // latent values of persons per time point" ) )
		# free structures go in as parameters
		if( length( wifres <- which( structure.type %in% "free" ) ) > 0 ){
			x <- c( x, paste0( "  // free structures: ", paste( names( structure.type[wifres] ), collapse=", " ) ) )
			for( st in names( structure.type[wifres] ) ){
				eval( parse( text=paste0( "x <- c( x, paste0( '  ",structure.dim[st]," ",st,";' ) )" ) ) )
			}
		}		
		# parameters of mixed structures
		if( length( parameters.of.mixed.structures ) > 0 ){		
			x <- c( x, paste0( "  // parameters of mixed structures" ) )
			eval( parse( text=paste0("x <- c( x, '  real ",parameters.of.mixed.structures,";' )" ) ) )
		}
		# end parameters
		x <- c( x, paste0( "}" ) )
		
		## transformed parameters
		x <- c( x, paste0( "transformed parameters {" ) )
		# mixed structures go in as transformed parameters
		# if( length( wimis <- which( structure.type %in% "mixed" ) ) > 0 ){
		if( length( wimis ) > 0 ){
			x <- c( x, paste0( "  // mixed structures: ", paste( names( structure.type[wimis] ), collapse=", " ) ) )
			for( st in names( structure.type[wimis] ) ){
				eval( parse( text=paste0( "x <- c( x, paste0( '  ",structure.dim[st]," ",st,";' ) )" ) ) )
				for( c in 1:eval(parse(text=paste0("ncol(",st,")"))) ) {
					for( r in 1:eval(parse(text=paste0("nrow(",st,")"))) ){
						x <- c( x, paste0( "  ",st,"[",r,",",c,"] <- ",eval(parse(text=paste0(st,"[",r,",",c,"]"))),";" ) )
					}
				}
			}
		}
		x <- c( x, paste0( "}" ) )
		
		## model
		x <- c( x, paste0( "model {" ) )
		x <- c( x, paste0( "  // ####################################" ) )
		x <- c( x, paste0( "  for (j in 1:N){" ) )
		x <- c( x, paste0( "    for (p in 1:Tj[j]){" ) )
		x <- c( x, paste0( "      //  " ) )
		x <- c( x, paste0( "      to_vector( thetajp[,1,j,p] ) ~ multi_normal(mu,Sigma);" ) )
		x <- c( x, paste0( "    }" ) )
		x <- c( x, paste0( "  }" ) )
		x <- c( x, paste0( "  // ##### measurement model #########################################" ) )
		x <- c( x, paste0( "  for (j in 1:N){" ) )
		x <- c( x, paste0( "    for (p in 1:Tj[j]){" ) )
		x <- c( x, paste0( "      // measurement model, Eq. 18/19 " ) )
		x <- c( x, paste0( "      to_vector( yjp[,1,j,p] ) ~ multi_normal( Delta*to_vector(thetajp[,1,j,p]),Sigmaeps);" ) )
		x <- c( x, paste0( "    }" ) )
		x <- c( x, paste0( "  }" ) )
		# priors for parameters of mixed structures
		if( length( parameters.of.mixed.structures ) > 0 ){		
			x <- c( x, paste0( "  // ##### priors for parameters of mixed structures #################" ) )
			eval( parse( text=paste0("x <- c( x, '  ",parameters.of.mixed.structures," ~ normal( 1, 0.25 );' )" ) ) )
		}
		# end model
		x <- c( x, paste0( "}" ) )
		
		## empty line
		x <- c( x, paste0( "" ) )
		
		## write file
		file.name <- "model"
		syntax.path <- file.path( run.dir, paste0( file.name, ".stan" ) )
		write( x, file=syntax.path, sep="\n" )
		# remove rds
		rds.path <- file.path( run.dir, paste0( file.name, ".rds" ) )
		invisible( if( file.exists( rds.path ) ) file.remove( rds.path ) )

		## data
		dat <- list( "F"=F, "I"=I, "N"=N, "T"=T, "Tunique"=Tunique, "Tj"=Tj, "yjp"=yjp )
		# add fixed structures
		if( length( wifis ) > 0 ){
			for( st in names( structure.type[wifis] ) ){
				eval( parse( text=paste0( "dat$", st, " <- ", st ) ) ) 
			}
		}
		
		## start values for parameters of mixed structures
		# https://discourse.mc-stan.org/t/how-to-define-initial-values-in-stan-in-r/16855
		if( length( parameters.of.mixed.structures ) > 0 ){		
			eval(parse(text=paste0( "init_fun <- function(...) list(  ",paste( paste0( "'", parameters.of.mixed.structures, "'=1" ), collapse=", " ),"  )" )))
		} else {
			init_fun <- "random"
		}
		
		# fit
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
