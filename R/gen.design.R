## Changelog:
# MH 2024-04-26: set up

## Documentation
#' @title Generate design
#' @description Generate an individualized longitudinal design.
#' @param F number of processes (variables), must be >= 1
#' @param N number of persons, must be >= 1
#' @param T the maximum number of time points for each person, must be >= 2
#' @param Tdiv a negative number or zero indicating the extent to which the individualized number of time points may vary, T - Tdiv must be >= 2
#' @param deltas possible time lengths between consecutive time points, used for sampling with the \code{sample()} function
#' @param first.time.points possible first time points, used for sampling with the \code{sample()} function
#' @param env an environment where return objects are written; if \code{NULL}, a new environment is created
#' @param seed a number used as the seed for \code{set.seed(seed)} or the value "random" for the random generation of a seed
#' @param verbose a logical value indicating whether to print detailed messages and progress updates during the execution of the function
#' @return An environment is returned containing design characteristics and generated time points. Use \code{ls(envir=<returned environment>)} to view its contents.

## Function definition
gen.design <- function( F=2, N=5, T=3, Tdiv=-1, deltas=c(0.5,1,1.5), first.time.points=c(-1,-0.5,0,0.5,1), env=NULL, seed="random", verbose=FALSE ){
		
		# number of items
		I=F
		
		### based on tvct_v1.pdf (2024-04-04)

		# checks
		# if( ! (F == 2) ) stop( paste0( "F = ", F, " is not supported, currently only F=2 is available" ) )

		# create new environment
		temp.env <- new.env()

		# put all design input elements on environment
		design.char <- c( "F", "I", "N", "T" )
		for( i in 1:length( design.char ) ){
			assign( design.char[i], eval( parse( text=design.char[i] ) ), envir = temp.env, inherits = FALSE, immediate=TRUE )
		}

		## get empty design-related structures
		str.env <- gen.empty.structures( env=temp.env )
		# put all needed structures here
		str.names <- c( "tjp", "deltajp", "ptuniquejp" )
		# for( i in 1:length( str.names ) ){
			# eval( parse( text=paste0( str.names[i], " <- get('", str.names[i], "', envir=str.env, inherits=FALSE)" ) ) )
		# }
		tjp <- get('tjp', envir=str.env, inherits=FALSE)
		deltajp <- get('deltajp', envir=str.env, inherits=FALSE)
		ptuniquejp <- get('ptuniquejp', envir=str.env, inherits=FALSE)
		
		# seed
		if( seed %in% "random" ){
			gen.design.seed <- sample( 1:999999999, 1 )
		} else {
			gen.design.seed <- seed
		}
		set.seed( gen.design.seed )

		## gen design characteristics

		# lowest individual T
		low <- ifelse( T+Tdiv >=2, T+Tdiv, 2 )
		if( N == 1 ){
			Tj <- T
		} else if( N == 2 ){
			Tj <- c( sample( as.character(min(low,T):T), N-1, replace = TRUE ), T )
		} else if( N >= 3 ){
			Tj <- c( min(low,T), sample( as.character(min(low,T):T), N-2, replace = TRUE ), T )
		} else {
			stop( paste0( "N = ", N, " not >= 1\n" ) )
		}
		# sorting of Tj very important!!
		Tj <- sort( as.integer( Tj ) )

		# deltajp
		for( j in 1:N ){
			for( p in 1:(Tj[j]-1) ){
				deltajp[j,p] <- sample( deltas, 1 )
			}
		}

		# tjp
		for( j in 1:N ){
			# first time point
			tjp[j,1] <- sample( first.time.points, 1 )
			for( p in 2:Tj[j] ){
				tjp[j,p] <- tjp[j,p-1] + deltajp[j,p-1]
			}
		}

		# all unique time points
		tunique2 <- unique( sort ( sapply( tjp, "c" ) ) )
		tunique2 <- tunique2[ !is.na( tunique2 ) ]
		
		# number of unique time points
		Tunique <- length( tunique2 )

		# just for consistency, get tunique structure (which depends on Tunique) and fill again
		assign( "Tunique", eval( parse( text="Tunique" ) ), envir = temp.env, inherits = FALSE, immediate=TRUE )
		str.env2 <- gen.empty.structures( env=temp.env )
		eval( parse( text=paste0( "tunique", " <- get('", "tunique", "', envir=str.env2)" ) ) )
		tunique[] <- tunique2
		
		# indices of individual time points in tunique vector
		for ( r in 1:nrow(ptuniquejp) ){
			for ( c in 1:ncol(ptuniquejp) ){
				if( !is.na( tjp[r,c] ) ){
					ptuniquejp[r,c] <- which( tunique %in% tjp[r,c] )
				}
			}
		}
		
		# structures that should be returned
		str.names <- c( design.char, "Tunique", "Tj", str.names, "tunique", "ptuniquejp", "gen.design.seed" )

		# create environment if not provided
		if( is.null( env ) ) env <- new.env()
		
		# assign
		for( i in 1:length( str.names ) ){
			assign( str.names[i], eval( parse( text=str.names[i] ) ), envir = env, inherits = FALSE, immediate=TRUE )
		}
		
		# return
		return( env )
}


### development
# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    # "Dropbox/139_conttime/conttime/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("gen.design.R") ] # ,"estimate.R"
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }

# m <- gen.design()
# ls( envir=m )

# get( "Tj", envir=m, inherits=FALSE )
# get( "tjp", envir=m, inherits=FALSE )
# get( "tunique", envir=m, inherits=FALSE )
# get( "Tunique", envir=m, inherits=FALSE )
# get( "ptuniquejp", envir=m, inherits=FALSE )
# get( "deltajp", envir=m, inherits=FALSE )

# while( TRUE ){
	# m <- gen.design( N=sample(1:5,1),T=sample(2:5,1),Tdiv=sample(-3:0,1) )
	# print( get( "Tj", envir=m, inherits=FALSE ) )
    # print( get( "tjp", envir=m, inherits=FALSE ) )
    # print( get( "tunique", envir=m, inherits=FALSE ) )
    # print( get( "Tunique", envir=m, inherits=FALSE ) )
    # print( get( "ptuniquejp", envir=m, inherits=FALSE ) )
    # print( get( "deltajp", envir=m, inherits=FALSE ) )
	# print( "============" )
	# flush.console()
	# Sys.sleep( 1 )
# }


### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
