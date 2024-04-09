## Changelog:
# MH 2024-04-04: set up

## Documentation
#' @title
#' @description
#' @param
#' @param
#' @param
#' @return

## Function definition
gen.data <- function( defaults=c(1), env=NULL, seed="random", verbose=FALSE ){
		
		### based on tvct_v1.pdf (2024-04-04)

		# require packages
		require( mvtnorm )
		require( expm )
	
		# seed
		if( seed %in% "random" ){
			seed <- sample( 1:999999999, 1 )
		}
		set.seed( seed )
		
		# get empty structures
		if( is.null( env ) ) env <- gen.empty.structures()

		# put all elements from environment here
		names <- ls(envir=env)
		for( i in 1:length( names ) ){
			eval( parse( text=paste0( names[i], " <- get('", names[i], "', envir=env)" ) ) )
		}

		## gen design characteristics
		# Tj
		Tj <- as.integer( sample( as.character(min(2,T):T), N, replace = TRUE ) )
		# deltajp
		for( j in 1:N ){
			for( p in 1:(Tj[j]-1) ){
				deltajp[j,p] <- sample( c(0.5,1,1.5), 1 )
			}
		}
		# tjp
		for( j in 1:N ){
			# first time point
			tjp[j,1] <- sample( c(-1,-0.5,0,0.5,1), 1 )
			for( p in 2:Tj[j] ){
				tjp[j,p] <- tjp[j,p-1] + deltajp[j,p-1]
			}
		}
		
		A0[] <- 0.1
		diag( A0 ) <- -0.75
		Q0[] <- 0.25
		diag( Q0 ) <- 1
		mu0[] <- 0
		
		if( defaults %in% 1 ){
			Achange[] <- 0
			diag( Achange ) <- 0
			Qchange[] <- 0
			diag( Qchange ) <- 0
			muchange[]  <- 0
		}
		
		# Achange[1,1] <- c(0 , 0.020 , 0.030 , 0.040 )
		# Achange[2,2] <- c(0 , 0.020 , 0.030 , 0.040 )
		# Achange[1,2] <- c(0 , 0.010 , 0.015 , 0.020 )
		# Achange[2,1] <- c(0 , 0.010 , 0.015 , 0.020 )
		# Achange <- -Achange*0.9

		# Qchange[1,1] <- c(0 , 0.025 , 0.050 , 0.075 )
		# Qchange[2,2] <- c(0 , 0.025 , 0.050 , 0.075 )
		# Qchange[1,2] <- c(0 , 0.010 , 0.020 , 0.030 )
		# Qchange[2,1] <- c(0 , 0.010 , 0.020 , 0.030 )
		# Qchange <- -Qchange

		# muchange[1] <- c(0 , 0.1 , 0.2 , 0.3 )
		# muchange[2] <- c(0 , 0.1 , 0.2 , 0.3 )
		# muchange <- muchange*0.5

		Sigmamu[] <- 0
		diag( Sigmamu ) <- 1

		# error covariance matrices
		SigmaepsA[] <- 0
		diag( SigmaepsA ) <- 0.0025
		SigmaepsQ[] <- 0
		diag( SigmaepsQ ) <- 0.0025
		Sigmaepsmu[] <- 0
		diag( Sigmaepsmu ) <- 0.0025

		# muj, Eq. 8
		for( j in 1:N ){
			muj[,1,j] <- rmvnorm( 1, mean=zerovecF, sigma=Sigmamu )
		}
		
		for( j in 1:N ){
			for( p in 1:Tj[j] ){
				# Eq. 2
				epsAjp[,,j,p] <- rmvnorm( 1, mean=zerovecF2, sigma=SigmaepsA )
				Ajp[,,j,p] <- irow( S1 %*% row( A0 + Achange*tjp[j,p] + irow(epsAjp[,,j,p,drop=FALSE]) ) ) + irow( S2 %*% row( A0 * exp( -(Achange*tjp[j,p] + irow(epsAjp[,,j,p,drop=FALSE]) ) ) ) )
				# Eq. 3
				epsQjp[,,j,p] <- rmvnorm( 1, mean=zerovecFF12, sigma=SigmaepsQ )
				Qjp[,,j,p] <- irow( S1 %*% row( Q0 + Qchange*tjp[j,p] + irow(S3%*%epsQjp[,,j,p,drop=FALSE]) ) ) + irow( S2 %*% row( Q0 * exp( Qchange*tjp[j,p] + irow(S3%*%epsQjp[,,j,p,drop=FALSE]) ) ) )
				# Eq. 10
				epsmujp[,1,j,p] <- rmvnorm( 1, mean=zerovecF, sigma=Sigmaepsmu )
				# Eq. 9
				mujp[,1,j,p] <- mu0 + muchange*tjp[j,p] + as.matrix( epsmujp[,1,j,p,drop=FALSE] ) + as.matrix( muj[,1,j,drop=FALSE] )
				# Ahash, Eq. 14
				Ahashjp[,,j,p] <- Ajp[,,j,p] %x% IF + IF %x% Ajp[,,j,p]
				# Sigmaw, Eq. 14
				Sigmawjp[,,j,p] <- irow( -solve( Ahashjp[,,j,p] ) %*% row(Qjp[,,j,p]) )
			}
		}

		for( j in 1:N ){
			for( p in 1:(Tj[j]-1) ){
				# Astarjp, Eq. 12
				Astarjp[,,j,p] <- expm( Ajp[,,j,p] * deltajp[j,p] )
				# Qstarjp, Eq. 13
				Qstarjp[,,j,p] <- irow( -( expm( Ahashjp[,,j,p] * deltajp[j,p] ) - IF2 ) %*% row( Sigmawjp[,,j,p] ) )			
			}
		}

		for( j in 1:N ){
			# theta, first time point, Eq. 17
			thetajp[,1,j,1] <- rmvnorm( 1, mean=as.matrix( mujp[,1,j,p,drop=FALSE] ), sigma=Sigmawjp[,,j,1] )
			for( p in 2:Tj[j] ){
				# omegajp, Eq. 15
				omegajp[,1,j,p] <- rmvnorm( 1, mean=zerovecF, sigma=Qstarjp[,,j,p-1] )
				# thetajp, Eq. 16
				thetajp[,1,j,p] <- Astarjp[,,j,p-1] %*% as.matrix( thetajp[,1,j,p-1,drop=FALSE] ) + ( IF - Astarjp[,,j,p-1] ) %*% as.matrix( mujp[,1,j,p,drop=FALSE] ) + as.matrix( omegajp[,1,j,p,drop=FALSE] )
			}
		}
	
		## Delta (default)
		nitems.per.dim <- floor( I/F )
		Delta[] <- 0
		for( f in 1:F ){
			Delta[ ((f-1)*nitems.per.dim+1):(f*nitems.per.dim) ,f] <- 1
		}
		if( any( rs <- rowSums( Delta ) == 0 ) ){
			Delta[which(rs),F] <- 1
		}

		Sigmaeps[] <- 0
		diag( Sigmaeps ) <- 0.10
		
		for( j in 1:N ){
			for( p in 1:Tj[j] ){
				# measurement error, Eq. 19
				epsjp[,1,j,p] <- rmvnorm( 1, mean=zerovecI, sigma=Sigmaeps )
				# responses, Eq. 18
				yjp[,1,j,p] <- Delta %*% as.matrix( thetajp[,1,j,p,drop=FALSE] ) + as.matrix( epsjp[,1,j,p,drop=FALSE] )
			}
		}
		
		env2 <- new.env()
		for( i in 1:length( str.names ) ){
			assign( str.names[i], eval( parse( text=str.names[i] ) ), envir = env2, inherits = FALSE, immediate=TRUE )
		}
		return( env2 )
}


### development
# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    # "Dropbox/139_conttime/conttime/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("gen.data.R") ]
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }

# m <- gen.data()
# ls( envir=m )

# while( TRUE ){
	# e <- gen.empty.structures()
	# e <- gen.empty.structures(T=5,N=8,F=2,I=11)
	# m <- gen.data(env=e)
	# print( get( "Tj", envir=m, inherits=FALSE ) ); flush.console()
# }

# get( "Tj", envir=m, inherits=FALSE )
# get( "yjp", envir=m, inherits=FALSE )
# get( "Delta", envir=m, inherits=FALSE )
# get( "Sigmaeps", envir=m, inherits=FALSE )
# get( "A0", envir=m, inherits=FALSE )
# get( "Achange", envir=m, inherits=FALSE )
# get( "Tj", envir=m, inherits=FALSE )
# get( "deltajp", envir=m, inherits=FALSE )



### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
