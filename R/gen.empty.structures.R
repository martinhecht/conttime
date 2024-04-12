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
gen.empty.structures <- function( F=2, I=F, N=5, T=3, Tunique=T*10, env=NULL, verbose=FALSE ){
		
		### based on tvct_v1.pdf (2024-04-04)
		
		## F x F structures
		FxF.names <- c( "A0","Achange","Q0","Qchange","Sigmamu","Sigmaepsmu" )
		eval( parse( text = paste0( FxF.names, " <- array( as.numeric(NA), dim=c(F,F) )" ) ) )
		# identity matrix
		IF <- diag( F )
		FxF.names <- c( FxF.names, "IF" )
		
		## I x F structures
		IxF.names <- c( "Lambda" )
		eval( parse( text = paste0( IxF.names, " <- array( as.numeric(NA), dim=c(I,F) )" ) ) )
		
		## I x I structures
		IxI.names <- c( "Sigmaeps" )
		eval( parse( text = paste0( IxI.names, " <- array( as.numeric(NA), dim=c(I,I) )" ) ) )

		## I x 1 structures
		Ix1.names <- c( "zerovecI" )
		eval( parse( text = paste0( Ix1.names, " <- array( as.numeric(0), dim=c(I) )" ) ) )
		
		## F x 1 structures
		Fx1.names1 <- c( "mu0","muchange" )
		eval( parse( text = paste0( Fx1.names1, " <- array( as.numeric(NA), dim=c(F,1) )" ) ) )
		Fx1.names2 <- c( "zerovecF" )
		eval( parse( text = paste0( Fx1.names2, " <- array( as.numeric(0), dim=c(F,1) )" ) ) )
		Fx1.names <- c(Fx1.names1,Fx1.names2)

		## F x 1 x N structures
		Fx1xN.names <- c( "muj" )
		eval( parse( text = paste0( Fx1xN.names, " <- array( as.numeric(NA), dim=c(F,1,N) )" ) ) )
		
		## F x 1 x N x T structures
		Fx1xNxT.names <- c( "thetajp","omegajp","mujp" ) # "epsmujp"
		eval( parse( text = paste0( Fx1xNxT.names, " <- array( as.numeric(NA), dim=c(F,1,N,T) )" ) ) )		

		## F x 1 x Tunique structures
		Fx1xTunique.names <- c( "mut", "epsmut" )
		eval( parse( text = paste0( Fx1xTunique.names, " <- array( as.numeric(NA), dim=c(F,1,Tunique) )" ) ) )

		## I x 1 x N x T structures
		Ix1xNxT.names <- c( "yjp","epsjp" )
		eval( parse( text = paste0( Ix1xNxT.names, " <- array( as.numeric(NA), dim=c(I,1,N,T) )" ) ) )		
		
		## F x F x N x T
		FxFxNxT.names <- c( "Ajp","Astarjp","Qjp","Qstarjp","Sigmawjp" )
		eval( parse( text = paste0( FxFxNxT.names, " <- array( as.numeric(NA), dim=c(F,F,N,T) )" ) ) )

		## F x F x Tunique
		FxFxTunique.names <- c( "At","Qt" )
		eval( parse( text = paste0( FxFxTunique.names, " <- array( as.numeric(NA), dim=c(F,F,Tunique) )" ) ) )

		## F^2 x 1 x N x T
		# F2x1xNxT.names <- c( "epsAjp" )
		# eval( parse( text = paste0( F2x1xNxT.names, " <- array( as.numeric(NA), dim=c(F^2,1,N,T) )" ) ) )
		F2x1xNxT.names <- NULL

		## F^2 x 1 x Tunique
		F2x1xTunique.names <- c( "epsAt" )
		eval( parse( text = paste0( F2x1xTunique.names, " <- array( as.numeric(NA), dim=c(F^2,1,Tunique) )" ) ) )

		## F(F+1)/2 x 1 x N x T
		# FF12x1xNxT.names <- c( "epsQjp" )
		# eval( parse( text = paste0( FF12x1xNxT.names, " <- array( as.numeric(NA), dim=c(F*(F+1)/2,1,N,T) )" ) ) )
		FF12x1xNxT.names <- NULL

		## F(F+1)/2 x 1 x Tunique
		FF12x1xTunique.names <- c( "epsQt" )
		eval( parse( text = paste0( FF12x1xTunique.names, " <- array( as.numeric(NA), dim=c(F*(F+1)/2,1,Tunique) )" ) ) )
		
		## F^2 x F^2 x N x T
		F2xF2xNxT.names <- c( "Ahashjp" )
		eval( parse( text = paste0( F2xF2xNxT.names, " <- array( as.numeric(NA), dim=c(F^2,F^2,N,T) )" ) ) )
		
		## N x T structures
		NxT.names <- c( "tjp" )
		eval( parse( text = paste0( NxT.names, " <- array( as.numeric(NA), dim=c(N,T) )" ) ) )

		## N x (T-1) structures
		NxTmin1.names <- c( "deltajp" )
		eval( parse( text = paste0( NxTmin1.names, " <- array( as.numeric(NA), dim=c(N,T-1) )" ) ) )
		
		## F^2 x F^2 structures
		F2xF2.names <- c( "SigmaepsA" )
		eval( parse( text = paste0( F2xF2.names, " <- array( as.numeric(NA), dim=c(F^2,F^2) )" ) ) )
		# Selection matrices
		S2 <- array( 0, dim=c(F^2,F^2) )
		f2 <- function( n ) n*(F+1)-F
		for( n in 1:F ){
			S2[f2(n),f2(n)] <- 1
		}
		S1 <- array( 0, dim=c(F^2,F^2) )
		diag( S1 ) <- -(diag(S2) - 1)
		# identity matrix
		IF2 <- diag( F^2 )
		F2xF2.names <- c( F2xF2.names, "S1","S2","IF2" )

		## F^2 x 1 structures
		F2x1.names <- c( "zerovecF2" )
		eval( parse( text = paste0( F2x1.names, " <- array( as.numeric(0), dim=c(F^2,1) )" ) ) )
		
		## F(F+1)/2 x F(F+1)/2 structures
		FF12xFF12.names <- c( "SigmaepsQ" )
		eval( parse( text = paste0( FF12xFF12.names, " <- array( as.numeric(NA), dim=c(F*(F+1)/2,F*(F+1)/2) )" ) ) )
		
		## F(F+1)/2 x 1 structures
		FF12x1.names <- c( "zerovecFF12" )
		eval( parse( text = paste0( FF12x1.names, " <- array( as.numeric(0), dim=c(F*(F+1)/2,1) )" ) ) )
		
		## N structures
		N.names <- c( "Tj" )
		eval( parse( text = paste0( N.names, " <- array( as.numeric(NA), dim=c(N) )" ) ) )
		
		## I x F structures
		IxF.names <- c( "Delta" )
		eval( parse( text = paste0( IxF.names, " <- array( as.numeric(NA), dim=c(I,F) )" ) ) )
		
		## special structures
		S3 <- NULL
		# S3 for F=2
		if( F==2 ) S3 <- matrix( c(1,0,0,0,  0,1,1,0,  0,0,0,1), nrow=F^2, ncol=F*(F+1)/2 )
		special.names <- "S3"
		
		# environment
		if( is.null( env ) ){
			env <- new.env()
		}
		str.names <- c( FxF.names, IxF.names, IxI.names, Ix1.names, Fx1.names, Fx1xN.names, Fx1xNxT.names, Fx1xTunique.names, Ix1xNxT.names, FxFxNxT.names, FxFxTunique.names, F2x1xNxT.names, F2x1xTunique.names, FF12x1xNxT.names, FF12x1xTunique.names, F2xF2xNxT.names, NxT.names, NxTmin1.names, F2xF2.names, F2x1.names, FF12xFF12.names, FF12x1.names, N.names, IxF.names, special.names, "F", "I", "N", "T", "Tunique", "str.names" )
		for( i in 1:length( str.names ) ){
			assign( str.names[i], eval( parse( text=str.names[i] ) ), envir = env, inherits = FALSE, immediate=TRUE )
		}
		
		return( env )
}


### development
# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    # "Dropbox/139_conttime/conttime/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("gen.empty.structures.R") ]
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }

# print( m <- gen.empty.structures() )
# ls( envir=m )
# get( "A0", envir=m, inherits=FALSE )
# get( "Achange", envir=m, inherits=FALSE )
# get( "Tj", envir=m, inherits=FALSE )
# get( "deltajp", envir=m, inherits=FALSE )



### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
