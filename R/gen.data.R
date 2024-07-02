## Changelog:
# MH 2024-07-02 0.0.57, now checking individual for good matrix properties, if not good, a new design is drawn; added tries.max and fac as arguments
# MH 2024-04-04: set up

## Documentation
#' @title Generate data
#' @description Generate data for a time-varying continuous-time model based on an individualized longitudinal design.
#' @param design.env an environment containing objects created by the \code{gen.design()} function
#' @param seed a number used as the seed for \code{set.seed(seed)} or the value "random" for the random generation of a seed
#' @param value.env an environment containing the true values of the matrices A0, Achange, and Q0 for data generation; if \code{NULL}, default values are used
#' @param gen.data a logical value indicating whether to generate data
#' @param tries.max an integer value that specifies the number of attempts to replace persons (via design) with poorly behaved DT matrices
#' @param fac a factor used to determine extreme values of matrix properties, where a value is considered extreme if the property exceeds fac times the standard deviation (SD) over time points
#' @param verbose a logical value indicating whether to print detailed messages and progress updates during the execution of the function
#' @return An environment is returned containing design characteristics, generated time points, and generated data. Use \code{ls(envir=<returned environment>)} to view its contents.

## Function definition
gen.data <- function( design.env, seed="random", value.env=NULL, gen.data=TRUE, tries.max=10000, fac=1.5, verbose=TRUE ){

		# trigger for no between-(co)variances in mu
		between.mu <- FALSE
		# between.mu <- TRUE

		# matrix properties data frame
		mp <- data.frame( "j"=NA, "p"=NA, "matrix"=NA, "symmetric"=NA, "posdef"=NA, "posdef2"=NA, "possemidef"=NA, "kappa"=NA, "minSVD"=NA, "maxSVD"=NA, "eigenvaluespread"=NA, "rank"=NA, "fullrank"=NA )
		mp <- mp[-1,,drop=FALSE]

		### based on tvct_v1.pdf (2024-04-04)

		# require packages
		requireNamespace( "mvtnorm" )
		requireNamespace( "expm" )
	
		# seed
		if( seed %in% "random" ){
			seed <- sample( 1:999999999, 1 )
		}
		set.seed( seed )

		# get empty structures
		str.env <- gen.empty.structures( env=design.env )

		# put all empty structures here
		# names <- ls(envir=str.env)
		# for( i in 1:length( names ) ){
			# eval( parse( text=paste0( names[i], " <- get('", names[i], "', envir=str.env, inherits=FALSE)" ) ) )
		# }
		A0          <- get('A0'         , envir=str.env, inherits=FALSE)
		Achange     <- get('Achange'    , envir=str.env, inherits=FALSE)
		SigmaepsA   <- get('SigmaepsA'  , envir=str.env, inherits=FALSE)
		Q0          <- get('Q0'         , envir=str.env, inherits=FALSE)
		Qchange     <- get('Qchange'    , envir=str.env, inherits=FALSE)
		SigmaepsQ   <- get('SigmaepsQ'  , envir=str.env, inherits=FALSE)
		mu0         <- get('mu0'        , envir=str.env, inherits=FALSE)
		muchange    <- get('muchange'   , envir=str.env, inherits=FALSE)
		Sigmaepsmu  <- get('Sigmaepsmu' , envir=str.env, inherits=FALSE)
		if( between.mu ) Sigmamu     <- get('Sigmamu'    , envir=str.env, inherits=FALSE)
		Delta       <- get('Delta'      , envir=str.env, inherits=FALSE)
		Sigmaeps    <- get('Sigmaeps'   , envir=str.env, inherits=FALSE)
		epsAt       <- get('epsAt'      , envir=str.env, inherits=FALSE)
		At          <- get('At'         , envir=str.env, inherits=FALSE)
		epsQt       <- get('epsQt'      , envir=str.env, inherits=FALSE)
		Qt          <- get('Qt'         , envir=str.env, inherits=FALSE)
		epsmut      <- get('epsmut'     , envir=str.env, inherits=FALSE)
		mut         <- get('mut'        , envir=str.env, inherits=FALSE)
		if( between.mu ) muj         <- get('muj'        , envir=str.env, inherits=FALSE)
		Ajp         <- get('Ajp'        , envir=str.env, inherits=FALSE)
		Qjp         <- get('Qjp'        , envir=str.env, inherits=FALSE)
		mujp        <- get('mujp'       , envir=str.env, inherits=FALSE)
		Ahashjp     <- get('Ahashjp'    , envir=str.env, inherits=FALSE)
		Sigmawjp    <- get('Sigmawjp'   , envir=str.env, inherits=FALSE)
		Astarjp     <- get('Astarjp'    , envir=str.env, inherits=FALSE)
		Qstarjp     <- get('Qstarjp'    , envir=str.env, inherits=FALSE)
		thetajp     <- get('thetajp'    , envir=str.env, inherits=FALSE)
		omegajp     <- get('omegajp'    , envir=str.env, inherits=FALSE)
		epsjp       <- get('epsjp'      , envir=str.env, inherits=FALSE)
		yjp         <- get('yjp'        , envir=str.env, inherits=FALSE)
		zerovecF    <- get('zerovecF'   , envir=str.env, inherits=FALSE)
		zerovecF2   <- get('zerovecF2'  , envir=str.env, inherits=FALSE)
		zerovecFF12 <- get('zerovecFF12', envir=str.env, inherits=FALSE)
		zerovecI    <- get('zerovecI'   , envir=str.env, inherits=FALSE)
		IF          <- get('IF'         , envir=str.env, inherits=FALSE)
		IF2         <- get('IF2'        , envir=str.env, inherits=FALSE)
		S1          <- get('S1'         , envir=str.env, inherits=FALSE)
		S2          <- get('S2'         , envir=str.env, inherits=FALSE)
		S3          <- get('S3'         , envir=str.env, inherits=FALSE)
		        # <- get('yjp'       , envir=str.env, inherits=FALSE)



		# put all elements from design.env (should contain all design elements) here
		# design.names <- ls(envir=design.env)
		# for( i in 1:length( design.names ) ){
			# eval( parse( text=paste0( design.names[i], " <- get('", design.names[i], "', envir=design.env, inherits=FALSE)" ) ) )
		# }
		F          <- get('F'         , envir=design.env, inherits=FALSE)
		I          <- get('I'         , envir=design.env, inherits=FALSE)
		N          <- get('N'         , envir=design.env, inherits=FALSE)
		T          <- get('T'         , envir=design.env, inherits=FALSE)
		Tunique    <- get('Tunique'   , envir=design.env, inherits=FALSE)
		Tj         <- get('Tj'        , envir=design.env, inherits=FALSE)
		tunique    <- get('tunique'   , envir=design.env, inherits=FALSE)
		ptuniquejp <- get('ptuniquejp', envir=design.env, inherits=FALSE)
		deltajp    <- get('deltajp'   , envir=design.env, inherits=FALSE)

		### default values for model parameters ###
		
		# Q0[] <- 0.25
		# MH 0.0.24 2024-05-03 Q0 covariance now 0
		# Q0[] <- 0
		# diag( Q0 ) <- 1
		# MH 0.0.27 2024-05-04 Q0 is Q (not time-varying)
		# Q0[] <- 0.25

		# mu0[] <- 0
		
		# if( defaults %in% 1 ){
			A0[] <- 0.10
			diag( A0 ) <- -0.69

			Achange[] <- 0
			diag( Achange ) <- 0

			# MH 0.0.28 2024-05-04 Q covariance fixed to 0
			Q0[] <- 0
			diag( Q0 ) <- 0.5
			
			# Qchange[] <- 0
			# diag( Qchange ) <- 0
			# muchange[]  <- 0

			if( between.mu ){
				Sigmamu[] <- 0
				diag( Sigmamu ) <- 1
			}

		# }
		
		# get elements of values.env and overwrite defaults
		if( !is.null( value.env ) ){
			if( "A0"      %in% ls(envir=value.env) )      A0      <- get('A0'         , envir=value.env, inherits=FALSE)
			if( "Achange" %in% ls(envir=value.env) )      Achange <- get('Achange'    , envir=value.env, inherits=FALSE)
			if( "Q0"      %in% ls(envir=value.env) )      Q0      <- get('Q0'         , envir=value.env, inherits=FALSE)
		}
		
		if( verbose ){
			cat( paste0( "values used for data generation:\n" ) )
			cat( paste0( "A0:\n" ) )
			print( A0 )
			cat( paste0( "Achange:\n" ) )
			print( Achange )
			cat( paste0( "Q0:\n" ) )
			print( Q0 )
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

		# error covariance matrices
		# SigmaepsA[] <- 0
		# diag( SigmaepsA ) <- 0.0025
		# SigmaepsQ[] <- 0
		# diag( SigmaepsQ ) <- 0.0025
		# MH 0.0.26 2024-05-04 variance of Q-covariance errors now 0
		# SigmaepsQ[2,2] <- 0.0001
		# Sigmaepsmu[] <- 0
		# diag( Sigmaepsmu ) <- 0.0025		

		# Sigmaeps[] <- 0
		# diag( Sigmaeps ) <- 0.10

		## Delta (default)
		# nitems.per.dim <- floor( I/F )
		# Delta[] <- 0
		# for( f in 1:F ){
			# Delta[ ((f-1)*nitems.per.dim+1):(f*nitems.per.dim) ,f] <- 1
		# }
		# if( any( rs <- rowSums( Delta ) == 0 ) ){
			# Delta[which(rs),F] <- 1
		# }


		### data generation ###
		
		# time-varying parameters
		for( p in 1:Tunique ){
			# Eq. 2
			# MH 0.0.30 2024-05-07, no epsAt
			# epsAt[,,p] <- rmvnorm( 1, mean=zerovecF2, sigma=SigmaepsA )
			# At[,,p] <- irow( S1 %*% row( A0 + Achange*tunique[p] + irow(epsAt[,,p,drop=FALSE]) ) ) + irow( S2 %*% row( A0 * exp( -(Achange*tunique[p] + irow(epsAt[,,p,drop=FALSE]) ) ) ) )
			# At[,,p] <- irow( S1 %*% row( A0 + Achange*tunique[p] ) ) + irow( S2 %*% row( A0 * exp( -(Achange*tunique[p] ) ) ) )
			# MH 0.0.46 without exp
			# Ac <- row( A0 + Achange*tunique[p] )
			# At[,,p] <- irow( S1 %*% Ac + S2 %*% Ac )
			At[,,p] <- A0 + Achange*tunique[p]
			# Eq. 3
			# epsQt[,,p] <- rmvnorm( 1, mean=zerovecFF12, sigma=SigmaepsQ )
			# Qt[,,p] <- irow( S1 %*% row( Q0 + Qchange*tunique[p] + irow(S3%*%epsQt[,,p,drop=FALSE]) ) ) + irow( S2 %*% row( Q0 * exp( Qchange*tunique[p] + irow(S3%*%epsQt[,,p,drop=FALSE]) ) ) )
			# MH 0.0.27 2024-05-04 Q0 is Q (not time-varying)
			Qt[,,p] <- Q0
			# Eq. 10
			# epsmut[,1,p] <- rmvnorm( 1, mean=zerovecF, sigma=Sigmaepsmu )
			# Eq. 9
			# mut[,1,p] <- mu0 + muchange*tunique[p] + as.matrix( epsmut[,1,p,drop=FALSE] )
			# 0.0.29 2024-05-06, no mean
			# mut[,1,p] <- mu0 + muchange*tunique[p] + as.matrix( epsmut[,1,p,drop=FALSE] )
		}

		# matrix with small values
		F2F2add <- matrix( 1e-10, F^2, F^2 )
		diag( F2F2add ) <- 0
		
		# individualization
		if( verbose ) cat( paste0( "trying to generate person-specific DT matrices\n" ) )
		for( j in 1:N ){
			
			# MH 2024-07-02 0.0.57, eleminate "bad" persons in data generation
			keep.trying <- TRUE
			try <- 1
			while( keep.trying && try <= tries.max ){
				if( verbose ) {
					if( try==1 ) cat( paste0( "person ", j, " ." ) ) else cat( "." )
					flush.console()
				}
			
				# muj, Eq. 8
				if( between.mu ){ muj[,1,j] <- rmvnorm( 1, mean=zerovecF, sigma=Sigmamu ) } 
				
				for( p in 1:Tj[j] ){
					# Eq. 11
					Ajp[,,j,p] <- At[,,ptuniquejp[j,p]]
					Qjp[,,j,p] <- Qt[,,ptuniquejp[j,p]]
					# 0.0.29 2024-05-06, no mean
					# if( between.mu ){
						# mujp[,1,j,p] <- mut[,1,ptuniquejp[j,p]] + as.matrix( muj[,1,j,drop=FALSE] )
					# } else {
						# mujp[,1,j,p] <- mut[,1,ptuniquejp[j,p]]
					# }
					# Ahash, Eq. 14
					Ahashjp[,,j,p] <- Ajp[,,j,p] %x% IF + IF %x% Ajp[,,j,p]
					
					# if not positive definite add F2F2add
					if( !is_positive_definite2( Ahashjp[,,j,p] ) ){
						Ahashjp[,,j,p] <- Ahashjp[,,j,p] + F2F2add
					}
					
					# Sigmaw, Eq. 14
					Sigmawjp[,,j,p] <- irow( -solve( Ahashjp[,,j,p] ) %*% row(Qjp[,,j,p]) )
	
					### check matrix properties of Sigmawjp[,,j,p]
					ind <- nrow( mp ) + 1
					mp[ind,"j"] <- j
					mp[ind,"p"] <- p
					mp[ind,"matrix"] <- paste0( "Sigmawjp[,,",j,",",p,"]" )
					# symmetric
					mp[ind,"symmetric"] <- isSymmetric( Sigmawjp[,,j,p] )
					# positive definiteness
					mp[ind,"posdef"] <- is_positive_definite( Sigmawjp[,,j,p] )
					mp[ind,"posdef2"] <- is_positive_definite2( Sigmawjp[,,j,p] )
					# positive semi-definiteness
					mp[ind,"possemidef"] <- is_positive_semi_definite( Sigmawjp[,,j,p] )
					# condition number
					mp[ind,"kappa"] <- kappa( Sigmawjp[,,j,p] )
					# minimal/maximal Singular Value Decomposition (SVD)
					mp[ind,"minSVD"] <- min( svd( Sigmawjp[,,j,p] )$d )
					mp[ind,"maxSVD"] <- max( svd( Sigmawjp[,,j,p] )$d )
					# eigenvalues spread
					eigenvalues <- eigen( Sigmawjp[,,j,p] )$values
					if (any(Im(eigenvalues) != 0)) {
						mp[ind,"eigenvaluespread"] <- NA
					} else {
						mp[ind,"eigenvaluespread"] <- max(eigenvalues) - min(eigenvalues)
					}
					# rank
					mp[ind,"rank"] <- qr( Sigmawjp[,,j,p] )$rank
					# full rank
					mp[ind,"fullrank"] <- qr( Sigmawjp[,,j,p] )$rank == F
					
				}
	
				for( p in 1:(Tj[j]-1) ){
					# Astarjp, Eq. 12
					# MH 0.0.44 2024-05-31, as.matrix needed for F=1
					Astarjp[,,j,p] <- expm( as.matrix( Ajp[,,j,p] * deltajp[j,p] ) )
					# Qstarjp, Eq. 13
					Qstarjp[,,j,p] <- irow( -( expm( as.matrix( Ahashjp[,,j,p] * deltajp[j,p] ) ) - IF2 ) %*% row( Sigmawjp[,,j,p] ) )			
	
					### check matrix properties of Qstarjp[,,j,p]
					ind <- nrow( mp ) + 1
					mp[ind,"j"] <- j
					mp[ind,"p"] <- p
					mp[ind,"matrix"] <- paste0( "Qstarjp[,,",j,",",p,"]" )
					# symmetric
					mp[ind,"symmetric"] <- isSymmetric( Qstarjp[,,j,p] )
					# positive definiteness
					mp[ind,"posdef"] <- is_positive_definite( Qstarjp[,,j,p] )
					mp[ind,"posdef2"] <- is_positive_definite2( Qstarjp[,,j,p] )
					# positive semi-definiteness
					mp[ind,"possemidef"] <- is_positive_semi_definite( Qstarjp[,,j,p] )
					# condition number
					mp[ind,"kappa"] <- kappa( Qstarjp[,,j,p] )
					# minimal/maximal Singular Value Decomposition (SVD)
					mp[ind,"minSVD"] <- min( svd( Qstarjp[,,j,p] )$d )
					mp[ind,"maxSVD"] <- max( svd( Qstarjp[,,j,p] )$d )
					# eigenvalues spread
					eigenvalues <- eigen( Qstarjp[,,j,p] )$values
					if (any(Im(eigenvalues) != 0)) {
						mp[ind,"eigenvaluespread"] <- NA
					} else {
						mp[ind,"eigenvaluespread"] <- max(eigenvalues) - min(eigenvalues)
					}
					# rank
					mp[ind,"rank"] <- qr( Qstarjp[,,j,p] )$rank
					# full rank
					mp[ind,"fullrank"] <- qr( Qstarjp[,,j,p] )$rank == F				
					
					make_symmetric <- function(mat) {
						return((mat + t(mat)) / 2)
					}
					if( !isSymmetric( Qstarjp[,,j,p] ) ) {
						# Qstarjp[,,j,p] <- round( Qstarjp[,,j,p], 5 )
						Qstarjp[,,j,p] <- make_symmetric( Qstarjp[,,j,p] )
					}
				}
			
				# check whether matrices are ok or person needs to be replaced
				if( gen.data ){
				    mp.j <- mp[mp$j %in% j,]
					
					# all matrices must be symmetric, pos def, pos semi def anf with full rank
					check5 <- all( all( mp.j$symmetric ), all( mp.j$posdef ), all( mp.j$posdef2 ), all( mp.j$possemidef ), all( mp.j$fullrank ) )
	
					# relative criteria
					mp.j1 <- mp.j[ grepl("Sigmawjp",mp.j$matrix),]
					check6 <- all( mp.j1$kappa <= mean(mp.j1$kappa) + fac*sd(mp.j1$kappa) )
					check7 <- all( mp.j1$minSVD >= mean(mp.j1$minSVD) - fac*sd(mp.j1$minSVD) )
					check8 <- all( mp.j1$maxSVD >= mean(mp.j1$maxSVD) - fac*sd(mp.j1$maxSVD) )
					if( any( is.na( mp.j1$eigenvaluespread ) ) ){
						check9 <- FALSE
					} else {
						check9 <- all( mp.j1$eigenvaluespread <= mean(mp.j1$eigenvaluespread) + fac*sd(mp.j1$eigenvaluespread) )
					}
					mp.j2 <- mp.j[ grepl("Qstarjp",mp.j$matrix),]
					check10 <- all( mp.j2$kappa <= mean(mp.j2$kappa) + fac*sd(mp.j2$kappa) )
					check11 <- all( mp.j2$minSVD >= mean(mp.j2$minSVD) - fac*sd(mp.j2$minSVD) )
					check12 <- all( mp.j2$maxSVD >= mean(mp.j2$maxSVD) - fac*sd(mp.j2$maxSVD) )
					if( any( is.na( mp.j2$eigenvaluespread ) ) ){
						check13 <- FALSE
					} else {				
						check13 <- all( mp.j2$eigenvaluespread <= mean(mp.j2$eigenvaluespread) + fac*sd(mp.j2$eigenvaluespread) )
					}					
					
					if( all( c(check5,check6,check7,check8,check9,check10,check11,check12,check13) ) ) {
						keep.trying <- FALSE
						if( verbose ){
							cat( paste0( "\nsuccess (after ",try," iterations)","\n" ) )
							flush.console()
						}
					} else {

						# generate new design for this person
						deltas.arg <- sort( unique( as.vector( deltajp ) ) )
						first.time.points.arg <- sort( unique( tunique[ ptuniquejp[,1] ] ) )
						# try generate until all time points are existant in the all-person time points vector
						keep.trying2 <- TRUE
						tries.max2 <- 1000
						try2 <- 1
						while( keep.trying2 && try2 <= tries.max2 ){
							Tj.j <- Tj[j]
							env.j <- gen.design( F=F, N=1, T=Tj.j, Tdiv=0, deltas=deltas.arg, first.time.points=first.time.points.arg, verbose=verbose )
							ptuniquejp.j <- get( "ptuniquejp", envir=env.j )
							tunique.j <- get( "tunique", envir=env.j )
							
							if( all( tunique.j %in% tunique ) ){
								keep.trying2 <- FALSE
							}
							try2 <- try2 + 1
						}
						if( keep.trying2 ) stop( paste0( "did not find new person-specific design for person ", j, " after ", tries.max2, " tries." ) )
						
						# merge new design into all-person design
						ptuniquejp[j,] <- NA # NA probably important if Tj differs
						ptuniquejp[j,1:Tj.j] <- which( tunique %in% tunique.j )
						deltajp[j,] <- NA # NA probably important if Tj differs
						deltajp[j,1:(Tj.j-1)] <- diff( tunique.j )
						
					}
					try <- try+1
				} else {
					# if not gen data, then continue in any case
					keep.trying <- FALSE
				}
			
			} # end of while loop
			if( keep.trying & gen.data ) stop( paste0( "did not find person-specific DT matrices for person ", j, " after ", tries.max, " tries." ) )

		} # end of loop over persons

		if( gen.data ){
			for( j in 1:N ){
				# theta, first time point, Eq. 17
				# thetajp[,1,j,1] <- rmvnorm( 1, mean=as.matrix( mujp[,1,j,1,drop=FALSE] ), sigma=Sigmawjp[,,j,1] )
				# 0.0.29 2024-05-06, no mean
				# MH 0.0.44 2024-05-31, as.matrix needed for F=1
				thetajp[,1,j,1] <- rmvnorm( 1, mean=zerovecF, sigma=as.matrix( Sigmawjp[,,j,1] ) )
				for( p in 2:Tj[j] ){
					# omegajp, Eq. 15
					omegajp[,1,j,p] <- rmvnorm( 1, mean=zerovecF, sigma=as.matrix( Qstarjp[,,j,p-1] ) )
					# thetajp, Eq. 16
					# thetajp[,1,j,p] <- Astarjp[,,j,p-1] %*% as.matrix( thetajp[,1,j,p-1,drop=FALSE] ) + ( IF - Astarjp[,,j,p-1] ) %*% as.matrix( mujp[,1,j,p,drop=FALSE] ) + as.matrix( omegajp[,1,j,p,drop=FALSE] )
					# 0.0.29 2024-05-06, no mean
					thetajp[,1,j,p] <- Astarjp[,,j,p-1] %*% as.matrix( thetajp[,1,j,p-1,drop=FALSE] ) + as.matrix( omegajp[,1,j,p,drop=FALSE] )
				}
			}
			
			for( j in 1:N ){
				for( p in 1:Tj[j] ){
					# measurement error, Eq. 19
					# epsjp[,1,j,p] <- rmvnorm( 1, mean=zerovecI, sigma=Sigmaeps )
					# responses, Eq. 18
					# yjp[,1,j,p] <- Delta %*% as.matrix( thetajp[,1,j,p,drop=FALSE] ) + as.matrix( epsjp[,1,j,p,drop=FALSE] )
					# MH 0.0.29 2024-05-06, no measurement model
					yjp[,1,j,p] <- as.matrix( thetajp[,1,j,p,drop=FALSE] )
				}
			}
		}

		# checks
		# if( j==N & all(is.na(yjp[,1,N,])) ) browser()
		
		# add structures that should additionally be returned
		data.names <- ls()
		data.names <- data.names[ !data.names %in% c("f", "j", "p", "rs", "design.env", "str.env" ) ]
		data.env <- new.env()
		for( i in 1:length( data.names ) ){
			assign( data.names[i], eval( parse( text=data.names[i] ) ), envir = data.env, inherits = FALSE, immediate=TRUE )
		}
		return( data.env )
}


### development
# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    # "Dropbox/139_conttime/conttime/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("gen.data.R") ] # ,"estimate.R"
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }

# design.env <- gen.design()
# ls( envir=design.env )

# data.env <- gen.data( design.env=design.env )
# ls( envir=data.env )


# while( TRUE ){
	# design.env <- gen.design(T=20,Tdiv=-20,N=3)
	# data.env <- gen.data(design.env=design.env)
	# print( get( "Tj", envir=data.env, inherits=FALSE ) ); flush.console()
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
