
set.priors <- function ( env ) {
		
		# get variables from env
		eval( parse( text=paste0( "assign( '",ls(envir=env), "' , get('",ls(envir=env),"', envir=env ) )" ) ) )

		# console output
		if ( verbose ) cat("setting priors\n\n")		

		# list of priors
		prior <- list()

		# A
		if ( verbose ) cat( "                                 drift matrix A: " )
		if ( any( is.parameter ( A ) ) ) {
				invisible( make.priors( m.name="A", m=A, priors=priors, env=env, diag.prior = "dnorm(-0.5,0.1)", offdiag.prior = "dnorm(0,0.1)", verbose=verbose ) )
		} else {
				if( verbose ) cat( paste0( "n/a (all ", prod(dim(A)), " values fixed)\n" ) )
		}
# browser()
		## process error matrix Q
		# in jags/ctsem Q
		# in ctstan cholQ
		if ( engine %in% c("jags") ) {		
				# Q
				if ( verbose ) cat( "                             diffusion matrix Q: " )
				if ( any( is.parameter ( Q ) ) ) {
						invisible( make.priors( m.name="Q", m=Q, priors=priors, env=env, diag.prior = "dgamma(1,1)", offdiag.prior = "dnorm(0,0.1)", verbose=verbose ) )
				} else {
						if( verbose ) cat( paste0( "n/a (all ", prod(dim(Q)), " values fixed)\n" ) )
				}		
		}
		if ( engine %in% c("ctstan","ctsem") ) {		
				# cholQ
				if ( verbose ) cat( "                         diffusion matrix cholQ: " )
				if ( any( is.parameter ( cholQ ) ) ) {
						invisible( make.priors( m.name="cholQ", m=cholQ, priors=priors, env=env, diag.prior = "dgamma(1,1)", offdiag.prior = "dnorm(0,0.1)", verbose=verbose ) )
				} else {
						if( verbose ) cat( paste0( "n/a (all ", prod(dim(cholQ)), " values fixed)\n" ) )
				}		
		}		
		
		# b
		if ( verbose ) cat( "                   continuous time intercepts b: " )
		if ( any ( is.parameter( b ) ) ) {
				invisible( make.priors( m.name="b", m=b, priors=priors, env=env, prior = "dnorm(0,0.1)", verbose=verbose ) )
		} else {
				if( verbose ) cat( paste0( "n/a (all ", prod(dim(b)), " values fixed)\n" ) )
		}		
		# person cont. intercepts bj
		if ( exists("bj") && any ( is.parameter ( bj ) ) ) {
				if ( verbose ) cat( "                person cont. time intercepts bj: " )
         		if ( F>1 )  invisible( make.priors( m.name="bj", m=bj, priors=priors, env=env, prior = "dmnorm( b[,1], prec.b[,] )", diag.prior = "dmnorm( b[,1], prec.b[,] )", offdiag.prior = "dmnorm( b[,1], prec.b[,] )", verbose=verbose ) )
         		if ( F==1 ) invisible( make.priors( m.name="bj", m=bj, priors=priors, env=env, prior = "dnorm( b[1,1], prec.b[1,1] )", diag.prior = "dnorm( b[1,1], prec.b[1,1] )", offdiag.prior = "dnorm( b[1,1], prec.b[1,1] )", verbose=verbose ) )
		}		
		
		# Lambda
		if ( verbose ) cat( "                          loading matrix Lambda: " )
		if ( any( is.parameter ( Lambda ) ) ) {
				invisible( make.priors( m.name="Lambda", m=Lambda, priors=priors, env=env, prior = "dnorm( 1, 0.0001 )", verbose=verbose ) )
		} else {
				if( verbose ) cat( paste0( "n/a (all ", prod(dim(Lambda)), " values fixed)\n" ) )
		}	
# browser()		
		# beta
		if ( verbose ) cat( "                             item easiness beta: " )
		if ( any( is.parameter ( beta ) ) ) {
				
				if ( all ( is.parameter ( beta ) ) ) {
						beta.pr <- "dnorm( mu.beta, prec.beta )"
				} else {
						beta.pr <- "dnorm( 1, 0.0001 )"
				}
				invisible( make.priors( m.name="beta", m=beta, priors=priors, env=env, prior = beta.pr, verbose=verbose ) )
		} else {
				if( verbose ) cat( paste0( "n/a (all ", prod(dim(beta)), " values fixed)\n" ) )
		}		

# browser()
		# E
		if ( exists("E") && any ( is.parameter ( E ) ) ) {
				if ( verbose ) cat( "                     measurement error matrix E: " )
				invisible( make.priors( m.name="E", m=E, priors=priors, env=env, diag.prior = "dgamma(1,1)", offdiag.prior = "dnorm(0,0.1)", verbose=verbose ) )
		}

		# prec.beta prior
		if ( exists("prec.beta") && any ( is.parameter ( prec.beta ) ) ) {
				# eval( parse ( text=paste0( "assign( 'prec.beta' , 'prec.beta' , envir=env )" ) ) )
				# prec.beta <- get( "prec.beta", envir=env )
				if ( verbose ) cat( "           precision of item easiness prec.beta: " )
				invisible( make.priors( m.name="prec.beta", m=prec.beta, priors=priors, env=env, diag.prior = "dgamma(1,1)", verbose=verbose ) )
		}

		# mu.t1 prior
		if ( exists("mu.t1") && any ( is.parameter ( mu.t1 ) ) ) {
				# eval( parse ( text=paste0( "assign( 'mu.t1' , 'mu.t1' , envir=env )" ) ) )
				# mu.t1 <- get( "mu.t1", envir=env )
				if ( verbose ) cat( "          mean vector of first time point mu.t1: " )
# browser()				
				if( measurement.model$family %in% c("binomial") ) pr <- "dnorm(0,1)" else pr <- "dnorm(0,0.1)"
				invisible( make.priors( m.name="mu.t1", m=mu.t1, priors=priors, env=env, prior = pr , verbose=verbose ) )
				rm( pr )
		}
# browser()	
		# person mu.t1 prior
		if ( exists("mu.t1.j") && any ( is.parameter ( mu.t1.j ) ) ) {
				if ( verbose ) cat( "           person cont. time intercepts mu.t1.j: " )
         		if ( F>1 )  invisible( make.priors( m.name="mu.t1.j", m=mu.t1.j, priors=priors, env=env, prior = "dmnorm( mu.t1[], prec.mu.t1.j[,] )", diag.prior = "dmnorm( mu.t1[], prec.mu.t1.j[,] )", offdiag.prior = "dmnorm( mu.t1[], prec.mu.t1.j[,] )", verbose=verbose ) )
         		if ( F==1 ) invisible( make.priors( m.name="mu.t1.j", m=mu.t1.j, priors=priors, env=env, prior = "dnorm( mu.t1[1], prec.mu.t1.j[1,1] )", diag.prior = "dnorm( mu.t1[1], prec.mu.t1.j[1,1] )", offdiag.prior = "dnorm( mu.t1[1], prec.mu.t1.j[1,1] )", verbose=verbose ) )
		}		
		# precision of mu.t1.j
		if ( exists("prec.mu.t1.j") && any( is.parameter ( prec.mu.t1.j ) ) ) {
				if ( verbose ) cat( "      precision matrix of mu.t1.j, prec.mu.t1.j: " )
				if ( nrow( prec.t1 ) > 1 ) {
						if ( verbose ) cat( "Wishart distribution\n" )
						assign( "prec.mu.t1.j.prior" , "dwish( I1 , F+1 )", envir=env )
				} else if ( nrow( prec.t1 ) == 1 ) {
						invisible( make.priors( m.name="prec.mu.t1.j", m=prec.mu.t1.j, priors=priors, env=env, diag.prior = "dgamma(1,1)", offdiag.prior = "dnorm(0,0.1)", verbose=verbose ) )
				}
		}
		
		
		
		## first time point prec/var
		# in jags prec.t1
		# in ctstan chol.var.t1
		if ( exists("prec.t1") && any ( is.parameter ( prec.t1 ) ) ) {
				# eval( parse ( text=paste0( "assign( 'prec.t1' , 'prec.t1' , envir=env )" ) ) )
				# prec.t1 <- get( "prec.t1", envir=env )
# browser()		
				if ( verbose ) cat( "   precision matrix of first time point prec.t1: " )
				if ( nrow( prec.t1 ) > 1 ) {
						if ( verbose ) cat( "Wishart distribution\n" )
						assign( "prec.t1.prior" , "dwish( I1 , F+1 )", envir=env )
				} else if ( nrow( prec.t1 ) == 1 ) {
						invisible( make.priors( m.name="prec.t1", m=prec.t1, priors=priors, env=env, diag.prior = "dgamma(1,1)", offdiag.prior = "dnorm(0,0.1)", verbose=verbose ) )
				}
		}
		if ( exists("chol.var.t1") && any ( is.parameter ( chol.var.t1 ) ) ) {
				# eval( parse ( text=paste0( "assign( 'chol.var.t1' , 'chol.var.t1' , envir=env )" ) ) )
				# chol.var.t1 <- get( "chol.var.t1", envir=env )
				if ( verbose ) cat( "  chol. var. matr. of first time p. chol.var.t1: Wishart distribution\n" )
				# prior[[length(prior)+1]] <- make.priors( m.name="chol.var.t1", m=chol.var.t1, priors=priors, env=env, prior = "dgamma( 1, 1 )", verbose=verbose )
				assign( "chol.var.t1.prior" , "dwish( I1 , F+1 )", envir=env )		
		}

		## prec/var of b (TRAITVAR)
		# in jags prec.b
		# in ctstan/ctsem chol.var.b
		if ( exists("prec.b") && any ( is.parameter ( prec.b ) ) ) {
				# eval( parse ( text=paste0( "assign( 'prec.b' , 'prec.b' , envir=env )" ) ) )
				# prec.b <- get( "prec.b", envir=env )
# browser()		
				if ( verbose ) cat( "    precision matrix of cont. intercepts prec.b: " )
				if ( nrow( prec.b ) > 1 ) {
						if ( verbose ) cat( "Wishart distribution\n" )
						assign( "prec.b.prior" , "dwish( I1 , F+1 )", envir=env )
				} else if ( nrow( prec.b ) == 1 ) {
						invisible( make.priors( m.name="prec.b", m=prec.b, priors=priors, env=env, diag.prior = "dgamma(1,1)", offdiag.prior = "dnorm(0,0.1)", verbose=verbose ) )
				}
		}
		if ( exists("chol.var.b") && any ( is.parameter ( chol.var.b ) ) ) {
				# eval( parse ( text=paste0( "assign( 'chol.var.b' , 'chol.var.b' , envir=env )" ) ) )
				# chol.var.b <- get( "chol.var.b", envir=env )
				if ( verbose ) cat( "      chol. var. matr. of cont. int. chol.var.b: Wishart distribution\n" )
				# prior[[length(prior)+1]] <- make.priors( m.name="chol.var.b", m=chol.var.b, priors=priors, env=env, prior = "dgamma( 1, 1 )", verbose=verbose )
				assign( "chol.var.b.prior" , "dwish( I1 , F+1 )", envir=env )		
		}		
		
		
		# at the end line break on console
		if ( verbose ) cat( paste0( "\n" ) )
		
		### TODO priors for E
		
		
		### (over)write relevant variables to environment ###
		obj <- c( "prior" )
		eval( parse ( text=paste0( "assign( '",obj, "' , get('",obj,"') , envir=env )" ) ) )

		# return
		TRUE
}

make.priors <- function( m.name, m, env, priors=NULL, mode=c("prior","startingvalue"), diag.prior = "dnorm(0,0.001)", offdiag.prior = "dnorm(0,0.001)", prior = "dnorm(0,0.001)", lower=TRUE, upper=TRUE, verbose=TRUE ){
# browser()
		if ( length( mode ) > 1 ) mode <- mode[1]
		
# browser()	
# browser()
		# free parameters
		free <- default <- is.parameter ( m ) & !is.par.plus.redefined ( m )
		
		
		# user set prior
		if( exists( paste0( m.name, '.', mode ), envir=env ) ) {
				user.prior <- get( paste0( m.name, '.', mode ), envir=env )
				user <- !is.na( user.prior )
		} else {
				user <- default
				user[] <- FALSE
		}

# browser()
		# duplicated
		dupl <- is.duplicated( m )				
		
		# aus user die duplicates raus
		user[ dupl ] <- FALSE
		# TODO: Warnmeldung wenn user ein duplicated prior beschrieben hat
		# wird überschrieben

		# aus default user und dupl raus
		default[ user | dupl ] <- FALSE
		
		# priors
		p <- m
		p[] <- as.character(NA)
		
		# user prior setzen
		if( any( user ) ) p[ user ] <- user.prior[ user ]
		
		# default prior setzen
		if ( all( dim(m) == dim(m)[1] ) ) {
				
				# TODO symmetric abfangen
				
# browser()					
				# diagonal
				p[ default & (!lower.tri(p) & !upper.tri(p)) ] <- diag.prior
				
				# off-diagonal
				p[ default & !(!lower.tri(p) & !upper.tri(p)) ] <- offdiag.prior
				
		} else {

				## non-quadratic
				p[ default ] <- prior
		}		

		# if mode is starting value, transform to numeric
		if ( mode %in% "startingvalue" ){
				dim.p <- dim(p)
				p <- suppressWarnings( as.numeric( p ) )
				dim(p) <- dim.p
		}
		
		# write to environment
		assign( paste0( m.name, '.', mode ), p, envir=env )
	
		# console output
		Ndef <- length(which(default))
		Nuser <- length(which(user))
		# if ( verbose ) cat( paste0( Ndef, " default, ", Nuser, " user (",fixed, " fixed value, ",Nna," NA)\n" ) )
		if ( verbose ) cat( paste0( Ndef, " default, ", Nuser, " user \n" ) )
		
		return( TRUE )
}
				
make.priors.old <- function( m.name, m, env, priors=NULL, mode=c("prior","startingvalue"), diag.prior = "dnorm(0,0.001)", offdiag.prior = "dnorm(0,0.001)", prior = "dnorm(0,0.001)", lower=TRUE, upper=TRUE, verbose=TRUE ){
		
		if ( length( mode ) > 1 ) mode <- mode[1]
		
		if( !exists( paste0( m.name, '.', mode ), envir=env ) ) {			
				# free parameter to NA
				m2 <- is.parameter( m )
				
				m2[ m2 ] <- NA
				if ( !is.null( dim(m) ) ) {
						# m2 <- suppressWarnings( array( as.character( as.numeric( m ) ), dim=dim(m) ) )
						m.is.vector <- FALSE
				} else {
						# if no matrix, i.e., vector
						# make column vector here
						m2 <- matrix( m2 , ncol=1 )
						# later transform back to vector
						m.is.vector <- TRUE
				}
				m2[is.na(m2)] <- "<setprior>"
				m2[is.na(m)] <- NA
# browser()				
				m2.fixed <- !( is.na(m2) | m2 %in% "<setprior>" )
				
				## all duplicate free parameters (e.g. for symmetric matrix) to NA
				# if structure without dimensions (e.g. vector)
				dim.m <- dim( m )
				if( is.null( dim.m ) ) {
						
						# long structure
						m. <- data.frame( matrix( seq( along= eval( parse( text=paste0( "", m.name, "" ) ), envir=env ) ), ncol=1 ) )
						
				} else {
				# if structure with dimensions (e.g. matrix)
				
						# long structure
						m. <- eval(parse(text=paste0( "Reduce(function(x, y) merge(x, y, by=NULL, all=TRUE),list(", paste( paste0( "1:", dim.m ), collapse="," ), "),accumulate=FALSE )" )))

				}				
				m.$par <- apply( m., 1, function ( z ) eval( parse( text= paste0( "m[", paste(z,collapse=","), "]" ) ) ) )
				# duplicated free parameters (not fixed values)
				m.2 <- m.[ duplicated(m.$par) & is.na(suppressWarnings(as.numeric(m.$par))), ]
# browser()						
				if ( nrow( m.2 ) > 0 ) {
						do <- apply( m.2[,-ncol(m.2),drop=FALSE], 1, function ( z ) paste0( "m[", paste(z,collapse=","), "] <- NA; m2[", paste(z,collapse=","), "] <- NA; m2.fixed[", paste(z,collapse=","), "] <- NA" ) )
						for ( do. in do ) eval( parse( text=do. ) )
				}
				
				
				# number of free parameters (for which priors need to be set)
				free <- length( m2[m2 %in% "<setprior>"] )
				# number NA-parameter
				na <- length( m2[is.na( m2 )] )
				# number of fixed parameters
				fixed <- do.call( "*", as.list( dim( m2 ) ) ) - free - na
				
				## user defined priors
				if ( !is.null( priors ) ) {
						
						# names of priors list
						pn <- names( priors )
						
						# which matrix?
						matr <- sub( "^([^0-9&^,&^_&^\\[&^\\(]+).*$", "\\1", pn )
					
						if ( any( matr %in% m.name ) ) {
								
								# reduce to relevant user priors
								priors <- priors[ matr %in% m.name ]
								pn <- pn[ matr %in% m.name ]
								
								# get numbers out of string
								x <- gsub( "[^0-9&^,&^_]", "", sub( m.name, "", pn ) )
								
								# _ zu Komma
								x <- sapply( x, function(x) gsub( "_+", ",", x ) )
								
								# if nchar equals number of dims split
								# e.g. 21 -> 2,1
								x <- sapply( x, function ( x ) if ( nchar( x ) == length( dim(m) ) ) paste( unlist( strsplit( x, "" ) ), collapse="," ) else x )
								
								# set user priors
								do <- paste0( "m2[", x, "] <- priors[[", seq(along=x), "]]" )
								eval( parse( text=do ) )
										
						}
				}
				# number user defined parameters
				udef <- free - length( m2[m2 %in% "<setprior>"] )
# browser()
				## defaults		
				# if quadratic matrix
				if ( all( dim(m2) == dim(m2)[1] ) ) {
						
						# TODO symmetric abfangen
						
# browser()					
						
						# set diagonal priors (matrix or array)
						for ( i in 1:dim(m2)[1] ) {
								do2 <- paste0( "if( m2[",paste( rep( i, length(dim(m2)) ), collapse="," ),"] %in% '<setprior>' ) m2[",paste( rep( i, length(dim(m2)) ), collapse="," ),"] <- diag.prior " )
								eval( parse( text = do2 ) )
						}

						# lower triangle (of matrix)
						if (lower) m2[lower.tri(m2) & m2 %in% "<setprior>"] <- offdiag.prior

						# upper triangle (of matrix)
						if (upper) m2[upper.tri(m2) & m2 %in% "<setprior>"] <- offdiag.prior
						
				} else {

						## non-quadratic
						m2[m2 %in% "<setprior>"] <- prior
				}
				
				# fixed parameter auf NA setzen
				# e.g. 1 -> NA
				m2[m2.fixed] <- NA
				
				# rownames von Matrix fuer prior Matrix
				rownames( m2 ) <- rownames( m )
				colnames( m2 ) <- colnames( m )
# browser()				
				# number defaulted parameters
				# def <- free - udef - length( m2[is.na( m2 )] )
				def <- free - udef
				
				# transform back to vector
				if ( m.is.vector ) {
						m2 <- m2[,1]
				}
				
				# if mode is starting value, transform to numeric
				if ( mode %in% "startingvalue" ){
						m2b <- suppressWarnings( as.numeric( m2 ) )
						dim(m2b) <- dim(m2)
						m2 <- m2b
				}
				
				# write to environment
				assign( paste0( m.name, '.', mode ), m2, envir=env )
				
				# console output
				if ( verbose ) cat( paste0( def, " default, ", udef, " user (",fixed, " fixed value, ",na," NA)\n" ) )
				
		} else {
				# console output
				if ( verbose ) cat( paste0( m.name, ".prior set by user", "\n" ) )
		}
	
		return( TRUE )
}

# any.free <- function( m ) {
		# any( is.na( suppressWarnings( as.numeric( m ) ) ) )
# }
# all.free <- function( m ) {
		# m.vec <- do.call( "c", sapply( m, function(x) x, simplify=FALSE ) )
		# all( is.na( suppressWarnings( as.numeric( m.vec ) ) ) )
# }
# all.fixed <- function( m ) {
		# m.vec <- do.call( "c", sapply( m, function(x) x, simplify=FALSE ) )
		# all( !is.na( suppressWarnings( as.numeric( m.vec ) ) ) )
# }

is.duplicated <- function( m ){

		## all duplicate free parameters (e.g. for symmetric matrix) to NA
		# if structure without dimensions (e.g. vector)
		dim.m <- dim( m )
		if( is.null( dim.m ) ) {
				
				# long structure
				# m. <- data.frame( matrix( seq( along= eval( parse( text=paste0( "", m.name, "" ) ), envir=env ) ), ncol=1 ) )
				m. <- data.frame( matrix( 1:length(m), ncol=1 ) )
				
		} else {
		# if structure with dimensions (e.g. matrix)
		
				# long structure
				m. <- eval(parse(text=paste0( "Reduce(function(x, y) merge(x, y, by=NULL, all=TRUE),list(", paste( paste0( "1:", dim.m ), collapse="," ), "),accumulate=FALSE )" )))

		}				
		# apply( m., 1, function ( z ) paste0( "m[", paste(z,collapse=","), "]" ) )			
# browser()		
		m.$par <- apply( m., 1, function ( z ) eval( parse( text= paste0( "m[", paste(z,collapse=","), "]" ) ) ) )				
		# duplicated 
		m.2 <- m.[ duplicated(m.$par), ]

		
		m2 <- rep( FALSE, prod( dim.m ) )
		dim( m2 ) <- dim.m
		if ( nrow( m.2 ) > 0 ) {
				do <- apply( m.2[,-ncol(m.2),drop=FALSE], 1, function ( z ) paste0( "m2[", paste(z,collapse=","), "] <- TRUE" ) )
				for ( do. in do ) eval( parse( text=do. ) )
		}				
		return(m2)
}

