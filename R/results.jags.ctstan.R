
results.jags.ctstan <- function ( env, mode ) {
# browser()		
# TODO: verbose

		# requireNamespace packages
		requireNamespace( "ggplot2" )
		requireNamespace( "shinystan" )
		requireNamespace( "coda" )
		requireNamespace( "modeest" ) # mlv()
		
		# get variables from env
		eval( parse( text=paste0( "assign( '",ls(envir=env), "' , get('",ls(envir=env),"', envir=env ) )" ) ) )
# browser()		

		# iterations
		iter <- r$runpar$iter

		# iter must be at least 2
		if ( ! (iter > 1) ) {
				warning( paste0( "number of iterations (=", iter, ") is smaller 2, NULL is returned" ) )
				est <- NULL
		} else {
				# defaults of burnin
				if (!exists("burnin",mode="numeric")) burnin <- 0
				
				## iter must be adjusted
				# if jags runs with thin, then results are of iter/thin length
				thin <- r$runpar$thin
				if( is.null( thin ) ) thin <- 1
				iter <- floor( iter / thin )
				if( thin > 1 ) {
						cat( paste0( "iterations have been adjusted for thinning: ",r$runpar$iter," -> ",iter,"\n\n" ) )
				}
				
				# burnin must be lower than iterations
				if ( burnin >= iter ) {
						cat( paste0( "burnin=", burnin, " is not lower than iter=", iter, "; now set to iter/2=", burnin <- floor( iter/2 ), " \n\n" ) )
				}
				
				
				# output
				if ( verbose ){
						if ( mode %in% "jags" )	engine.str <- "JAGS" else if ( mode %in% "ctstan" )	engine.str <- "Stan" else engine.str <- ""
						cat( paste0( "Extracting results from ",engine.str," object...", "\n" ) )
						if ( mode %in% c("jags","ctstan") ) cat( paste0( "\n   burnin: ", burnin, "\n" ) )
						# cat( paste0( "            iterations: ", iter, "\n" ) )
						# cat( paste0( "                chains: ", chains, "\n" ) )
						# cat( paste0( "     thinning interval: ", thin, "\n" ) )
						cat( paste0( "\n" ) )
						cat( paste0( "   fetching results:\n\n" ) )
						flush.console()
				}

		# browser()
				# make list of all parameters
				pars.l <- mapply( get.par.list, r$parameters, names( r$parameters ), MoreArgs=list(mode=mode), SIMPLIFY=FALSE )
				pars <- do.call( "rbind", pars.l )
				rownames( pars ) <- seq( along=rownames( pars ) )
		# browser()
				### for ctstan there musn't be . in the parameter names
				# if ( mode %in% "ctstan" ) {
						# pars$parameter.mod <- gsub( ".", "", pars$parameter, fixed=TRUE )
				# } else if ( mode %in% "jags" ) {
						# pars$parameter.mod <- pars$parameter
				# } else {
						# pars$parameter.mod <- pars$parameter
				# }
				
				# indvarying <- rep( FALSE, length(pars) )
				# est.l <- try( mapply( extr, pars, SIMPLIFY=FALSE ) )
				extr <- function( z ) {
						
						if ( verbose ) cat( paste0( "      ", z["parameter"], "\n" ) ); flush.console()
						
						# if ( !indvarying & !par %in% c("lp__") & !grepl("^eta",par) ){
							# iterations/chains fuer parameter
							# x <- extract( fit, pars=paste0("output_hmean_", par), permuted=FALSE, inc_warmup=TRUE )[,,1]
						# } else {
							# x <- extract( fit, pars=par, permuted=FALSE, inc_warmup=TRUE )[,,1]
						# }
		# browser()
						# get chains of parameter
						# x <- eval( parse( text=paste0( "r$results$",z["call"],"" ) ) )
						x <- eval( parse( text=paste0( z["call"] ) ) )
					
						# wide ( iteration x chain ) to long
						# data set primarily for iteration plot, that's why complete, inclusive burnin
						# multiple chains -> melt
						# single chain -> manual mod
		# browser()
						if( is.null(dim(x)) ) {
								xl <- matrix( NA, ncol=3, nrow=length(x) )
								xl[,1] <- 1:length(x)
								xl[,2] <- 1
								xl[,3] <- x
								xl <- data.frame( xl )
								colnames( xl ) <- c( "iterations", "chains", "value" )
								x <- dcast( xl, iterations ~ chains, value.var="value" )				
						} else {
								# library( reshape2 )
								# loadNamespace( "reshape2" )
								xl <- melt( x )
								colnames( xl ) <- c( "iterations", "chains", "value" )
						}
						# chains to factor
						if( is.numeric( xl$chains ) ) xl$chains <- factor( xl$chains, levels=sort(unique(xl$chains)), labels=paste0( "chain:", sort(unique(xl$chains)) ) )
		# browser()				
						# delete burnin, original wide data
						x2 <- x[((burnin+1):iter),,drop=FALSE]
						
						### delete outlier chains
						# means after burnin
						mab <- apply( x2, 2, mean )
						del <- abs( mab - mean( mab ) ) > 2*sd( mab )
						if ( any ( del ) ) {
								# aus burnin geloeschten Daten rausnehmen
								x2 <- x2[,!del,drop=FALSE]
								chains <- ncol( x2 )
								
								# aus Iterationsplot-Datensatz die geloeschte Kette hinter burnin rausnehmen, damit mans sieht
								xl <- xl[ !( xl$chains %in% names(del)[del] & xl$iterations > burnin), ]
						} else {
								chains <- r$runpar$chains
								### from here on chains!!!
						}
		# browser()
						if( !is.null( plot.dir ) ){
								
								# create plot.dir if not exists
								if( !dir.exists( plot.dir ) ){
										dir.create( plot.dir, recursive=FALSE )
								}
		# browser()
								# conditionally no plots for ind varying
								if( ! ( z["name"] %in% c("theta","bj","mu.t1.j") && !plot.person.par ) ){
								
										# Iteration-Plots fuer nicht ind varying
										# if (!indvarying) {
												# Iteration-Plots
												# pdf( file.path( folder.plots, paste0( par, ".pdf" ) ) )
												suppressWarnings(
														pl <- ggplot(xl, aes(x=iterations, y=value, colour=chains)) + geom_point(shape=16, size=1.5, alpha=0.50) +
														scale_colour_hue(l=50) + # Use a slightly darker palette than normal
														geom_smooth(method="loess",
																se=FALSE,    # Don't add shaded confidence region
																fullrange=TRUE) + # Extend regression lines
														geom_vline(xintercept = burnin+0.5) +
														theme_bw() +
														theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank() )
												)
												suppressWarnings(
														ggsave( file.path( plot.dir, paste0( z["parameter"], ".png" ) ), pl, width=2*29.7, height=21, units="cm", dpi=300 )
												)
												# dev.off()
										# }
								}
								
						}

								
						# mcmc-Objekt bauen
						do <- paste0( "as.mcmc.list( list( ", paste( paste0( " as.mcmc( x2[,",1:chains,"] ) " ), collapse="," ) , " ) )" )
						mcmclist <- eval( parse( text=do ) )

						# shinystan-Objekt bauen
						do <- paste0( "as.shinystan( list( ", paste( paste0( "  as.matrix( data.frame( '",z["parameter"],"' = x2[,",1:chains,"] ) ) " ), collapse="," ) , " ) )" )
						sso <- eval( parse( text=do ) )
		# browser()				
						if( chains > 1 ){
								psrf.coda.obj <- gelman.diag( mcmclist )
								psrf.coda <- unname(psrf.coda.obj$psrf[1,1])
								psrf.UL.coda <- unname(psrf.coda.obj$psrf[1,2])
						} else {
								psrf.coda <- NA
								psrf.UL.coda <- NA
						}
						psrf.ss <- retrieve( sso, "Rhat" )
						effN.coda <- effectiveSize( mcmclist )
						effN.ss <- retrieve( sso, "Neff" )
						# stats <- summary( mcmclist )
						relMCSE.ss <- retrieve( sso, "mcse" ) / retrieve( sso, "sd" )
						
						# return data.frame
						# ret <- data.frame( "variable"=z["parameter"], "value"=unname(stats$statistics["Mean"]), "SD"=unname(stats$statistics["SD"]), "Naive SE"=unname(stats$statistics["Naive SE"]), "Time-series SE"=unname(stats$statistics["Time-series SE"]), "psrf.coda"=unname(psrf.coda$psrf[1,1]), "psrf.UL.coda"=unname(psrf.coda$psrf[1,2]), "psrf.ss"=psrf.ss, "effN.coda"=unname(effN.coda), "effN.ss"=effN.ss, "relMCSE"=relMCSE )

						# put all chains in one long chain
						onechain <- do.call( "c", sapply( mcmclist, as.numeric, simplify=FALSE ) )
		# browser()				
						# return data.frame
						ret <- data.frame( "name"=z["name"], "variable"=z["parameter"], "mode"=mlv( onechain, method="density" )$M, "median"=unname(retrieve(sso,"quantiles")["50%"]), "mean"=retrieve(sso,"mean"), "hdi95.lb" = unname(retrieve(sso,"quantiles")["2.5%"]), "hdi95.ub" = unname(retrieve(sso,"quantiles")["97.5%"]), "sd"=retrieve(sso,"sd"), "mcse.ss"=retrieve( sso, "mcse" ), "relative.mcse.ss"=relMCSE.ss, "psrf.coda"=psrf.coda, "psrf.UL.coda"=psrf.UL.coda, "psrf.ss"=psrf.ss, "effN.coda"=unname(effN.coda), "effN.ss"=effN.ss, "engine"=engine, stringsAsFactors=FALSE )
						
						# if( chains==1 ) { psrf.est <- psrf.UL <-NA } else { psrf <- gelman.diag( mcmclist ); psrf.est <- unname(psrf$psrf[1,1]); psrf.UL <- unname(psrf$psrf[1,2]) }
						# effN <- effectiveSize( mcmclist )
						# stats <- summary( mcmclist )
						
						# return data.frame
						# ret <- data.frame( "variable"=par, "value"=unname(stats$statistics["Mean"]), "SD"=unname(stats$statistics["SD"]), "Naive SE"=unname(stats$statistics["Naive SE"]), "Time-series SE"=unname(stats$statistics["Time-series SE"]), "psrf.est"=psrf.est, "psrf.UL"=psrf.UL, "effN"=unname(effN) )
						
						return( ret )
				}

				# operating system
				os <- Sys.info()["sysname"]
				# parallelization yes/no
				parall <- exists("cores") && !is.null(cores) && cores > 1 && os %in% c("Windows","Linux")
				if( parall ){

						if( os %in% "Windows" )	eval( parse( text=paste0( "cl <- makePSOCKcluster(",cores,")")) )
						if( os %in% "Linux" )	eval( parse( text=paste0( "cl <- makeForkCluster(",cores,")")) )
						registerDoParallel(cl)
						
						pack <- c("reshape2","ggplot2","coda","shinystan",ifelse(engine %in% "ctstan","rstan",NA) )
						pack <- pack[!is.na(pack)]
						
						est.l. <- foreach(par=1:nrow(pars),.export="r",.packages=pack) %dopar% {        
								# calc results for all parameters
								est.l <- apply( pars[par,], 1, extr )
						}
						est.l <- unlist( est.l., recursive=FALSE )
				
						stopCluster(cl)
				} else {
						# calc results for all parameters
						est.l <- apply( pars, 1, extr )
				}
				est <- do.call( "rbind", est.l )
		# browser()
			
				### software specific mods
				
				## conversions
				if ( verbose && engine %in% c("jags","ctstan") ) {
						cat( paste0( "\n   converting results:\n\n" ) )
				}
		# browser()	
				## jags
				if ( engine %in% "jags" ) {
		# browser()					
						# prec.t1 in jags is precision matrix, transform to variance matrix
						if( "prec.t1" %in% est$name ) {
								if (verbose) { cat( paste0( "      prec.t1 -> var.t1\n" ) ); flush.console() }
								est <- transform.var.matrix( parameters$prec.t1, "prec.t1", "var.t1", "solve( M )", est, value )
						}
		# browser()			
						# prec.mu.t1.j in jags is precision matrix, transform to variance matrix
						if( "prec.mu.t1.j" %in% est$name ) {
								if (verbose) { cat( paste0( "      prec.mu.t1.j -> var.mu.t1.j\n" ) ); flush.console() }
								est <- transform.var.matrix( parameters$prec.mu.t1.j, "prec.mu.t1.j", "var.mu.t1.j", "solve( M )", est, value )
						}
						
						# if prec.beta exists, transform to variance
						if( "prec.beta" %in% est$name ) {
								if (verbose) { cat( paste0( "      prec.beta -> var.beta\n" ) ); flush.console() }
								est <- transform.var.matrix( parameters$prec.beta, "prec.beta", "var.beta", "solve( M )", est, value )				
						}

						# if prec.b exists, transform to variance
						if( "prec.b" %in% est$name ) {
								if (verbose) { cat( paste0( "      prec.b -> var.b\n" ) ); flush.console() }
								est <- transform.var.matrix( parameters$prec.b, "prec.b", "var.b", "solve( M )", est, value )				
						}
						
				}
				
				## ctstan
				if ( engine %in% "ctstan" ) {
		# browser()		
						# cholQ in ctstan is cholesky matrix, transform to variance matrix
						if( "cholQ" %in% est$name ) {
								if (verbose) { cat( paste0( "            cholQ -> Q\n" ) ); flush.console() }
								est <- transform.var.matrix( parameters$cholQ, "cholQ", "Q", "solve( chol2inv( t( M ) ) )", est, value )
						}
						
						# chol.var.t1 in ctstan is cholesky matrix, transform to variance matrix
						if( "chol.var.t1" %in% est$name ) {
								if (verbose) { cat( paste0( "      chol.var.t1 -> var.t1\n" ) ); flush.console() }
								est <- transform.var.matrix( parameters$chol.var.t1, "chol.var.t1", "var.t1", "solve( chol2inv( t( M ) ) )", est, value )
						}
						
						# sd.b to var.b
						if( "sd.b" %in% est$name ) {
								if (verbose) { cat( paste0( "             sd.b -> var.b\n" ) ); flush.console() }
# browser()						
								### Achtung, nur für sd->var, nicht für corr->cov, muss noch abgefangen werden
								est <- transform.var.matrix( parameters$sd.b, "sd.b", "var.b", "M^2", est, value )
						
								# diagonal of chol.var.b in ctstan is sd, transform to variance
								# if (verbose) { cat( paste0( "      chol.var.b -> var.b\n" ) ); flush.console() }
								# est <- transform.var.matrix( parameters$chol.var.b, "chol.var.b", "var.b", "M^2", est )
						}
						
				}
				
				if ( verbose && engine %in% c("jags","ctstan") ) cat( "\n" )
		# browser()	

				## additional parameters		
				if ( engine %in% "ctstan" ) {

						### variance of b
						# get all b
						#bs <- names( r$results )
						#bs <- bs[ grepl( "output_hsd_b", bs ) ]
						#bs.par.name <- sub( "output_hsd_", "", bs )
                        #
						#if ( length(bs) > 0 ) {
		                #
						#		pars2 <- data.frame( "name"="var.b", "parameter"=paste0("var.",bs.par.name), "call"=paste0("extract( r$results, pars=paste0('output_hsd_', '",bs.par.name,"'), permuted=FALSE, inc_warmup=TRUE )[,,1]"), stringsAsFactors=FALSE )
						#		est.l2 <- apply( pars2, 1, extr )
						#		est2 <- do.call( "rbind", est.l2 )
						#		# square (because it's sd)
						#		est2[,value] <- est2[,value]^2
						#		
						#		# bind on estimates
						#		est <- rbind( est, est2 )
						#}
						
						### variance of mu.t1.j
						# get all mu.t1.j
						bs <- names( r$results )
						bs <- bs[ grepl( "output_hsd_mut1", bs ) ]
						bs.par.name <- sub( "output_hsd_", "", bs )
		# browser()				
						bs.par.name.new <- sub( "mut1", "var.mu.t1.j", bs.par.name )
						if ( length(bs) > 0 ) {
		# browser()				
								
		
								pars2 <- data.frame( "name"="var.mu.t1.j", "parameter"=bs.par.name.new, "call"=paste0("extract( r$results, pars=paste0('output_hsd_', '",bs.par.name,"'), permuted=FALSE, inc_warmup=TRUE )[,,1]"), stringsAsFactors=FALSE )
								est.l2 <- apply( pars2, 1, extr )
								est2 <- do.call( "rbind", est.l2 )
								# square (because it's sd)
								est2[,value] <- est2[,value]^2
								
								# bind on estimates
								est <- rbind( est, est2 )
						}
				
						if ( verbose && length(bs) > 0 ) {
								cat( paste0( "\n   additional results:\n\n" ) )
								cat( paste0( "             var.mu.t1.j\n" ) ); flush.console()
						}								
				
				}
				rownames( est ) <- seq( along=rownames( est ) )
		} 
		
		# return
		return( est )
}

get.par.list <- function( m, m.name, mode ){
# browser()
		
		# data structures to long
		dim.m <- dim( m )
		if( is.null( dim.m ) ) {
				
				# long structure
				m. <- data.frame( matrix( seq( along= m ), ncol=1 ) )
				
		} else {
		# if structure with dimensions (e.g. matrix)
		
				# long structure
				m. <- eval(parse(text=paste0( "Reduce(function(x, y) merge(x, y, by=NULL, all=TRUE),list(", paste( paste0( "1:", dim.m ), collapse="," ), "),accumulate=FALSE )" )))

		}
		m.$parameter <- apply( m., 1, function ( z ) eval( parse( text= paste0( "m[", paste(z,collapse=","), "]" ) ) ) )
		# NOT duplicated free parameters
		m. <- m.[ !duplicated(m.$parameter) & is.na(suppressWarnings(as.numeric(m.$parameter))), ]
# browser()
		

		### for ctstan/ctsem there musn't be . in the parameter names
		if ( mode %in% c("ctstan","ctsem") ) {
				m.$parameter.mod <- gsub( ".", "", m.$parameter, fixed=TRUE )
# browser()
				if ( mode %in% c("ctsem") && nrow(r$parameters$A)==1 ) {
						# for ctsem object name must not be equal to parameter name
						# usually problem if F==1
						m.$parameter.mod <- paste0( m.$parameter.mod, "_" )
				}
		} else if ( mode %in% "jags" ) {
				m.$parameter.mod <- m.$parameter
		} else {
				m.$parameter.mod <- m.$parameter
		}				
		
		# name
		m.$name <- m.name

		# call (how to access parameter)
		if ( mode %in% "jags" ) {
				m.$call <- apply( m.[,colnames(m.)[!colnames(m.) %in% c("parameter","parameter.mod","name")],drop=FALSE], 1, function ( z ) paste0( "r$results$'",m.name,"'[", paste(z,collapse=","), ",,]" ) )
		} else if ( mode %in% "ctstan" ) {
				# if ( !indvarying & !par %in% c("lp__") & !grepl("^eta",par) ){
						# iterations/chains fuer parameter
	
						# determine if hmean or hsd
						if( any( m.$name %in% c("sd.b") ) ) morsd <- "hsd_" else morsd <- "hmean_"
						if( any( m.$name %in% c("lp__") ) ) morsd <- ""
						if( any( grepl( "^tipred", m.$name ) ) ) morsd <- ""
						if( any( grepl( "^tdpred", m.$name ) ) ) morsd <- ""
						# morsd <- "hmean"
# browser()				
# if( any( m.$name %in% c("var.b") ) ) browser()
						# spezial
						if( any( m.$name %in% c("sd.b") ) ) m.$parameter.mod <- "b"
						
						### mod for var.b
						# if( any( m.$name %in% c("var.b") ) ) {
								# delete prefix "cholvar"
								# m.$parameter.mod <- sub( "cholvar", "", m.$parameter.mod )
# browser()
								# somehow parameters names are adjusted
								# delete last doubled suffix _theta1 / _theta2
								# m.$parameter.mod <- sub( "\\_theta1$", "", m.$parameter.mod )
								# m.$parameter.mod <- sub( "\\_theta2$", "", m.$parameter.mod )
								
								# somehow offdiag are missing, delete for now
								# m. <- m.[ (m.$x==m.$y),  ]
						# }
						
						# standard call
						# m.$call <- paste0( "extract( r$results, pars=paste0('output_",morsd,"_', '",m.$parameter.mod,"'), permuted=FALSE, inc_warmup=TRUE )[,,1]" )
						## since ctsem version 2.1.0: r$results$stanfit
						## since ctsem version 2.1.0: no output as prefix
						m.$call <- paste0( "extract( r$results$stanfit, pars=paste0('",morsd,"', '",m.$parameter.mod,"'), permuted=FALSE, inc_warmup=TRUE )[,,1]" )
# if( any( m.$name %in% c("sd.b") ) ) browser()						
						# call for ind varying b (bj) 
						if ( any( m.$name %in% "bj" ) ){
								# CINT matrix in ctstan is other way round, so switch
								cint.dfr <- m.[,!colnames(m.) %in% c("parameter","parameter.mod","name","call")]
								cint.dfr <- cint.dfr[ , rev(colnames(cint.dfr)) ]
								m.$call <- paste0( "extract( r$results$stanfit, pars=paste0('CINT[",apply( cint.dfr , 1, function(z) paste0( z, collapse="," ) ),"]'), permuted=FALSE, inc_warmup=TRUE )[,,1]" )
						}

						# call for ind varying mu.t1 (mu.t1.j) 
						if ( any( m.$name %in% "mu.t1.j" ) ){
								# T0MEANS matrix in ctstan is other way round, so switch
								t0means.dfr <- m.[,!colnames(m.) %in% c("parameter","parameter.mod","name","call")]
								t0means.dfr <- t0means.dfr[ , rev(colnames(t0means.dfr)) ]
								m.$call <- paste0( "extract( r$results$stanfit, pars=paste0('T0MEANS[",apply( t0means.dfr , 1, function(z) paste0( z, collapse="," ) ),"]'), permuted=FALSE, inc_warmup=TRUE )[,,1]" )
						}
						
				# } else {
						# x <- extract( fit, pars=par, permuted=FALSE, inc_warmup=TRUE )[,,1]
				# }
		} else if ( mode %in% "ctsem" ) {
				m.$call <- paste0( "summary( r$results )$ctparameters$Value[  summary( r$results )$ctparameters$'Continuous time free param' %in% c('",m.$parameter.mod,"') ]" )
		}
		
		# sort
		m. <- m.[,c("name","parameter","call")]
		
		# return
		return( m. )
}
		
transform.var.matrix <- function( matr, matr.name, matr.name.replace, transform.string, est, value ){
# browser()		
		parM <- M <- matr
		for ( i in 1:length(M) ) {
				val <- est[ est$variable %in% M[i], value ]
				if ( length(val) > 0 ) M[i] <- est[ est$variable %in% M[i], value ]
		}
		dim.M <- dim( M )
		M <- suppressWarnings( as.numeric( M ) )
		dim( M ) <- dim.M
		
		# transform
		eval( parse( text=paste0( "P <- ", transform.string ) ) )
		
		estP <- est[ est$name %in% matr.name, ]
		for ( i in 1:length(parM) ) {
				estP[ estP$variable %in% parM[i], value ] <- P[i]
		}
		# all other statistics to NA
		toNA <- colnames( estP )[ !colnames( estP ) %in% c("name","variable",value,"engine") ]
		eval( parse( text=paste0( "estP$'", toNA, "' <- NA" ) ) )
		# rename
		estP$name <- sub( matr.name, matr.name.replace, estP$name )
		estP$variable <- sub( matr.name, matr.name.replace, estP$variable )
		# sort transformed parameter behind original
		ind <- which( est$name %in% matr.name )
		ind <- ind[length(ind)]
		if ( ind >= nrow( est ) ) {
				mL <- list( est[1:ind,,drop=FALSE] , estP )
		} else  mL <- list( est[1:ind,,drop=FALSE] , estP , est[(ind+1):nrow(est),,drop=FALSE] )
		est <- do.call( "rbind", mL )
		
		return( est )
}
