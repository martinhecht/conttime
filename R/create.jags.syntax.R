
create.jags.syntax <- function ( env ) {

		# get variables from env
		eval( parse( text=paste0( "assign( '",ls(envir=env), "' , get('",ls(envir=env),"', envir=env ) )" ) ) )
		
		# objects with free parameters must be overwritten or deleted
		# environment to save original objects
		par.env <- new.env()
		
		# syntax matrix (1 column)
		x<-matrix(  "model", 1, 1 )
		x<-rbind(x, "{                                                                    ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    ### DATA ###                                                     ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # data dl in long format                                         ")
		x<-rbind(x, "    # dl[,col.id]: person                                            ")
		x<-rbind(x, "    # dl[,col.item]: item                                            ")
		x<-rbind(x, "    # dl[,col.time]: time point                                      ")
		x<-rbind(x, "    # dl[,col.y]: responses                                          ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                                                                     ")		
		x<-rbind(x, "    ### MEASUREMENT MODEL ###                                        ")
		x<-rbind(x, "                                                                     ")
		# x<-rbind(x, "    # estimated parameters                                           ")
		# x<-rbind(x, "    # theta:        J (person)    x F (faktor)    x T (time point)   ")
		# x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # loop over rows in long data set dl                             ")
		x<-rbind(x, "    for (r in 1:R) {                                                 ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "        # distributional assumption                                  ")
		if( measurement.model$family %in% "gaussian" ) {
		x<-rbind(x, "        dl[r,col.y] ~ dnorm( mu.y[r], E[ dl[r,col.item], dl[r,col.item] ] ) ") }
		if( measurement.model$family %in% "binomial" ) {
		x<-rbind(x, "        dl[r,col.y] ~ dbern( mu.y[r] )                               ") }
		if( measurement.model$family %in% "poisson" ) {
		x<-rbind(x, "        dl[r,col.y] ~ dpois( mu.y[r] ) ") }
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "        # link                                                       ")
		if( measurement.model$family %in% "gaussian" ) {
		x<-rbind(x, "        mu.y[r] <- eta[r]                                            ") }		
		if( measurement.model$link %in% "logit" ) {
		x<-rbind(x, "        mu.y[r] <- ilogit( eta[r] )                                  ") }
		if( measurement.model$link %in% "log" ) {
		x<-rbind(x, "        log( mu.y[r] ) <- eta[r]                                     ") }
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "        # linear predictor                                           ")
		# x<-rbind(x, "        eta[r] <- sum( Lambda[ dl[r,col.item],  , dl[r,col.time] ] * theta[ dl[r,col.id], , dl[r,col.time] ] )  +  beta[ dl[r,col.item], 1 ]  ")
		x<-rbind(x, "        eta[r] <- sum( Lambda[ dl[r,col.item], ] * theta[ dl[r,col.id], , dl[r,col.time] ] )  +  beta[ dl[r,col.item], 1 ] ")
		# x<-rbind(x, "        eta[r] <- sum( Lambda[ dl[r,col.item], ] * theta[ dl[r,col.id], , dl[r,col.time] ] ) ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "                                                                     ")
		if ( exists( "E" ) ) {
		x<-rbind(x, "    # precision of measurement errors                                ")
		x<-rbind(x, make.str( "E" ) ); if( any ( is.parameter( E ) ) ) invisible(moveTo.par.env("E",env,par.env)) else rm("E", envir=env)
		x<-rbind(x, "                                                                     ") }		
		# if( measurement.model$family %in% "gaussian" ) {		
		# x<-rbind(x, "    # precision of measurement errors                                ")
		# x<-rbind(x, "    for (i in 1:I) {                                                 ")
		# x<-rbind(x, "        E[i] ~ dgamma(1,0.005)                                       ")
		# x<-rbind(x, "    }                                                                ") 
		# x<-rbind(x, "                                                                     ") }
		x<-rbind(x, "    # values/priors of item easiness                                 ")
		x<-rbind(x, make.str( "beta" ) ); if( any ( is.parameter( beta ) ) ) invisible(moveTo.par.env("beta",env,par.env)) else rm("beta", envir=env)
		x<-rbind(x, "                                                                     ")
		if ( exists( "mu.beta" ) ) {
		x<-rbind(x, "    # values/priors of mean of item easiness                         ")
		x<-rbind(x, make.str( "mu.beta" ) ); if( any ( is.parameter( mu.beta ) ) ) invisible(moveTo.par.env("mu.beta",env,par.env)) else rm("mu.beta", envir=env)
		x<-rbind(x, "                                                                     ") }
		if ( exists( "mu.beta" ) ) {
		x<-rbind(x, "    # values/priors of precision of item easiness                    ")
		x<-rbind(x, make.str( "prec.beta" ) ); if( any ( is.parameter( prec.beta ) ) ) invisible(moveTo.par.env("prec.beta",env,par.env)) else rm("prec.beta", envir=env)
		x<-rbind(x, "                                                                     ") }
		x<-rbind(x, "    # values/priors of Lambda                                        ")
		x<-rbind(x, make.str( "Lambda" ) ); if( any ( is.parameter( Lambda ) ) ) invisible(moveTo.par.env("Lambda",env,par.env)) else rm("Lambda", envir=env)
		x<-rbind(x, "                                                                     ")		
		# x<-rbind(x, "    # standard deviation of latent scale                             ")
		# x<-rbind(x, "    #sd.eta <- sd(eta[])                                             ")
		# x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    ### CONTINUOUS TIME MODEL ###                                    ")
		x<-rbind(x, "                                                                     ")
# browser()		
		if( !exists( "stationarity" ) ) stationarity <- FALSE
		if( person.var["mu.t1"] ) {
				if (! stationarity ){
						x<-rbind(x, "    ## personal t1 means                                               ")
						x<-rbind(x, "    # mu.t1 varies over persons, i.e. for each person a separate mu.t1 ")		
						mu.t1.j. <- make.str( "mu.t1.j" ); if( any ( is.parameter( mu.t1.j ) ) ) invisible(moveTo.par.env("mu.t1.j",env,par.env)) else rm("mu.t1.j", envir=env)
						# mods to mu.t1.j.
						mu.t1.j.1 <- mu.t1.j.[ !grepl( "~", mu.t1.j., fixed=TRUE ),,drop=FALSE ]
						mu.t1.j.2 <- mu.t1.j.[ grepl( "~", mu.t1.j., fixed=TRUE ),,drop=FALSE ]
						if( F>1 )  mu.t1.j.2[,1] <- sub( "mu.t1.j\\[\\d+", "mu.t1.j[1:F", mu.t1.j.2[,1] )
						if( F==1 ) mu.t1.j.2[,1] <- sub( "mu.t1.j\\[\\d+", "mu.t1.j[1", mu.t1.j.2[,1] )
						mu.t1.j.2 <- mu.t1.j.2[!duplicated(mu.t1.j.2),,drop=FALSE] 	
						if( !identical( mu.t1.j.1, character(0) ) ) x<-rbind(x,mu.t1.j.1)
						if( !identical( mu.t1.j.2, character(0) ) ) x<-rbind(x,mu.t1.j.2) 
						x<-rbind(x, "                                                                     ")		
						x<-rbind(x, "    # values/prior for prec.mu.t1.j                                         ")	
						x<-rbind(x, ifelse( exists("prec.mu.t1.j.prior") && is.null( dim(prec.mu.t1.j.prior) ), paste0( "    prec.mu.t1.j[1:F,1:F] ~ ", prec.mu.t1.j.prior, "                                      " ), make.str( "prec.mu.t1.j" ) ) ); if( any ( is.parameter( prec.mu.t1.j ) ) ) invisible(moveTo.par.env("prec.mu.t1.j",env,par.env)) else rm("prec.mu.t1.j", envir=env)
				}
				if (stationarity){				
						mu.t1.j. <- make.str( "mu.t1.j" )
						mu.t1.j.1 <- mu.t1.j.[ !grepl( "~", mu.t1.j., fixed=TRUE ),,drop=FALSE ]
						mu.t1.j.2 <- mu.t1.j.[ grepl( "~", mu.t1.j., fixed=TRUE ),,drop=FALSE ]
						if( F>1 )  mu.t1.j.2[,1] <- sub( "mu.t1.j\\[\\d+", "mu.t1.j[1:F", mu.t1.j.2[,1] )
						if( F==1 ) mu.t1.j.2[,1] <- sub( "mu.t1.j\\[\\d+", "mu.t1.j[1", mu.t1.j.2[,1] )
						mu.t1.j.2 <- mu.t1.j.2[!duplicated(mu.t1.j.2),,drop=FALSE]
						mu.t1.j.3 <- sub( "^(.*)~.*$", "\\1", mu.t1.j.2 )
						mu.t1.j.4 <- matrix( paste0( mu.t1.j.3, " <- -1 * A.inv[,] %*% bj[,", as.integer( sub( "^.*,(.*)\\].*$", "\\1", mu.t1.j.3 ) ) ,"]" ), ncol=1 )
						if( !identical( mu.t1.j.4, character(0) ) ) x<-rbind(x,mu.t1.j.4)
						# push to parameter env, because we want results for that
						if( any ( is.parameter( mu.t1.j ) ) ) invisible(moveTo.par.env("mu.t1.j",env,par.env)) else rm("mu.t1.j", envir=env)
				}
		}
		
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # loop over persons j                                            ")
		x<-rbind(x, "    for (j in 1:J) {                                                 ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "            # first time point                                       ")
		if( !person.var["mu.t1"] ) {
		x<-rbind(x, paste0( "            theta[j,1:F,1] ~ d",ifelse(F>1,"m",""),"norm( mu.t1, prec.t1 )                ") ) }
		if( person.var["mu.t1"] ) {
		x<-rbind(x, paste0( "            theta[j,1:F,1] ~ d",ifelse(F>1,"m",""),"norm( mu.t1.j[,j], prec.t1 )              ") ) }
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "            # loop over t=2,...,Tj personal time point               ")
		x<-rbind(x, "            for (t in 2:Tj[j]) {                                     ")
		x<-rbind(x, "                                                                     ")
# browser()
		if( !person.var["b"] ) {
		x<-rbind(x, paste0( "                    theta[j,1:F,t] ~ d",ifelse(F>1,"m",""),"norm( At[,,t-1,Lpat.group[j]] %*% theta[j,,t-1] + bt[,t-1,Lpat.group[j]], Qt.prec[,,t,Lpat.group[j]] ) ") ) }
		if( person.var["b"] ) {
		x<-rbind(x, paste0("                    theta[j,1:F,t] ~ d",ifelse(F>1,"m",""),"norm( At[,,t-1,Lpat.group[j]] %*% theta[j,,t-1] + bt[,t-1,j], Qt.prec[,,t,Lpat.group[j]] ) ") ) }
		
		x<-rbind(x, "                                                                     ")		
		x<-rbind(x, "            }                                                        ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    ## values/prior of parameters                                    ")
		# x<-rbind(x, "    ## Note: values are commented (already set in R)                 ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # values/prior of mean of first time point                       ")
		if( !person.var["mu.t1"] || !stationarity ) {
		x<-rbind(x, make.str( "mu.t1" ) ); if( any ( is.parameter( mu.t1 ) ) ) invisible(moveTo.par.env("mu.t1",env,par.env)) else rm("mu.t1", envir=env)
		} else {
		x<-rbind(x, "    for (f in 1:F) {" )
		x<-rbind(x, "         mu.t1[f] <- mean( mu.t1.j[f,] )" )
		x<-rbind(x, "    }" )
		if( any ( is.parameter( mu.t1 ) ) ) invisible(moveTo.par.env("mu.t1",env,par.env)) else rm("mu.t1", envir=env)
		}
		
		x<-rbind(x, "                                                                     ")
# browser()		
		x<-rbind(x, "    # values/prior of precision of first time point                  ")
		# x<-rbind(x, ifelse( exists("prec.t1.prior") && is.null( dim(prec.t1.prior) ), paste0( "    prec.t1[1:F,1:F] ~ ", prec.t1.prior, "                                      " ), make.str( "prec.t1" ) ) ); if( any ( is.parameter( prec.t1 ) ) ) invisible(moveTo.par.env("prec.t1",env,par.env)) else rm("prec.t1", envir=env)
		x<-rbind(x, make.str( "prec.t1" ) ); if( any ( is.parameter( prec.t1 ) ) ) invisible(moveTo.par.env("prec.t1",env,par.env)) else rm("prec.t1", envir=env)

		# prec.t1 is symmetric, needs definition of upper to lower elements
		# nasty workaround here
		# prec.t1.prior <- prec.t1
		# pstr <- make.str( "prec.t1" )
		# pstr <- pstr[ grepl( "<-", pstr[,1] ), ,drop=FALSE]
		# if( nrow( pstr ) > 0 ) {
				# x<-rbind(x,pstr)
		# }
		
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # values/prior of drift matrix                                   ")
		x<-rbind(x, make.str( "A" ) ); if( any ( is.parameter( A ) ) ) invisible(moveTo.par.env("A",env,par.env)) else rm("A", envir=env)
		x<-rbind(x, "                                                                     ")		
		x<-rbind(x, "    # values/prior of diffusion matrix                               ")
# browser()		
		x<-rbind(x, make.str( "Q" ) ); if( any ( is.parameter( Q ) ) ) invisible(moveTo.par.env("Q",env,par.env)) else rm("Q", envir=env)
		x<-rbind(x, "                                                                     ")	
# browser()
		if( person.var["b"] ) {		
		x<-rbind(x, "    ## continuous time intercepts                                    ")
		x<-rbind(x, "    # b varies over persons, i.e. for each person a separate b       ")		
		# x<-rbind(x, "    for( j in 1:J ) {                                                ")		
		# x<-rbind(x, "         bj[1:F,j] ~ dmnorm( b[,1], prec.b[,] )                      ")		
		# x<-rbind(x, "    }                                                                ") }
		bj. <- make.str( "bj" ); if( any ( is.parameter( bj ) ) ) invisible(moveTo.par.env("bj",env,par.env)) else rm("bj", envir=env)
		# mods to bj.
		bj.1 <- bj.[ !grepl( "~", bj., fixed=TRUE ),,drop=FALSE ]
		bj.2 <- bj.[ grepl( "~", bj., fixed=TRUE ),,drop=FALSE ]
		if( F>1 )  bj.2[,1] <- sub( "bj\\[\\d+", "bj[1:F", bj.2[,1] )
		if( F==1 ) bj.2[,1] <- sub( "bj\\[\\d+", "bj[1", bj.2[,1] )
		bj.2 <- bj.2[!duplicated(bj.2),,drop=FALSE] 	
		if( !identical( bj.1, character(0) ) ) x<-rbind(x,bj.1)
		if( !identical( bj.2, character(0) ) ) x<-rbind(x,bj.2) }
		
		x<-rbind(x, "    # values/prior of continuous time intercepts                     ")		
		x<-rbind(x, make.str( "b" ) ); if( any ( is.parameter( b ) ) ) invisible(moveTo.par.env("b",env,par.env)) else rm("b", envir=env)
		if( person.var["b"] ) {
		x<-rbind(x, "    # values/prior of precision of b                                 ")
		# x<-rbind(x, ifelse( exists("prec.b.prior") && is.null( dim(prec.b.prior) ), paste0( "    prec.b[1:F,1:F] ~ ", prec.b.prior, "                                      " ), make.str( "prec.b" ) ) ); if( any ( is.parameter( prec.b ) ) ) invisible(moveTo.par.env("prec.b",env,par.env)) else rm("prec.b", envir=env) }
# browser()	
		x<-rbind(x, make.str( "prec.b" ) ); if( any ( is.parameter( prec.b ) ) ) invisible(moveTo.par.env("prec.b",env,par.env)) else rm("prec.b", envir=env) }
		
		x<-rbind(x, "                                                                     ")			
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # rowQ: stack Q row-wise into a column vector                    ")
		x<-rbind(x, "    for ( z in (1:Aw) ) {                                            ")
		x<-rbind(x, "        for ( c in (1:Aw) ) {                                        ")
		x<-rbind(x, "            rowQ[ c+Aw*(z-1), 1 ] <- Q[z,c]                          ")
		x<-rbind(x, "        }                                                            ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    ## discrete/continuous time relations                            ")
		x<-rbind(x, "    # Qtv for first time point                                       ")
		x<-rbind(x, "    # for technical reasons, same values are set for all lag patterns (not necessary)  ")
		x<-rbind(x, "    for(p in 1:P) {                                                  ")
		x<-rbind(x, "            Qtv[1:(F*F),1,p] <- ( -1 * Ah.inv[,] ) %*% rowQ[,1]      ")
		x<-rbind(x, "    }                                                                ")
		
		# mexp or exp
# browser()		
		if( F>1 ) fexp <- "mexp" else if ( F== 1 ) fexp <- "exp"
		
		x<-rbind(x, "    # loop over p=1,...,P lag patterns                               ")
		x<-rbind(x, "    for(p in 1:P) {                                                  ")
		x<-rbind(x, "            # loop over t=2,...,Tp pattern-specific time point       ")
		x<-rbind(x, "            for (t in 2:Tp[p]) {                                     ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                    # autoregression matrix / drift matrix           ")
		x<-rbind(x, paste0( "                    At[1:F,1:F,t-1,p] <- ",fexp,"( A[,] * Lpat[p,t-1] )  ") )
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                    # continuous time intercepts                     ")
		if( !person.var["b"] ) {	
		x<-rbind(x, paste0( "                    bt[1:F,t-1,p] <- ( A.inv[,] %*% ( ",fexp,"( A[,] * Lpat[p,t-1] ) - I1 ) ) %*% b[,1]         ") ) }
		if( person.var["b"] ) {
		x<-rbind(x, "                    # b varies over persons, so loop over persons in specific pattern")
		x<-rbind(x, "                    # PNj        ... number of persons in pattern    ")
		x<-rbind(x, "                    # Pj (P x J) ... ragged matrix, indicators of persons in pattern")
		x<-rbind(x, "                    for (j in 1:PNj[p]) {                            ")
		x<-rbind(x, paste0( "                         bt[1:F,t-1,Pj[p,j]] <- ( A.inv[,] %*% ( ",fexp,"( A[,] * Lpat[p,t-1] ) - I1 ) ) %*% bj[,j]") )
		x<-rbind(x, "                    }                                                ") }
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                    # Qtv is vectorized Qt matrix                    ")
		x<-rbind(x, paste0( "                    #Qtv[1:(F*F),t,p] <- ifelse( t==1, ( -1 * Ah.inv[,] ) %*% rowQ[,1] , ( Ah.inv[,] %*% ( ",fexp,"( Ah[,] * Lpat[p,t-1] ) - I2 ) ) %*% rowQ[,1] )     ") )
		x<-rbind(x, paste0( "                    Qtv[1:(F*F),t,p] <- Ah.inv[,] %*% ( ",fexp,"( Ah[,] * Lpat[p,t-1] ) - I2 ) %*% rowQ[,1]     ") )
		x<-rbind(x, "            }                                                        ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    ## calculate Qt.prec                                             ")
		x<-rbind(x, "    for(p in 1:P) {                                                  ")
		x<-rbind(x, "            for (t in 1:Tp[p]) {                                     ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                    # Qt matrix (create with 'irow')                 ")
		x<-rbind(x, "                    for ( z in (1:Aw) ) {                            ")
		x<-rbind(x, "                        for ( c in (1:Aw) ) {                        ")
		x<-rbind(x, "                            Qt[z,c,t,p] <- Qtv[ c+Aw*(z-1),t,p]      ")
		x<-rbind(x, "                        }                                            ")
		x<-rbind(x, "                    }                                                ")
		x<-rbind(x, "                                                                     ")
		# inverse of Qt; Qt has dimensions of Q; Qt.replace must exist
		x<-rbind(x, make.inverse.2( "Qt", dim(Q)[1] ) )
		x<-rbind(x, "            }                                                        ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "                                                                     ")
		# inverse of A and Ah
		x<-rbind(x, make.inverse.1( "A", dim(A)[1] ) )
		x<-rbind(x, make.inverse.1( "Ah", dim(A)[1]^2 ) )
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # Kronecker of A and I1                                          ")
		x<-rbind(x, "    # https://sourceforge.net/p/mcmc-jags/discussion/610037/thread/17ddb2ce/     ")
		x<-rbind(x, "    for (i in 1:Aw){              # Aw = width of A                  ")
		x<-rbind(x, "        for (j in 1:Aw){                                             ")
		x<-rbind(x, "            for (k in 1:I1w){     # I1w = width of I1                ")
		x<-rbind(x, "                for (l in 1:I1w){                                    ")
		x<-rbind(x, "                     Ah1[(i-1)*I1w+k,(j-1)*I1w+l] <- A[i,j]*I1[k,l]  ")
		x<-rbind(x, "                }                                                    ")
		x<-rbind(x, "            }                                                        ")
		x<-rbind(x, "        }                                                            ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "    # Kronecker of I1 and A                                          ")
		x<-rbind(x, "    for (i in 1:I1w){             # I1w = width of I1                ")
		x<-rbind(x, "        for (j in 1:I1w){                                            ")
		x<-rbind(x, "            for (k in 1:Aw){      # Aw = width of A                  ")
		x<-rbind(x, "                for (l in 1:Aw){                                     ")
		x<-rbind(x, "                     Ah2[(i-1)*Aw+k,(j-1)*Aw+l] <- I1[i,j]*A[k,l]    ")
		x<-rbind(x, "                }                                                    ")
		x<-rbind(x, "            }                                                        ")
		x<-rbind(x, "        }                                                            ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "    # Ah matrix                                                      ")
		x<-rbind(x, "    Ah[1:(I1w*Aw),1:(I1w*Aw)] <- Ah1[,] + Ah2[,]                     ")
		x<-rbind(x, "                                                                     ")

# browser()		
		# additional jags syntax
		if( exists( "jags.add" ) && !is.null( jags.add ) ) {
				if ( !is.matrix( jags.add ) ) jags.add <- matrix( jags.add, ncol=1 )
				jags.add <- rbind( "## Additional/user code", jags.add )
				jags.add <- rbind( jags.add, "" )
				x<-rbind(x, jags.add)
		}

		x<-rbind(x, "}                                                                    ")
		
		# operating system
		os <- Sys.info()["sysname"]
		# parallelization yes/no
		parall <- exists("cores") && !is.null(cores) && cores > 1 && os %in% c("Windows","Linux")
		
		### call matrix (1 column)
		y<-matrix( paste0( "### R syntax for ", model.name ), 1, 1 )
		y<-rbind(y,paste0( "### engine: ", engine )	)
		y<-rbind(y, "" )
		y<-rbind(y, "# packages" )
		y<-rbind(y, "require( 'rjags' )" )
		
		if( parall ){
		y<-rbind(y, "require( 'doParallel' ) # function foreach %dopar%                   ")		
		y<-rbind(y, "require( 'abind' )      # required by ctglm                          ")		
		y<-rbind(y, "#require( 'ctglm' )      # function mcarray.chains.combine            ") }		
		y<-rbind(y, "" )
		y<-rbind(y, "# package versions" )
		y<-rbind(y, paste0( "print( installed.packages()[ installed.packages()[,1] %in% c(",paste( paste0( "'", na.omit( c('rjags' , ifelse( parall, 'ctglm', NA) ) ), "'" ) , collapse="," ),"), c(1,3) ] )" ) )
		y<-rbind(y, "" )
		
		if( !parall ){
		y<-rbind(y, "# JAGS Modules" )
		if( F>1 ) y<-rbind(y, "load.module('msm') # for matrix exponential, mexp()" )
		y<-rbind(y, "load.module('glm') # for better glm sampler        " )
		y<-rbind(y, "" ) }

		# create starting values
		# function make.priors with mode="startingvalue" is used
		if ( exists("A") && any ( is.parameter( A ) ) ) invisible( make.priors( "A", A, diag.prior = -0.5, offdiag.prior = 0, env=environment(), mode="startingvalue", verbose=FALSE ) )
# browser()		
		if ( exists("Q") && any ( is.parameter( Q ) ) ) invisible( make.priors( "Q", Q, diag.prior = 0.5, offdiag.prior = 0, env=environment(), mode="startingvalue", verbose=FALSE ) )
		if ( exists("b") && any ( is.parameter( b ) ) ) invisible( make.priors( "b", b, prior = 0, env=environment(), mode="startingvalue", verbose=FALSE ) )
		if ( exists("beta") && any ( is.parameter( beta ) ) ) invisible( make.priors( "beta", beta, prior = 0, env=environment(), mode="startingvalue", verbose=FALSE ) )
		if ( exists("mu.beta") && any ( is.parameter( mu.beta ) ) ) invisible( make.priors( "mu.beta", mu.beta, prior = 0, env=environment(), mode="startingvalue", verbose=FALSE ) )
		if ( exists("prec.beta") && any ( is.parameter( prec.beta ) ) ) invisible( make.priors( "prec.beta", prec.beta, diag.prior = 1, offdiag.prior = 0, env=environment(), mode="startingvalue", verbose=FALSE ) )
		if ( !stationarity && exists("mu.t1") && any ( is.parameter( mu.t1 ) ) ) invisible( make.priors( "mu.t1", mu.t1, prior = 0, env=environment(), mode="startingvalue", verbose=FALSE ) )
		if ( exists("prec.t1") && any ( is.parameter( prec.t1 ) ) ) {
				invisible( make.priors( "prec.t1", prec.t1, diag.prior = 1, offdiag.prior = 0,env=environment(), mode="startingvalue", verbose=FALSE ) )
				# !!!! because of dwish, complete starting value is needed, so assign upper with values of lower triangle
				prec.t1.startingvalue[upper.tri(prec.t1.startingvalue)] <- prec.t1.startingvalue[lower.tri(prec.t1.startingvalue)]
		}
		if ( exists("prec.mu.t1.j") && any ( is.parameter( prec.mu.t1.j ) ) ) {
				invisible( make.priors( "prec.mu.t1.j", prec.mu.t1.j, diag.prior = 1, offdiag.prior = 0,env=environment(), mode="startingvalue", verbose=FALSE ) )
				# !!!! because of dwish, complete starting value is needed, so assign upper with values of lower triangle
				prec.mu.t1.j.startingvalue[upper.tri(prec.mu.t1.j.startingvalue)] <- prec.mu.t1.j.startingvalue[lower.tri(prec.mu.t1.j.startingvalue)]
		}
		
		# list with starting values
		startingvalues <- list()
		do <- do.call( "c", sapply( c("A","Q","b","beta","mu.beta","prec.beta","mu.t1","prec.t1"), function(x) if (exists(paste0(x,".startingvalue"))) paste0( "startingvalues$", x ," <- ", x,".startingvalue" ) else NULL, simplify=FALSE ) )
		if ( length( do > 0 ) ) eval( parse( text=do ) )
		# push starting values to data environment
		# and create code to create as many starting value lists as chains
		# (chains are set in next step, run model)
		if ( length(startingvalues) > 0 ){
				assign( "startingvalues", startingvalues, envir=env )
				y<-rbind(y, "# starting values: create as many starting value lists as chains")
				y<-rbind(y, "startingvalues <- get( 'startingvalues', envir=data.env )")
				y<-rbind(y, "inits <- sapply( 1:chains, function (n,l) l, startingvalues, simplify=FALSE )")
				y<-rbind(y, "" )
		}
		
		# seeds
		y<-rbind(y, "# seeds for each chain")
		y<-rbind(y, "seeds <- parallel.seeds('base::BaseRNG', chains)")
		if ( length(startingvalues) > 0 ){
				y<-rbind(y, "inits <- mapply ( function ( l, k ) c(l,k), inits, seeds, SIMPLIFY=FALSE )")
		} else {
				y<-rbind(y, "inits <- seeds")
		}
		y<-rbind(y, "" )		
		
		y<-rbind(y, "# start time                                                         ")
		y<-rbind(y, "start <- Sys.time()                                                  ")
		y<-rbind(y, "" )	
# browser()
		# parallelization
		indent <- ifelse( parall, "     ", "" )
		if( parall ){
		y<-rbind(y, "## parallelization                                                   ")
		y<-rbind(y, "# create cluster                                                     ")
		if( os %in% "Windows" ){
		y<-rbind(y, paste0( "cl <- makePSOCKcluster(",cores,")                            ")) }
		if( os %in% "Linux" ){
		y<-rbind(y, paste0( "cl <- makeForkCluster(",cores,")                             ")) }		
		y<-rbind(y, "# register cluster                                                   ")
		y<-rbind(y, "registerDoParallel(cl)                                               ")
		y<-rbind(y, "" )
		y<-rbind(y, "# parallel chains                                                    ")
		y<-rbind(y, "res.l <- foreach(chain=1:chains, .packages='rjags') %dopar% {        ")
		y<-rbind(y, "" )
		y<-rbind(y, paste0( indent, "# JAGS Modules (need to be loaded here in parallel mode)" ) )
		if( F>1 ) y<-rbind(y, paste0( indent, "load.module('msm') # for matrix exponential, mexp()" ) )
		y<-rbind(y, paste0( indent, "load.module('glm') # for better glm sampler        " ) )
		y<-rbind(y, "" )
		}
		

		
		y<-rbind(y, paste0( indent, "# initialization/adaptation                                          ") )
		# globalenv !!! 
		# y<-rbind(y, "eval(parse(text=paste0(  name, ".ini <- jags.model ( file = mf , data=globalenv(), inits=sL, n.chains = ",chains,", n.adapt=",adapt,", quiet=FALSE )"  )),envir=globalenv() )" )
		# y<-rbind(y, "eval(parse(text=paste0(  name, ".ini <- jags.model ( file = mf , data=globalenv(), n.chains = ",chains,", n.adapt=",adapt,", quiet=FALSE )"  )),envir=globalenv() )" )
		# y<-rbind(y, "eval(parse(text=paste0(  model.name, .ini <- jags.model ( file = bugs.file , data=data.env, n.chains=chains, n.adapt=adapt, quiet=FALSE ) )),envir=data.env )" )
		# y<-rbind(y, "eval(parse(text=paste0(  model.name, .ini <- jags.model ( file = bugs.file , data=data.env, n.chains=chains, n.adapt=adapt, quiet=FALSE ) )),envir=globalenv())" )
		y<-rbind(y, paste0( indent, "ini <- jags.model ( file = bugs.file , data=data.env, n.chains=",ifelse(parall,"1","chains"),", n.adapt=adapt, inits=inits",ifelse(parall,"[chain]",""),", quiet=FALSE )" ) )
		y<-rbind(y, "" )
		y<-rbind(y, paste0( indent, "# run                                                                ") )
		# y<-rbind(y, "eval(parse(text=paste0(  model.name, .res <- jags.samples ( ',model.name,'.ini , variable.names=c('A','Q','b'), n.iter=iter, thin=thin, type='trace' , progress.bar = 'none', by=20 )' )), envir=globalenv() )" )
# browser()

		## add or delete person par
		if( exists( "track.person.par" ) && "theta" %in% track.person.par ) {
				# theta is not regularly created (makes no sense, since always estimated, never set)
				# so create a structure for theta for further processing
				thetas.dfr <- Reduce(function(x, y) merge(x, y, all=TRUE),list(data.frame("J"=1:J),data.frame("F"=1:F),data.frame("T"=1:T)),accumulate=FALSE )
				thetas <- apply( thetas.dfr, 1, function(z) paste0( "theta_", paste( z, collapse="_" ) ) )
				theta. <- array( thetas, dim=c(J,F,T) )
				assign( "theta", theta., envir=par.env )
		}
		if( !exists( "track.person.par" ) || !"bj" %in% track.person.par ) {
				# bj is regularly created, if it should not be tracked, delete it from par.env
				if ( exists( "bj", envir=par.env ) ) rm( "bj", envir=par.env )
		}		
		if( !exists( "track.person.par" ) || !"mu.t1.j" %in% track.person.par ) {
				if ( exists( "mu.t1.j", envir=par.env ) ) rm( "mu.t1.j", envir=par.env )
		}		

		# mod7: delete code in parameters
		invisible( make.matrices.consistent( par.env , mods=7 ) )
		
		# all parameters from par.env must be tracked
		pars <- ls(envir=par.env)
# browser()		
		# mix in track.include / track.exclude 
		if ( exists( "track.include" ) && is.list( track.include ) && length( track.include ) > 0 ) {
				# include in tracking
				pars <- unique( c( pars, names( track.include ) ) )
				# for automatic results retrieval structure of objects need to be known
				# if not user set (i.e. NULL) it is assumed as a scalar
				# in this case there is one parameter of the same name
				if( any( isnull <- sapply( track.include, is.null ) ) ) track.include[ isnull ] <- names( track.include[ isnull ] )
				# put into par.env
				eval( parse( text=paste0( "assign( '", names(track.include), "' , track.include$'", names(track.include), "' , envir=par.env )" ) ) )
		}
		if ( exists( "track.exclude" ) && is.character( track.exclude ) && length( track.exclude ) > 0 ) {
				# exclude from tracking
				if ( any ( del <- pars %in% track.exclude ) ) pars <- pars[ !del ]
				# also delete from par.env (ctglm.results depends on clean par.env)
				if ( any ( del <- ls(envir=par.env) %in% track.exclude ) ) rm( list=ls(envir=par.env)[del], envir=par.env ) 
		}
		
		# sortieren
		### ist glaub ich ziemlich sinnlos, da jags eh nicht die Reihenfolge hat dann
		ord <- match( c("A","Q","b"), pars )
		ord <- ord[!is.na(ord)]
		if( !identical( ord, integer(0) ) ) pars <- c( pars[ord], pars[ !pars %in% pars[ord] ] )
		par.string <- paste0( "c(", paste( paste0("'", pars, "'"), collapse="," ) ,")" )
		
		y<-rbind(y, paste0( indent, "res <- jags.samples ( ini, variable.names=",par.string,", n.iter=iter, thin=thin, type='trace' , progress.bar = 'text', by=20 )" ) )
		
		# parallelization, get results
		if( parall ){
		y<-rbind(y, "" )
		y<-rbind(y, "}" )
		y<-rbind(y, "" )
		y<-rbind(y, "# stop cluster" )
		y<-rbind(y, "stopCluster(cl)" )
		y<-rbind(y, "" )
		y<-rbind(y, "# combine results from parallel computing" )
		y<-rbind(y, "res <- mcarray.chains.combine( res.l )" ) }
		
		y<-rbind(y, "" )
		y<-rbind(y, "# run time                                                           ")		
		y<-rbind(y, "runtime <- Sys.time() - start                                        ")		
		
		### (over)write relevant variables to environment ###
		# jags.syntax <- x
		# obj <- c( "jags.syntax" )
		# eval( parse ( text=paste0( "assign( '",obj, "' , get('",obj,"') , envir=env )" ) ) )
		
		## create return object
		ret <- list()
		# first entry: engine
		ret$engine <- engine
		# second entry: model.name
		ret$model.name <- model.name
		# third entry: data environment
		ret$data.env <- env
		# fourth entry: environment with original matrices
		ret$par.env <- par.env
		# fifth entry: call
		ret$call <- y
		# sixth entry: syntax
		ret$syntax <- x
		
		
		# return
		return( ret )
}

make.str <- function( y.name ) {
# browser()		
		# environment where objects are
		env <- parent.frame()
		
		# dimensions of structure, get from parent env
		dim.y <- eval( parse( text=paste0( "dim(", y.name, ")" ) ), envir=env )

		
		# if structure without dimensions (e.g. vector)
		if( is.null( dim.y ) ) {
				
				# long structure
				y. <- data.frame( matrix( seq( along= eval( parse( text=paste0( "", y.name, "" ) ), envir=env ) ), ncol=1 ) )
				
		} else {
		# if structure with dimensions (e.g. matrix)
		
				# long structure
				y. <- eval(parse(text=paste0( "Reduce(function(x, y) merge(x, y, by=NULL, all=TRUE),list(", paste( paste0( "1:", dim.y ), collapse="," ), "),accumulate=FALSE )" )))
				
		}
		y.$par <- apply( y., 1, function ( z ) eval( parse( text= paste0( y.name,"[", paste(z,collapse=","), "]" ) ), envir=env ) )
# browser()
		# sort out code, keep for later adding
		if ( any ( is.code( y.$par ) ) ){
				code <- sub( "^[^;]*;(.*)$", "\\1", y.$par[ is.code( y.$par ) ] )
				add.code <- matrix( paste0( "    ", gsub( "^\\s*", "", do.call( "c", strsplit(code,";") ) ) ), ncol=1 )
				y.$par[ is.code( y.$par ) ] <- sub( "^([^;]*);.*$", "\\1", y.$par[ is.code( y.$par ) ] )
		} else {
				add.code <- NULL
		}
# browser()		
		
		# tag duplicated free parameters and generate values to set duplicated
		y.$dupl <- as.integer( duplicated(y.$par) & is.na(suppressWarnings(as.numeric(y.$par)))  )
		dupl <- unique( y.$par[ duplicated( y.$par ) & is.na(suppressWarnings(as.numeric(y.$par))) ] )
		if ( length( dupl ) > 0 ) {
				dl.l <- sapply( dupl, function( dl ){
						y.1 <- y.[ y.$par %in% dupl, ]
						y.1 <- y.1[ !duplicated( y.1$par ), ]
						y.1$val <- apply( y.1, 1, function ( z ) paste0( y.name, "", "[", paste( z[-c(length(z)-1,length(z))], collapse=","), "]" ) )
						return( y.1 )
				}, simplify=FALSE )
				dl <- do.call( "rbind", dl.l )
				y.cn <- colnames( y. )
				y.$order <- seq(along=rownames(y.))
				y. <- merge( y., dl[,c("par","val")], by="par", sort=FALSE, all.x=TRUE )
				y. <- y.[ order( y.$order), ]
				y.$order <- NULL
				y. <- y.[ , c( y.cn, "val" ), drop=FALSE ]
		} else {
				y.$val <- ""
		}

# if( any( grepl( "Q", y.$par ) ) ) browser()	
		
		# loop over long structure, generate strings
		make.str2 <- function( z ) {
# browser()
# if( grepl( "Q", z["par"] ) ) browser()
				dupl <- z[ length(z)-1 ]
				val <- z[ length(z) ]
				par <- z["par"]
				z <- z[ -c(length(z)-2,length(z)-1,length(z)) ]
				
				if ( dupl %in% "0" ){
# if( grepl( "Q", par ) ) browser()			
						# name of parameter
						nam <- paste0( y.name, "[", paste( z, collapse=","), "]" )
			
						# value of parameter
						par.val <- eval( parse( text=nam ), envir=env )
						
						# value of prior
						p.name <- paste0( y.name, ".prior" )
						if ( exists( p.name, envir=env) ){
								p.val <- eval( parse( text=paste0( p.name, "[", paste( z, collapse=","), "]" ) ), envir=env )
						} else {
								p.val <- NA
						}
						is.prior <- !is.na( p.val )
						
						# determine if fixed value or freely estimated
						# as.num <- suppressWarnings( as.numeric( eval( parse( text=nam ), envir=env ) ) )
						# is.fixed <- !is.na( as.num )
						
						# is fixed value or a parameter
						
		# browser()				
						# operator: <- (for fixed oder parameter) or ~ (for prior)
						# op <- ifelse( is.parameter( par.val ) | is.fixed( par.val ), " <- ", " ~ " )
						op <- ifelse( is.prior, " ~ ", " <- " )
						# if fixed, uncomment in jags syntax, because already set in R
						# com <- ifelse( is.fixed, "# ", "" )
						# com <- ""
# browser()						
						# value to be set
						set.val <- "TODO"
						# parameter
						if( is.parameter( par.val ) ) set.val <- par
						# fixed value (numeric)
						if( !is.na( suppressWarnings( as.numeric( par ) ) ) ) set.val <- as.numeric( par ) 
						# prior
						if( is.prior ) set.val <- p.val
	
						# val <- ifelse( is.fixed, as.character( as.num ), eval( parse( text=paste0( y.name, ".prior", "[", paste( z, collapse=","), "]" ) ), envir=env ) )
						# val <- ifelse( is.fixed, "", eval( parse( text=paste0( y.name, ".prior", "[", paste( z, collapse=","), "]" ) ), envir=env ) )
						# if( is.parameter( par.val ) ) set.val <- par 
						# if prior exists dann prior setzen
		
	
						# get("Q",envir=env)
						# get("Q.prior",envir=env)
						
						# string
						# s <- paste0( "    ", com, nam, op, set.val )
						s <- paste0( "    ",  nam, op, set.val )
				
				} else {
						
						s <- paste0( "    ",y.name, "", "[", paste( z, collapse=","), "] <- ", val )
						
				}
				
				# return
				return( s )
		}
# browser()
		# s <- apply( y.1[,-ncol(y.1),drop=FALSE], 1, make.str2 )
		s <- apply( y., 1, make.str2 )

		# append some white space (for optical reasons)
		s <- sapply( s, function( x ) paste0( x, paste( rep( " ", 69 - nchar(x) ), collapse="" ) ) )
		
		# as column vector
		s <- matrix( s, ncol=1 )

		# add.code
		if ( !is.null( add.code ) ){
				s <- rbind( s, add.code )
		}
		
		# return
		return( s )

}

moveTo.par.env <- function( name, env, par.env ){
		if( exists(name,envir=env) ){
				assign( name, get( name, envir=env ), envir=par.env )
				rm( list=name, envir=env )
		}
		TRUE
}


make.inverse.1 <- function( name, dim ){
		
		x<-matrix( "", 1, 1 )
		x<-rbind(x, paste0( "    ## inverse of ",name,"                                           " ) )		
	
		if (dim %in% 1) {
				x<-rbind(x, paste0( "    ",name,".inv[1,1] <- (1/",name,"[1,1])" ) )
		}
		
		if (dim %in% 2) {
				x<-rbind(x, paste0( "    # determinant of ",name,"                                        " ) )
				x<-rbind(x, paste0( "    ",name,".det <- ",name,"[1,1]*",name,"[2,2] - ",name,"[2,1]*",name,"[1,2] " ) )
				x<-rbind(x, paste0( "    # ",name," is invertible if determinant is not 0                          " ) )
				x<-rbind(x, paste0( "    ",name,".invertible <- ifelse( ",name,".det==0, 0, 1 )           " ) )
				x<-rbind(x, paste0( "    # if not invertible, modest mod of determinant                   " ) )
				x<-rbind(x, paste0( "    ",name,".detmod1 ~ dbern(0.5)                                    " ) )
				x<-rbind(x, paste0( "    ",name,".detmod2 <- ifelse( ",name,".invertible==1, 0, (",name,".detmod1-0.5)/500 )     " ) )
				x<-rbind(x, paste0( "    ",name,".det.adj <- ",name,".det + ",name,".detmod2                                     " ) )
				x<-rbind(x, paste0( "    # inverse of ",name,"                                                          " ) )
				x<-rbind(x, paste0( "    ",name,".help[1,1] <- ",name,"[2,2]                                            " ) )
				x<-rbind(x, paste0( "    ",name,".help[2,2] <- ",name,"[1,1]                                            " ) )
				x<-rbind(x, paste0( "    ",name,".help[1,2] <- -1*",name,"[1,2]                                         " ) )
				x<-rbind(x, paste0( "    ",name,".help[2,1] <- -1*",name,"[2,1]                                         " ) )
				x<-rbind(x, paste0( "    ",name,".inv[1:",dim,",1:",dim,"] <- (1/",name,".det.adj * ",name,".help[,])             " ) )
		}
		if (dim %in% 4) {
				x<-rbind(x, paste0( "    # http://stackoverflow.com/questions/1148309/inverting-a-4x4-matrix                                                                                                                                                                                                                       ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[1,1] <- ",name,"[2,2]*",name,"[3,3]*",name,"[4,4]-",name,"[2,2]*",name,"[3,4]*",name,"[4,3]-",name,"[3,2]*",name,"[2,3]*",name,"[4,4]+",name,"[3,2]*",name,"[2,4]*",name,"[4,3]+",name,"[4,2]*",name,"[2,3]*",name,"[3,4]-",name,"[4,2]*",name,"[2,4]*",name,"[3,3]     ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[2,1] <- -",name,"[2,1]*",name,"[3,3]*",name,"[4,4]+",name,"[2,1]*",name,"[3,4]*",name,"[4,3]+",name,"[3,1]*",name,"[2,3]*",name,"[4,4]-",name,"[3,1]*",name,"[2,4]*",name,"[4,3]-",name,"[4,1]*",name,"[2,3]*",name,"[3,4]+",name,"[4,1]*",name,"[2,4]*",name,"[3,3]    ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[3,1] <- ",name,"[2,1]*",name,"[3,2]*",name,"[4,4]-",name,"[2,1]*",name,"[3,4]*",name,"[4,2]-",name,"[3,1]*",name,"[2,2]*",name,"[4,4]+",name,"[3,1]*",name,"[2,4]*",name,"[4,2]+",name,"[4,1]*",name,"[2,2]*",name,"[3,4]-",name,"[4,1]*",name,"[2,4]*",name,"[3,2]     ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[4,1] <- -",name,"[2,1]*",name,"[3,2]*",name,"[4,3]+",name,"[2,1]*",name,"[3,3]*",name,"[4,2]+",name,"[3,1]*",name,"[2,2]*",name,"[4,3]-",name,"[3,1]*",name,"[2,3]*",name,"[4,2]-",name,"[4,1]*",name,"[2,2]*",name,"[3,3]+",name,"[4,1]*",name,"[2,3]*",name,"[3,2]    ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[1,2] <- -",name,"[1,2]*",name,"[3,3]*",name,"[4,4]+",name,"[1,2]*",name,"[3,4]*",name,"[4,3]+",name,"[3,2]*",name,"[1,3]*",name,"[4,4]-",name,"[3,2]*",name,"[1,4]*",name,"[4,3]-",name,"[4,2]*",name,"[1,3]*",name,"[3,4]+",name,"[4,2]*",name,"[1,4]*",name,"[3,3]    ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[2,2] <- ",name,"[1,1]*",name,"[3,3]*",name,"[4,4]-",name,"[1,1]*",name,"[3,4]*",name,"[4,3]-",name,"[3,1]*",name,"[1,3]*",name,"[4,4]+",name,"[3,1]*",name,"[1,4]*",name,"[4,3]+",name,"[4,1]*",name,"[1,3]*",name,"[3,4]-",name,"[4,1]*",name,"[1,4]*",name,"[3,3]     ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[3,2] <- -",name,"[1,1]*",name,"[3,2]*",name,"[4,4]+",name,"[1,1]*",name,"[3,4]*",name,"[4,2]+",name,"[3,1]*",name,"[1,2]*",name,"[4,4]-",name,"[3,1]*",name,"[1,4]*",name,"[4,2]-",name,"[4,1]*",name,"[1,2]*",name,"[3,4]+",name,"[4,1]*",name,"[1,4]*",name,"[3,2]    ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[4,2] <- ",name,"[1,1]*",name,"[3,2]*",name,"[4,3]-",name,"[1,1]*",name,"[3,3]*",name,"[4,2]-",name,"[3,1]*",name,"[1,2]*",name,"[4,3]+",name,"[3,1]*",name,"[1,3]*",name,"[4,2]+",name,"[4,1]*",name,"[1,2]*",name,"[3,3]-",name,"[4,1]*",name,"[1,3]*",name,"[3,2]     ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[1,3] <- ",name,"[1,2]*",name,"[2,3]*",name,"[4,4]-",name,"[1,2]*",name,"[2,4]*",name,"[4,3]-",name,"[2,2]*",name,"[1,3]*",name,"[4,4]+",name,"[2,2]*",name,"[1,4]*",name,"[4,3]+",name,"[4,2]*",name,"[1,3]*",name,"[2,4]-",name,"[4,2]*",name,"[1,4]*",name,"[2,3]     ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[2,3] <- -",name,"[1,1]*",name,"[2,3]*",name,"[4,4]+",name,"[1,1]*",name,"[2,4]*",name,"[4,3]+",name,"[2,1]*",name,"[1,3]*",name,"[4,4]-",name,"[2,1]*",name,"[1,4]*",name,"[4,3]-",name,"[4,1]*",name,"[1,3]*",name,"[2,4]+",name,"[4,1]*",name,"[1,4]*",name,"[2,3]    ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[3,3] <- ",name,"[1,1]*",name,"[2,2]*",name,"[4,4]-",name,"[1,1]*",name,"[2,4]*",name,"[4,2]-",name,"[2,1]*",name,"[1,2]*",name,"[4,4]+",name,"[2,1]*",name,"[1,4]*",name,"[4,2]+",name,"[4,1]*",name,"[1,2]*",name,"[2,4]-",name,"[4,1]*",name,"[1,4]*",name,"[2,2]     ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[4,3] <- -",name,"[1,1]*",name,"[2,2]*",name,"[4,3]+",name,"[1,1]*",name,"[2,3]*",name,"[4,2]+",name,"[2,1]*",name,"[1,2]*",name,"[4,3]-",name,"[2,1]*",name,"[1,3]*",name,"[4,2]-",name,"[4,1]*",name,"[1,2]*",name,"[2,3]+",name,"[4,1]*",name,"[1,3]*",name,"[2,2]    ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[1,4] <- -",name,"[1,2]*",name,"[2,3]*",name,"[3,4]+",name,"[1,2]*",name,"[2,4]*",name,"[3,3]+",name,"[2,2]*",name,"[1,3]*",name,"[3,4]-",name,"[2,2]*",name,"[1,4]*",name,"[3,3]-",name,"[3,2]*",name,"[1,3]*",name,"[2,4]+",name,"[3,2]*",name,"[1,4]*",name,"[2,3]    ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[2,4] <- ",name,"[1,1]*",name,"[2,3]*",name,"[3,4]-",name,"[1,1]*",name,"[2,4]*",name,"[3,3]-",name,"[2,1]*",name,"[1,3]*",name,"[3,4]+",name,"[2,1]*",name,"[1,4]*",name,"[3,3]+",name,"[3,1]*",name,"[1,3]*",name,"[2,4]-",name,"[3,1]*",name,"[1,4]*",name,"[2,3]     ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[3,4] <- -",name,"[1,1]*",name,"[2,2]*",name,"[3,4]+",name,"[1,1]*",name,"[2,4]*",name,"[3,2]+",name,"[2,1]*",name,"[1,2]*",name,"[3,4]-",name,"[2,1]*",name,"[1,4]*",name,"[3,2]-",name,"[3,1]*",name,"[1,2]*",name,"[2,4]+",name,"[3,1]*",name,"[1,4]*",name,"[2,2]    ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[4,4] <- ",name,"[1,1]*",name,"[2,2]*",name,"[3,3]-",name,"[1,1]*",name,"[2,3]*",name,"[3,2]-",name,"[2,1]*",name,"[1,2]*",name,"[3,3]+",name,"[2,1]*",name,"[1,3]*",name,"[3,2]+",name,"[3,1]*",name,"[1,2]*",name,"[2,3]-",name,"[3,1]*",name,"[1,3]*",name,"[2,2]     ") )
				x<-rbind(x, paste0( "    ",name,".det <- ",name,"[1,1]*",name,".inv.help[1,1]+",name,"[1,2]*",name,".inv.help[2,1]+",name,"[1,3]*",name,".inv.help[3,1]+",name,"[1,4]*",name,".inv.help[4,1]                                                                                                                       ") )
				x<-rbind(x, paste0( "    # ",name," is invertible if determinant is not 0                                   ") )
				x<-rbind(x, paste0( "    ",name,".invertible <- ifelse( ",name,".det==0, 0, 1 )                             ") )
				x<-rbind(x, paste0( "    # if not invertible, modest mod of determinant                                     ") )
				x<-rbind(x, paste0( "    ",name,".detmod1 ~ dbern(0.5)                                                      ") )
				x<-rbind(x, paste0( "    ",name,".detmod2 <- ifelse( ",name,".invertible==1, 0, (",name,".detmod1-0.5)/500 )") )
				x<-rbind(x, paste0( "    ",name,".det.adj <- ",name,".det + ",name,".detmod2                                ") )
				x<-rbind(x, paste0( "    ",name,".inv <- ",name,".inv.help / ",name,".det.adj                               ") )
		}
		
		x<-rbind(x,"")
		return( x )
}

make.inverse.2 <- function( name, dim ){
		
		### *.prec.replace must exist (for dim>1)
		
		x<-matrix( "", 1, 1 )
		
		if (dim %in% 1) {
				if (name %in% "Qt"){
						x<-rbind(x, "                    # inverse of Qt                                 ")						
						x<-rbind(x, "                    Qt.prec[1,1,t,p] <- (1/Qt[1,1,t,p])              ")
				}
		}
		
		if (dim %in% 2) {
				if (name %in% "Qt"){
						x<-rbind(x, "                    ## inverse of Qt                                 ")
						x<-rbind(x, "                    # determinant of Qt                              ")
						x<-rbind(x, "                    Qt.det[t,p] <- Qt[1,1,t,p]*Qt[2,2,t,p] - Qt[2,1,t,p]*Qt[1,2,t,p]     ")
						x<-rbind(x, "                    # Qt is invertible if determinant is not 0       ")
						x<-rbind(x, "                    Qt.invertible[t,p] <- ifelse( Qt.det[t,p]==0, 0, 1 )     ")
						x<-rbind(x, "                    # if not invertible, modest mod of determinant   ")
						x<-rbind(x, "                    Qt.detmod1[t,p] ~ dbern(0.5)                        ")
						x<-rbind(x, "                    Qt.detmod2[t,p] <- ifelse( Qt.invertible[t,p]==1, 0, (Qt.detmod1[t,p]-0.5)/500 )     ")
						x<-rbind(x, "                    Qt.det.adj[t,p] <- Qt.det[t,p] + Qt.detmod2[t,p]    ")
						x<-rbind(x, "                    # inverse of Qt                                  ")
						x<-rbind(x, "                    Qt.help[1,1,t,p] <- Qt[2,2,t,p]                  ")
						x<-rbind(x, "                    Qt.help[2,2,t,p] <- Qt[1,1,t,p]                  ")
						x<-rbind(x, "                    Qt.help[1,2,t,p] <- -1*Qt[1,2,t,p]               ")
						x<-rbind(x, "                    Qt.help[2,1,t,p] <- -1*Qt[2,1,t,p]               ")
						x<-rbind(x, "                    Qt.prec.orig[1:Aw,1:Aw,t,p] <- (1/Qt.det.adj[t,p] * Qt.help[,,t,p])     ")
						x<-rbind(x, "                                                                     ")
						x<-rbind(x, "                    # check if Qt.prec.orig is positive definite          ")
						x<-rbind(x, "                    Qt.prec.orig.det[t,p] <- Qt.prec.orig[1,1,t,p]*Qt.prec.orig[2,2,t,p] - Qt.prec.orig[2,1,t,p]*Qt.prec.orig[1,2,t,p]     ")
						x<-rbind(x, "                    Qt.prec.orig.posdef[t,p] <- ifelse(  Qt.prec.orig[1,1,t,p] > 0 && Qt.prec.orig.det[t,p] > 0, 1, 0 )     ")
						x<-rbind(x, "                                                                     ")
						x<-rbind(x, "                    # if Qt.prec.orig is not pos def then replace by a pos def matrix     ")
						x<-rbind(x, "                    Qt.prec[1:2,1:2,t,p] <- ifelse( Qt.prec.orig.posdef[t,p]==1, Qt.prec.orig[,,t,p] , Qt.prec.replace[,]  )     ")
				}
		}
		
		x<-rbind(x,"")
		return( x )
}

