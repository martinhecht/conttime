
create.ctstan.ctsem.syntax <- function ( env, mode ) {
# browser()
		# get variables from env
		eval( parse( text=paste0( "assign( '",ls(envir=env), "' , get('",ls(envir=env),"', envir=env ) )" ) ) )
		
		### call matrix (1 column)
		y<-matrix( paste0( "### R syntax for ", model.name ), 1, 1 )
		y<-rbind(y,paste0( "### engine: ", engine ) )
		y<-rbind(y, "" )
		y<-rbind(y, "# packages" )
		y<-rbind(y, "require( 'ctsem' )" )
		if( mode %in% "ctstan" ) {
		y<-rbind(y, "require( 'rstan' )" )
		y<-rbind(y, "rstan_options(auto_write = TRUE)" )
		y<-rbind(y, paste0( "# options(mc.cores = ",cores,") # cores is set in ctStanFit " )) }
		y<-rbind(y, "" )
		y<-rbind(y, "# package versions" )
		y<-rbind(y, "print( installed.packages()[ installed.packages()[,1] %in% c('ctsem','rstan'), c(1,3) ] )" )
		y<-rbind(y, "" )
		### modifications, must also be incorporated in results.ctsem and/or results.jags.ctstan
		y<-rbind(y, "## modifications" )
		y<-rbind(y, "# no . in parameter names" )	
		y<-rbind(y, 'eval( parse( text=paste0( "mu.t1[",1:length(mu.t1),"] <- gsub( \'.\', \'\', mu.t1[",1:length(mu.t1),"], fixed=TRUE  )" ) ) ) ')
		if( mode %in% c("ctstan","ctsem") ) {
		y<-rbind(y, 'eval( parse( text=paste0( "chol.var.t1[",1:length(chol.var.t1),"] <- gsub( \'.\', \'\', chol.var.t1[",1:length(chol.var.t1),"], fixed=TRUE  )" ) ) ) ') }
		if( mode %in% c("ctsem") && person.var["b"] ) {
		y<-rbind(y, 'eval( parse( text=paste0( "chol.var.b[",1:length(chol.var.b),"] <- gsub( \'.\', \'\', chol.var.b[",1:length(chol.var.b),"], fixed=TRUE  )" ) ) ) ') }
		# if( mode %in% "ctsem" ) {
		# y<-rbind(y, 'eval( parse( text=paste0( "var.t1[",1:length(var.t1),"] <- gsub( \'.\', \'\', var.t1[",1:length(var.t1),"], fixed=TRUE  )" ) ) ) ') }
		if( mode %in% "ctsem" && F==1 ) {
		y<-rbind(y, "# object name must not be equal to parameter name" ) 
		y<-rbind(y, 'pars <- c("A","cholQ","b","Lambda","beta",ifelse(exists("E"),"E",NA),ifelse(exists("chol.var.t1"),"chol.var.t1",NA),ifelse(exists("chol.var.b"),"chol.var.b",NA),ifelse(exists("mu.t1"),"mu.t1",NA))' )
		y<-rbind(y, "pars <- pars[!is.na(pars)]" )
		y<-rbind(y, 'eval( parse( text=paste0( pars, "2 <- ",pars,"; ",pars,"2[ is.na(suppressWarnings(as.numeric(",pars,"2))) ] <- paste0( ",pars,"[ is.na(suppressWarnings(as.numeric(",pars,"))) ] , \'_\' ); ",pars," <- ",pars,"2") ) ) ') }
		# y<-rbind(y, "# T0VAR lower triangular (Note: T0VAR needs to be cholesky decomposed matrix)" )
		# y<-rbind(y, "prec.t1[upper.tri(prec.t1)] <- 0" )
		# y<-rbind(y, "# DIFFUSION lower triangular (Note: Q needs to be cholesky decomposed matrix)" )
		# y<-rbind(y, "Q[upper.tri(Q)] <- 0" )
		y<-rbind(y, "" )			
		
		y<-rbind(y, paste0( "# ",mode," model                                      " ) )
        y<-rbind(y, paste0( "m <- ctModel( Tpoints=T,                              " ) )
        y<-rbind(y, paste0( "              n.latent=F,                             " ) )
        y<-rbind(y, paste0( "              n.manifest=I,                           " ) )
        if( measurement.model$family %in% "gaussian" ) {
        y<-rbind(y, paste0( "              MANIFESTVAR=E,                          " ) ) }
        if( measurement.model$family %in% "binomial" ) {
        y<-rbind(y, paste0( "              MANIFESTVAR=t(chol(diag(10^-20,I))),    " ) ) }
        y<-rbind(y, paste0( "              MANIFESTMEANS=beta,                     " ) )
        y<-rbind(y, paste0( "              LAMBDA=Lambda,                          " ) )
        y<-rbind(y, paste0( "              DRIFT=A,                                " ) )
        if( mode %in% c("ctstan","ctsem") ) {
		y<-rbind(y, paste0( "              DIFFUSION=cholQ,                        " ) ) } 
		# if( mode %in% "ctsem" ) {
		# y<-rbind(y, paste0( "              DIFFUSION=Q,                            " ) ) }
        y<-rbind(y, paste0( "              CINT=b,                                 " ) )
        y<-rbind(y, paste0( "              T0MEANS=matrix(mu.t1,ncol=1),           " ) ) 
        if( mode %in% c("ctstan","ctsem") ) {
		y<-rbind(y, paste0( "              T0VAR=chol.var.t1,                      " ) ) }
        # if( mode %in% "ctsem" ) {
		# y<-rbind(y, paste0( "              T0VAR=var.t1,                           " ) ) }
# browser()			
        # no traitvar for ctstan (Email Charlie 14.11.16)
		# if( mode %in% c("ctstan","ctsem") ) {
		if( mode %in% c("ctsem") ) {
		y<-rbind(y, paste0( "              TRAITVAR=chol.var.b,                   " ) ) }
		
		if( mode %in% "ctsem" ) {
		y<-rbind(y, paste0( "              type='omx'                              " ) ) }
		if( mode %in% "ctstan" ) {
		y<-rbind(y, paste0( "              type='stanct'                           " ) ) }
		y<-rbind(y, paste0( "            )                                         " ) )
		y<-rbind(y, "" )
# browser()		
		if( mode %in% "ctstan" ) {
		y<-rbind(y, "# individually varying parameters" )
### !!!ab spaetestens ab 24.3.17 heissts nicht mehr parameters, sondern pars 		
		y<-rbind(y, "m$pars$indvarying <- FALSE" )
		if( person.var["b"] ) { 
				y<-rbind(y, "m$pars$indvarying[ m$pars$matrix %in% c('CINT') ] <- TRUE" )
				}
		if( person.var["mu.t1"] ) { 
				y<-rbind(y, "m$pars$indvarying[ m$pars$matrix %in% c('T0MEANS') ] <- TRUE" )
				}
		y<-rbind(y, "" ) }
		
		# m2$parameters$indvarying <- FALSE
		# m2$parameters$indvarying[ m2$parameters$matrix %in% c("T0MEANS") ] <- TRUE
		# m2$parameters$indvarying[ m2$parameters$matrix %in% c("T0MEANS","CINT") ] <- TRUE
		# m2$parameters$indvarying[ m2$parameters$matrix %in% c("CINT") ] <- TRUE

		y<-rbind(y, "# start time                                                         ")
		y<-rbind(y, "start <- Sys.time()                                                  ")
		y<-rbind(y, "" )	

        y<-rbind(y, paste0( "# run model                                                           ") )		
        if( mode %in% "ctsem" ) {
		y<-rbind(y, paste0( "r <- ctFit( datawide=dw,                                              ") ) 
        y<-rbind(y, paste0( "            ctmodelobj=m                                              ") )		
        y<-rbind(y, paste0( "          )                                                           ") ) }		

### TODO in naechster Version: cores        
		if( mode %in% "ctstan" ) {
		y<-rbind(y, paste0( "r <- ctStanFit( datalong=d,                                           ") ) 
        # y<-rbind(y, paste0( "                ctstanmodelobj=m,                                     ") ) 
        y<-rbind(y, paste0( "                ctstanmodel=m,                                     ") ) 
        y<-rbind(y, paste0( "                iter=iter,                                            ") )  
        y<-rbind(y, paste0( "                plot=FALSE,                                           ") )   
        y<-rbind(y, paste0( "                chains=chains,                                        ") ) 
        y<-rbind(y, paste0( "                fit=TRUE,                                             ") )    
        y<-rbind(y, paste0( "                kalman=FALSE,                                         ") )    
        y<-rbind(y, paste0( "                stationary=FALSE,                                     ") )    
        y<-rbind(y, paste0( "                cores=",cores,",                                     ") )  
        # y<-rbind(y, paste0( "                noncentered=TRUE,                                     ") )  
        if( measurement.model$family %in% "gaussian" ) {          
        y<-rbind(y, paste0( "                binomial=FALSE                                        ") ) }
        if( measurement.model$family %in% "binomial" ) {          
        y<-rbind(y, paste0( "                binomial=TRUE                                         ") ) }
		y<-rbind(y, paste0( "              )                                                       ") ) }
		y<-rbind(y, "" )	
		
		y<-rbind(y, "# run time                                                           ")		
		y<-rbind(y, "runtime <- Sys.time() - start                                        ")			
		
		# for consistency with jags implementation, put all matrices with free parameters to par.env
		par.env <- new.env()		
		if( exists( "E" ) && any( is.parameter ( E ) ) ) invisible(moveTo.par.env("E",env,par.env))
		if( exists( "beta" ) && any( is.parameter ( beta ) ) ) invisible(moveTo.par.env("beta",env,par.env))
		# mu.beta/prec.beta not implemented in ctsem
		#if( exists( "mu.beta" ) && any( is.parameter ( mu.beta ) ) ) invisible(moveTo.par.env("mu.beta",env,par.env))
		#if( exists( "prec.beta" ) && any( is.parameter ( prec.beta ) ) ) invisible(moveTo.par.env("prec.beta",env,par.env))
		if( exists( "Lambda" ) && any( is.parameter ( Lambda ) ) ) invisible(moveTo.par.env("Lambda",env,par.env))
		if( exists( "mu.t1" ) && any( is.parameter ( mu.t1 ) ) ) invisible(moveTo.par.env("mu.t1",env,par.env))
		if( exists( "prec.t1" ) && any( is.parameter ( prec.t1 ) ) ) invisible(moveTo.par.env("prec.t1",env,par.env))
		if( exists( "chol.var.t1" ) && any( is.parameter ( chol.var.t1 ) ) ) invisible(moveTo.par.env("chol.var.t1",env,par.env))
		if( exists( "chol.var.b" ) && any( is.parameter ( chol.var.b ) ) ) invisible(moveTo.par.env("chol.var.b",env,par.env))
		# if( exists( "var.t1" ) && any( is.parameter ( var.t1 ) ) ) invisible(moveTo.par.env("var.t1",env,par.env))
		if( exists( "A" ) && any( is.parameter ( A ) ) ) invisible(moveTo.par.env("A",env,par.env))
		if( exists( "Q" ) && any( is.parameter ( Q ) ) ) invisible(moveTo.par.env("Q",env,par.env))
		if( exists( "cholQ" ) && any( is.parameter ( cholQ ) ) ) invisible(moveTo.par.env("cholQ",env,par.env))
		if( exists( "b" ) && any( is.parameter ( b ) ) ) invisible(moveTo.par.env("b",env,par.env))
		if( exists( "sd.b" ) && any( is.parameter ( sd.b ) ) ) invisible(moveTo.par.env("sd.b",env,par.env))
# browser()		
		if( exists( "track.person.par" ) && !is.null( track.person.par ) && mode %in% "ctstan" && exists( "bj" ) && any( is.parameter ( bj ) ) && "bj" %in% track.person.par ) invisible(moveTo.par.env("bj",env,par.env))
		if( exists( "track.person.par" ) && !is.null( track.person.par ) && mode %in% "ctstan" && exists( "mu.t1.j" ) && any( is.parameter ( mu.t1.j ) ) && "mu.t1.j" %in% track.person.par ) invisible(moveTo.par.env("mu.t1.j",env,par.env))
		
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
		
		# return
		return( ret )
}

