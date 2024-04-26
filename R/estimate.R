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
			# str.env <- gen.empty.structures(I=4, N=5, T=3)
			str.env <- gen.empty.structures(I=4, N=100, T=20)
			# data.env <- gen.data( env=str.env, seed=12345 ) # 2 3 3 3 3
			data.env <- gen.data( env=str.env, seed=1234567 ) # 2 2 3 3 3
			# data.env <- gen.data( env=str.env )
		}
		# put all relevant data and model parameter elements from data environment here
		# names <- ls(envir=env)
		data.structures <- c( "F", "I", "N", "T", "Tunique", "Tj", "deltajp", "yjp", "tunique", "ptuniquejp" )
		model.parameters <- c( "A0", "Achange", "Q0", "Qchange", "SigmaepsA", "SigmaepsQ", "mu0", "muchange", "Sigmamu", "Sigmaepsmu", "Sigmaeps", "Delta" )
		additional.structures <- c( "mujp", "IF", "IF2", "S1", "S2", "S3", "zerovecF", "zerovecF2", "zerovecFF12", "Q0", "Qchange", "SigmaepsA", "SigmaepsQ", "Sigmaeps", "A0", "Achange", "Delta", "epsAt", "epsQt" )
		# "Astarjp", "Qstarjp", "At", "Qt",
		names <- c( data.structures, model.parameters, additional.structures )
		for( i in 1:length( names ) ){
			eval( parse( text=paste0( names[i], " <- get('", names[i], "', envir=data.env)" ) ) )
		}
		print( Tj ); flush.console()

		# ids (j) need to be sorted by number of time points
		resort.ind <- do.call( "c", sapply( unique( sort( Tj ) ), function( T ) which( Tj %in% T ), simplify=FALSE ) )
		yjp <- yjp[,,resort.ind,,drop=FALSE]
		Tj <- Tj[resort.ind]
		deltajp <- deltajp[resort.ind,,drop=FALSE]
		ptuniquejp <- ptuniquejp[resort.ind,,drop=FALSE]
		# temporary
		mujp <- mujp[,,resort.ind,,drop=FALSE]
		# Ajp <- Ajp[,,resort.ind,,drop=FALSE]
		# Qjp <- Qjp[,,resort.ind,,drop=FALSE]
		# unique Tj
		uniqueTj <- unique( Tj ) # Tj should/needs to be sorted at this point
		# prepare indices vectors (lower/upper range) for persons with same number of time points
		Tj.list <- sapply( unique( Tj ), function( T ) which( Tj %in% T ), simplify=FALSE )
		Tjlow <- sapply( Tj.list, min )
		Tjup <- sapply( Tj.list, max )
		# number of different individual time points
		Tjn <- length( unique( Tj ) )
		# number of persons per T
		NperT <- sapply( Tj.list, length )
		# shifted cumulated number of persons per T (starting from 0 for 1st time point)
		NperTcum <- c( 0, cumsum( NperT )[1:(Tjn-1)] )

		# split structure into a list with person groups with same number of time points and delete NAs due to missing time points
		yjpT <- mapply( function( js, Tj ) yjp[,,js,1:Tj,drop=FALSE], Tj.list, uniqueTj, SIMPLIFY=FALSE )
		names( yjpT ) <- paste0( "yjpT", uniqueTj )
		deltajpT <- mapply( function( js, Tj ) deltajp[js,1:Tj,drop=FALSE], Tj.list, uniqueTj-1, SIMPLIFY=FALSE )
		names( deltajpT ) <- paste0( "deltajpT", uniqueTj )
		
		ptuniquejpT <- mapply( function( js, Tj ) ptuniquejp[js,1:Tj,drop=FALSE], Tj.list, uniqueTj, SIMPLIFY=FALSE )
		# ptuniquejpT <- mapply( function( js, Tj ) ptuniquejp[js,1:(Tj-1),drop=FALSE], Tj.list, uniqueTj, SIMPLIFY=FALSE )
		names( ptuniquejpT ) <- paste0( "ptuniquejpT", uniqueTj )


		# !!! alles was noch als data reingeht muss resorted werden

		# temporary
		# mujpT <- mapply( function( js, Tj ) mujp[,,js,1:Tj,drop=FALSE], Tj.list, uniqueTj, SIMPLIFY=FALSE )
		# names( mujpT ) <- paste0( "mujpT", uniqueTj )
		# AjpT <- mapply( function( js, Tj ) Ajp[,,js,1:Tj,drop=FALSE], Tj.list, uniqueTj, SIMPLIFY=FALSE )
		# names( AjpT ) <- paste0( "AjpT", uniqueTj )
		# QjpT <- mapply( function( js, Tj ) Qjp[,,js,1:Tj,drop=FALSE], Tj.list, uniqueTj, SIMPLIFY=FALSE )
		# names( QjpT ) <- paste0( "QjpT", uniqueTj )

		# Tests
		### Delta[2,1] <- "lambda21"
		# Delta[2,1] <- NA
		# Delta[4,2] <- NA
		### Delta[] <- "lambda"
		### Delta[] <- NA
		### Sigmaeps[] <- "sigmaeps"
		# Sigmawjp[1,1,1,1] <- NA
		
		# keep fixed model parameters from data generation for starting values
		keep.fixed <- list( "Delta"=Delta, "Sigmaeps"=Sigmaeps, "A0"=A0, "Achange"=Achange, "SigmaepsA"=SigmaepsA, "SigmaepsQ"=SigmaepsQ, "Q0"=Q0, "Qchange"=Qchange, "mu0"=mu0, "muchange"=muchange, "Sigmamu"=Sigmamu, "Sigmaepsmu"=Sigmaepsmu )
		
		# Defaults
		### TODO: ordentliche Defaults fuer Delta
		Delta[2,1] <- NA
		Delta[4,2] <- NA
		Sigmaeps[] <- NA
		Sigmaeps[ lower.tri( Sigmaeps ) ] <- 0
		Sigmaeps[ upper.tri( Sigmaeps ) ] <- 0
		A0[] <- NA
		Achange[] <- NA
		SigmaepsA[] <- 0
		diag(SigmaepsA) <- NA
		Q0[] <- NA
		### diag(Q0) <- NA
		Qchange[] <- NA
		# Q0 <- matrix( c("q011","q021","q021","q022" ), F, F )
		# Qchange <- matrix( c("qchange11","qchange21","qchange21","qchange22" ), F, F )
		SigmaepsQ[] <- 0
		diag(SigmaepsQ) <- NA
		mu0[] <- NA
		muchange[] <- NA
		Sigmaepsmu[] <- 0
		diag(Sigmaepsmu) <- NA
		Sigmamu[] <- NA

		# temporary, only for fixing as data
		# diag(Q0) <- diag(Q0) + 10^-6
		# diag(Qchange) <- diag(Qchange) + 10^-6

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
		structure.dim          <- c( "matrix[I,F] Delta", "cov_matrix[I] Sigmaeps", "matrix[F,F] A0", "matrix[F,F] Achange", "matrix[F*F,F*F] SigmaepsA", "cov_matrix[F] Q0", "cov_matrix[F] Qchange", "matrix[F*(F+1)/2,F*(F+1)/2] SigmaepsQ", "matrix[F,1] mu0", "matrix[F,1] muchange", "matrix[F,F] Sigmaepsmu", "matrix[F,F] Sigmamu" )
		names( structure.dim ) <- c( "Delta"            , "Sigmaeps"              , "A0"            , "Achange"            , "SigmaepsA"                , "Q0"              , "Qchange"              , "SigmaepsQ"                            , "mu0"            , "muchange"            , "Sigmaepsmu"            , "Sigmamu"             )
		
		# parameter labels for each mixed structure
		par.lab.mixed.str          <- c( "lambda", "sigmaeps", "a0", "achange", "sigmaepsA", "q0", "qchange", "sigmaepsQ", "mu0", "muchange", "sigmaepsmu", "sigmamu" )
		names( par.lab.mixed.str ) <- c( "Delta" , "Sigmaeps", "A0", "Achange", "SigmaepsA", "Q0", "Qchange", "SigmaepsQ", "mu0", "muchange", "Sigmaepsmu", "Sigmamu" )

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

		## functions
		x <- c( x, paste0( "functions {" ) )
	    x <- c( x, paste0( "  vector flatten_matrix_rowwise(matrix mat) {" ) )
	    x <- c( x, paste0( "    int rows = rows(mat); // Get the number of rows in the matrix" ) )
	    x <- c( x, paste0( "    int cols = cols(mat); // Get the number of columns in the matrix" ) )
	    x <- c( x, paste0( "    vector[rows * cols] vec; // Create a vector to hold all elements" ) )
	    x <- c( x, paste0( "    int pos = 1;" ) )
	    x <- c( x, paste0( "    for (i in 1:rows) {" ) )
	    x <- c( x, paste0( "        for (j in 1:cols) {" ) )
	    x <- c( x, paste0( "            vec[pos] = mat[i, j]; // Assign matrix elements to vector" ) )
	    x <- c( x, paste0( "            pos = pos + 1; // Increment position index" ) )
	    x <- c( x, paste0( "        }" ) )
	    x <- c( x, paste0( "    }" ) )
	    x <- c( x, paste0( "    return vec;" ) )
	    x <- c( x, paste0( "  }" ) )
        x <- c( x, paste0( "  matrix unflatten_vector_to_matrix(vector vec, int rows, int cols) {" ) )
        x <- c( x, paste0( "      matrix[rows, cols] mat; // Create a matrix of specified dimensions" ) )
        x <- c( x, paste0( "      int pos = 1;" ) )
        x <- c( x, paste0( "      for (i in 1:rows) {" ) )
        x <- c( x, paste0( "          for (j in 1:cols) {" ) )
        x <- c( x, paste0( "              mat[i, j] = vec[pos]; // Assign vector elements back to matrix" ) )
        x <- c( x, paste0( "              pos = pos + 1; // Increment position index" ) )
        x <- c( x, paste0( "          }" ) )
        x <- c( x, paste0( "      }" ) )
        x <- c( x, paste0( "      return mat;" ) )
        x <- c( x, paste0( "  }" ) )
        x <- c( x, paste0( "  matrix kronecker_product(matrix X, matrix Y) {" ) )
        x <- c( x, paste0( "      int a_rows = rows(X);" ) )
        x <- c( x, paste0( "      int a_cols = cols(X);" ) )
        x <- c( x, paste0( "      int b_rows = rows(Y);" ) )
        x <- c( x, paste0( "      int b_cols = cols(Y);" ) )
        x <- c( x, paste0( "      matrix[a_rows * b_rows, a_cols * b_cols] result;" ) )
        x <- c( x, paste0( "      for (i in 1:a_rows) {" ) )
        x <- c( x, paste0( "          for (j in 1:a_cols) {" ) )
        x <- c( x, paste0( "              for (k in 1:b_rows) {" ) )
        x <- c( x, paste0( "                  for (l in 1:b_cols) {" ) )
        x <- c( x, paste0( "                      result[(i - 1) * b_rows + k, (j - 1) * b_cols + l] = X[i, j] * Y[k, l];" ) )
        x <- c( x, paste0( "                  }" ) )
        x <- c( x, paste0( "              }" ) )
        x <- c( x, paste0( "          }" ) )
        x <- c( x, paste0( "      }" ) )
        x <- c( x, paste0( "      return result;" ) )
        x <- c( x, paste0( "  }" ) )

		# end functions
		x <- c( x, paste0( "}" ) )
		
		## data
		x <- c( x, paste0( "data {" ) )
		x <- c( x, paste0( "  int<lower=1> F;             // ",paste(F,collapse=',')," | number of processes" ) )
		x <- c( x, paste0( "  int<lower=1> I;             // ",paste(I,collapse=',')," | number of items" ) )
		x <- c( x, paste0( "  int<lower=1> N;             // ",paste(N,collapse=',')," | number of persons" ) )
		x <- c( x, paste0( "  int<lower=2> T;             // ",paste(T,collapse=',')," | number of maximum time points of persons" ) )
		x <- c( x, paste0( "  int<lower=2> Tunique;       // ",paste(Tunique,collapse=',')," | number of all unique time points" ) )
		x <- c( x, paste0( "  real tunique[Tunique];      // ",paste(tunique,collapse=',')," | all unique time points" ) )
		x <- c( x, paste0( "  int<lower=2> ",ifelse(N>1,"Tj[N]","Tj"),";",ifelse(N>1,"","   "),"         // ",paste(Tj,collapse=',')," | number of individual time points" ) )
		x <- c( x, paste0( "  int<lower=1> Tjn;           // ",paste(Tjn,collapse=',')," | number of different individual time points" ) )
		x <- c( x, paste0( "  int<lower=1> ",ifelse(Tjn>1,"Tjlow[Tjn]","Tjlow"),";",ifelse(Tjn>1,"","     "),"    // ",paste(Tjlow,collapse=',')," | index vector with lower limit of persons with same number of time points" ) )
		x <- c( x, paste0( "  int<lower=1> ",ifelse(Tjn>1,"Tjup[Tjn]","Tjup"),";",ifelse(Tjn>1,"","     "),"     // ",paste(Tjup,collapse=',')," | index vector with upper limit of persons with same number of time points" ) )
		x <- c( x, paste0( "  int<lower=1> ",ifelse(Tjn>1,"NperT[Tjn]","NperT"),";",ifelse(Tjn>1,"","     "),"    // ",paste(NperT,collapse=',')," | number of persons per T" ) )
		x <- c( x, paste0( "  int<lower=0> ",ifelse(Tjn>1,"NperTcum[Tjn]","NperTcum"),";",ifelse(Tjn>1,"","     ")," // ",paste(NperTcum,collapse=',')," | shifted cumulated number of persons per T (starting from 0 for 1st time point)" ) )
		x <- c( x, paste0( "  matrix[F,F] IF;             // identity matrix of size ", F ) )
		x <- c( x, paste0( "  matrix[F*F,F*F] IF2;        // identity matrix of size ", F^2 ) )
		x <- c( x, paste0( "  matrix[F*F,F*F] S1;         // selection matrix 1" ) )
		x <- c( x, paste0( "  matrix[F*F,F*F] S2;         // selection matrix 2" ) )
		x <- c( x, paste0( "  matrix[F*F,F*(F+1)/2] S3;   // selection matrix 3" ) )
		x <- c( x, paste0( "  matrix[F,1] zerovecF;       // zero column vector of size ", F ) )
		x <- c( x, paste0( "  matrix[F*F,1] zerovecF2;    // zero column vector of size ", F^2 ) )
		x <- c( x, paste0( "  matrix[F*(F+1)/2,1] zerovecFF12;  // zero column vector of size ", F*(F+1)/2 ) )

		# temporary
		# x <- c( x, paste0( "  cov_matrix[F] Q0;" ) )
		# x <- c( x, paste0( "  matrix[F,F] Q0;" ) )
		# x <- c( x, paste0( "  cov_matrix[F] Qchange;" ) )
		# x <- c( x, paste0( "  matrix[F,F] Qchange;" ) )
		# x <- c( x, paste0( "  matrix[I,I] Sigmaeps;" ) )
		# x <- c( x, paste0( "  matrix[F*F,F*F] SigmaepsA;" ) )
		# x <- c( x, paste0( "  matrix[F*(F+1)/2,F*(F+1)/2] SigmaepsQ;" ) )
		# x <- c( x, paste0( "  matrix[F,F] A0;" ) )
		# x <- c( x, paste0( "  matrix[F,F] Achange;" ) )
		# x <- c( x, paste0( "  matrix[I,F] Delta;" ) )



		# observed values of persons with same number of time points
		for( i in 1:length(uniqueTj) ){
			x <- c( x, paste0( "  real yjpT",uniqueTj[i],"[I,1,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i],"];",ifelse(T<10000,paste(rep(" ",5-nchar(as.character(uniqueTj[i]))),collapse="")," "),"// observed values of ",NperT[i]," persons with ",uniqueTj[i]," time points" ) )
		}
		# time interval lengths of persons with same number of time points
		for( i in 1:length(uniqueTj) ){
			x <- c( x, paste0( "  real deltajpT",uniqueTj[i],"[NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i]-1,"];",ifelse(T<10000,paste(rep(" ",5-nchar(as.character(uniqueTj[i]))),collapse="")," "),"// time interval lengths of ",NperT[i]," persons with ",uniqueTj[i]," time points" ) )
		}		
		# indices of time points of persons in tunique vector
		for( i in 1:length(uniqueTj) ){
			# x <- c( x, paste0( "  int<lower=1> ptuniquejpT",uniqueTj[i],"[NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i]-1,"];",ifelse(T<10000,paste(rep(" ",5-nchar(as.character(uniqueTj[i]-1))),collapse="")," "),"// indices of time points in tunique vector of ",NperT[i]," persons with ",uniqueTj[i]," time points" ) )
			x <- c( x, paste0( "  int<lower=1> ptuniquejpT",uniqueTj[i],"[NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i],"];",ifelse(T<10000,paste(rep(" ",5-nchar(as.character(uniqueTj[i]))),collapse="")," "),"// indices of time points in tunique vector of ",NperT[i]," persons with ",uniqueTj[i]," time points" ) )
		}		
		# temporary, Ajp
		# for( i in 1:length(uniqueTj) ){
			# x <- c( x, paste0( "  real AjpT",uniqueTj[i],"[F,F,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i],"];",ifelse(T<10000,paste(rep(" ",5-nchar(as.character(uniqueTj[i]))),collapse="")," ") ) )
			# x <- c( x, paste0( "  real QjpT",uniqueTj[i],"[F,F,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i],"];",ifelse(T<10000,paste(rep(" ",5-nchar(as.character(uniqueTj[i]))),collapse="")," ") ) )
		# }
		# temporary, mujpTX
		# for( i in 1:length(uniqueTj) ){
			# x <- c( x, paste0( "  real mujpT",uniqueTj[i],"[F,1,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i],"];",ifelse(T<10000,paste(rep(" ",5-nchar(as.character(uniqueTj[i]))),collapse="")," ") ) )
		# }
		# fixed structures go in as data
		if( length( wifis <- which( structure.type %in% "fixed" ) ) > 0 ){
			for( st in names( structure.type[wifis] ) ){
				eval( parse( text=paste0( "x <- c( x, paste0( '  ",structure.dim[st],";' ) )" ) ) )
			}
		}
		# temporary
		# x <- c( x, paste0( "  real Sigmawjp[F,F,N,T];" ) )
		# x <- c( x, paste0( "  real Qstarjp[F,F,N,T-1];" ) )
		# x <- c( x, paste0( "  real Astarjp[F,F,N,T-1];" ) )
		# x <- c( x, paste0( "  real mujp[F,1,N,T];" ) )
		# x <- c( x, paste0( "  real Ajp[F,F,N,T];" ) )
		# x <- c( x, paste0( "  real Qjp[F,F,N,T];" ) )
		# x <- c( x, paste0( "int<lower=1> O;" ) )
		# x <- c( x, paste0( "int<lower=1> M;" ) )
		# x <- c( x, paste0( "matrix[O, O] A;" ) )
		# x <- c( x, paste0( "matrix[M, M] B;" ) )
		
		# end data
		x <- c( x, paste0( "}" ) )

		## transformed data
		# x <- c( x, paste0( "transformed data {" ) )
        # x <- c( x, paste0( "matrix[F,F] scale_matrix_F;" ) )
        # x <- c( x, paste0( "for (i in 1:F) {" ) )
        # x <- c( x, paste0( "  for (j in 1:F) {" ) )
        # x <- c( x, paste0( "    scale_matrix_F[i,j] = (i == j) ? 1.0 : 0.0;  // Identity matrix" ) )
        # x <- c( x, paste0( "  }" ) )
        # x <- c( x, paste0( "}" ) )
		# x <- c( x, paste0( "  int Tjlow[2] = {1,5};" ) )			
		# x <- c( x, paste0( "   matrix[O*M, O*M] C;  ") )		
		# x <- c( x, paste0( "   C = kronecker_product(A, B);  ") )		
		# temporary
		# x <- c( x, paste0( "  matrix[F,F] Q0_pos_def = Q0 + 1e-6 * diag_matrix(rep_vector(1,F));" ) )
		# x <- c( x, paste0( "  matrix[F,F] Qchange_pos_def = Qchange + 1e-6 * diag_matrix(rep_vector(1,F));" ) )
		# x <- c( x, paste0( "  matrix[I,I] Sigmaeps_pos_def = Sigmaeps + 1e-6 * diag_matrix(rep_vector(1,I));" ) )
		# x <- c( x, paste0( "  matrix[F*F,F*F] SigmaepsA_pos_def = SigmaepsA + 1e-6 * diag_matrix(rep_vector(1,F*F));" ) )
		# x <- c( x, paste0( "  matrix[F*(F+1)/2,F*(F+1)/2] SigmaepsQ_pos_def = SigmaepsQ + 1e-6 * diag_matrix(rep_vector(1,F*(F+1)/2));" ) )

		
		# end transformed data
		# x <- c( x, paste0( "}" ) )
		
		## parameters
		x <- c( x, paste0( "parameters {" ) )
		# latent values of persons with same number of time points
		for( i in 1:length(uniqueTj) ){
			x <- c( x, paste0( "  real thetajpT",uniqueTj[i],"[F,1,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i],"]; // latent values of ",NperT[i]," persons with ",uniqueTj[i]," time points" ) )
		}
		# free structures go in as parameters
		if( length( wifres <- which( structure.type %in% "free" ) ) > 0 ){
			x <- c( x, paste0( "  // free structures: ", paste( names( structure.type[wifres] ), collapse=", " ) ) )
			for( st in names( structure.type[wifres] ) ){
				# eval( parse( text=paste0( "x <- c( x, paste0( '  ",structure.dim[st]," ",st,";' ) )" ) ) )
				eval( parse( text=paste0( "x <- c( x, paste0( '  ",structure.dim[st],";' ) )" ) ) )
			}
		}		
		# parameters of mixed structures
		if( length( parameters.of.mixed.structures ) > 0 ){		
			x <- c( x, paste0( "  // parameters of mixed structures" ) )
			eval( parse( text=paste0("x <- c( x, '  real ",parameters.of.mixed.structures,";' )" ) ) )
		}
		# epsAt, epsQt, epsmut
		x <- c( x, paste0( "  // epsAt, epsQt, epsmut" ) )
		x <- c( x, paste0( "  real epsAt[F*F,1,Tunique];" ) )
		x <- c( x, paste0( "  real epsQt[F*(F+1)/2,1,Tunique];" ) )
		x <- c( x, paste0( "  real epsmut[F,1,Tunique];" ) )
		x <- c( x, paste0( "  real muj[F,1,N];" ) )

		# end parameters
		x <- c( x, paste0( "}" ) )
		
		## transformed parameters
		x <- c( x, paste0( "transformed parameters {" ) )
		# mixed structures go in as transformed parameters
		if( length( wimis ) > 0 ){
			x <- c( x, paste0( "  // mixed structures: ", paste( names( structure.type[wimis] ), collapse=", " ) ) )
			for( st in names( structure.type[wimis] ) ){
				eval( parse( text=paste0( "x <- c( x, paste0( '  ",structure.dim[st],";' ) )" ) ) )
				for( c in 1:eval(parse(text=paste0("ncol(",st,")"))) ) {
					for( r in 1:eval(parse(text=paste0("nrow(",st,")"))) ){
						x <- c( x, paste0( "  ",st,"[",r,",",c,"] <- ",eval(parse(text=paste0(st,"[",r,",",c,"]"))),";" ) )
					}
				}
			}
		}
		x <- c( x, paste0( "  // Cholesky decompositions" ) )
		x <- c( x, paste0( "  matrix[F,F] Q0Chol = cholesky_decompose( Q0 );" ) )
		x <- c( x, paste0( "  matrix[F,F] QchangeChol = cholesky_decompose( Qchange );" ) )
		x <- c( x, paste0( "  matrix[F*F,F*F] SigmaepsAChol = cholesky_decompose( SigmaepsA );" ) )
		x <- c( x, paste0( "  matrix[F*(F+1)/2,F*(F+1)/2] SigmaepsQChol = cholesky_decompose( SigmaepsQ );" ) )
		x <- c( x, paste0( "  matrix[F,F] SigmaepsmuChol = cholesky_decompose( Sigmaepsmu );" ) )
		x <- c( x, paste0( "  matrix[F,F] SigmamuChol = cholesky_decompose( Sigmamu );" ) )
		x <- c( x, paste0( "  matrix[I,I] SigmaepsChol = cholesky_decompose( Sigmaeps );" ) )
		# At, Qt
		x <- c( x, paste0( "  // time-varying drift/diffusion/mu" ) )
		x <- c( x, paste0( "  real At[F,F,Tunique];  // time-varying drift matrices" ) )
		x <- c( x, paste0( "  real Qt[F,F,Tunique];  // time-varying diffusion matrices" ) )
		x <- c( x, paste0( "  real mut[F,1,Tunique]; // time-varying mean vectors" ) )
		x <- c( x, paste0( "  for( p in 1:Tunique ){" ) )
	    x <- c( x, paste0( "    for (i in 1:F){" ) )
	    x <- c( x, paste0( "      for (k in 1:F){" ) )
		x <- c( x, paste0( "        // Eq. 2" ) )
		x <- c( x, paste0( "        At[i,k,p] = (  unflatten_vector_to_matrix( S1 * flatten_matrix_rowwise( A0 + Achange*tunique[p] + unflatten_vector_to_matrix( to_vector(epsAt[,1,p]),F,F ) ), F,F)   +   unflatten_vector_to_matrix( S2 * flatten_matrix_rowwise( A0 .* exp( -( Achange*tunique[p] + unflatten_vector_to_matrix( to_vector(epsAt[,1,p]),F,F ) ) ) ), F,F)   )[i,k];" ) )
		x <- c( x, paste0( "        // Eq. 3" ) )
		x <- c( x, paste0( "        Qt[i,k,p] = (  unflatten_vector_to_matrix( S1 * flatten_matrix_rowwise( Q0Chol*Q0Chol' + (QchangeChol*QchangeChol')*tunique[p] + unflatten_vector_to_matrix( S3 * to_vector(epsQt[,1,p]),F,F ) ), F,F)   +   unflatten_vector_to_matrix( S2 * flatten_matrix_rowwise( (Q0Chol*Q0Chol') .* exp( -( (QchangeChol*QchangeChol')*tunique[p] + unflatten_vector_to_matrix( S3 * to_vector(epsQt[,1,p]),F,F ) ) ) ), F,F)   )[i,k];" ) )
		x <- c( x, paste0( "      }" ) )
		x <- c( x, paste0( "    }" ) )
	    x <- c( x, paste0( "    for (i in 1:F){" ) )
		x <- c( x, paste0( "      // Eq. 9" ) )
		x <- c( x, paste0( "      mut[i,1,p] = (  to_vector(mu0) + to_vector(muchange)*tunique[p] + to_vector(epsmut[,1,p])  )[i];" ) )
		x <- c( x, paste0( "    }" ) )
							# x <- c( x, paste0( "        print( At[,,p] );" ) )
							# x <- c( x, paste0( "        print( Qt[,,p] );" ) )
		x <- c( x, paste0( "  }" ) )
		# Ajp, Qjp, mujp
		x <- c( x, paste0( "  // Ajp, Qjp, mujp" ) )
		for( i in 1:length(uniqueTj) ){
			# x <- c( x, paste0( "  real ",c('AjpT','QjpT'),uniqueTj[i],"[F,F,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i]-1,"];" ) )
			x <- c( x, paste0( "  real ",c('AjpT','QjpT'),uniqueTj[i],"[F,F,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i],"];" ) )
			x <- c( x, paste0( "  real mujpT",uniqueTj[i],"[F,1,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i],"];",ifelse(T<10000,paste(rep(" ",5-nchar(as.character(uniqueTj[i]))),collapse="")," ") ) )
			
			x <- c( x, paste0( "  for (j in Tjlow",ifelse(Tjn>1,paste0('[',i,']'),''),":Tjup",ifelse(Tjn>1,paste0('[',i,']'),''),"){ // range: ",Tjlow[i],":",Tjup[i],", NperTcum=",NperTcum[i],", NperT=",NperT[i] ) )
			# x <- c( x, paste0( "    for (p in 1:(",ifelse(N>1,"Tj[j]","Tj"),"-1)){ // range: 1:",uniqueTj[i]-1 ) )
			x <- c( x, paste0( "    for (p in 1:",ifelse(N>1,"Tj[j]","Tj"),"){ // range: 1:",uniqueTj[i] ) )
							# x <- c( x, paste0( "print( ptuniquejpT",uniqueTj[i],"[j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] );" ) )
							# x <- c( x, paste0( "print( \"j: \", j );" ) )
							# x <- c( x, paste0( "print( \"p: \", p );" ) )
							# x <- c( x, paste0( "print( \"ptuniquejp",uniqueTj[i],": \", ptuniquejpT",uniqueTj[i],"[j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] );" ) )
			x <- c( x, paste0( "      for (i in 1:F){" ) )
			x <- c( x, paste0( "        for (k in 1:F){" ) )
			x <- c( x, paste0( "          // Eq. 11" ) )
			x <- c( x, paste0( "          ",c('AjpT','QjpT'),uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] = to_matrix(",c('At','Qt'),"[,,ptuniquejpT",uniqueTj[i],"[j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]])[i,k];" ) )
			x <- c( x, paste0( "        }" ) )
			x <- c( x, paste0( "      }" ) )
			x <- c( x, paste0( "      for (i in 1:F){" ) )
			x <- c( x, paste0( "        // Eq. 11" ) )
			# x <- c( x, paste0( "        mujpT",uniqueTj[i],"[i,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] = to_vector( mut[,1,ptuniquejpT",uniqueTj[i],"[j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]] )[i];" ) )
			x <- c( x, paste0( "        mujpT",uniqueTj[i],"[i,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] = (  to_vector( mut[,1,ptuniquejpT",uniqueTj[i],"[j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]] ) + to_vector( muj[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),"] )  )[i];" ) )
			x <- c( x, paste0( "      }" ) )
			
			x <- c( x, paste0( "    }" ) )
			x <- c( x, paste0( "  }" ) )
							# x <- c( x, paste0( "        print( AjpT",uniqueTj[i]," );" ) )
							# x <- c( x, paste0( "        print( QjpT",uniqueTj[i]," );" ) )
		}


		# Astarjp, Qstarjp, Sigmawjp
		for( i in 1:length(uniqueTj) ){
			x <- c( x, paste0( "  real ",c('AstarjpT','QstarjpT','QstarjpCholT','SigmawjpT','SigmawjpCholT'),uniqueTj[i],"[F,F,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i],"-1]; // matrices for ",NperT[i]," persons with ",uniqueTj[i]," time points" ) )
		}
		# Ahash
		for( i in 1:length(uniqueTj) ){
			x <- c( x, paste0( "  real ",c('AhashjpT'),uniqueTj[i],"[F*F,F*F,NperT",ifelse(Tjn>1,paste0('[',i,']'),''),",",uniqueTj[i],"-1]; // Ahash matrices for ",NperT[i]," persons with ",uniqueTj[i]," time points" ) )
		}
		# Astarjp, Qstarjp, Ahashjp, Sigmawjp
		x <- c( x, paste0( "  // ##### loops over person groups with same number of time points T #########################################" ) )
		for( i in 1:length(uniqueTj) ){
			x <- c( x, paste0( "  // #",i,"# loop over time points of persons with T=",uniqueTj[i] ) )		
		    x <- c( x, paste0( "  for (j in Tjlow",ifelse(Tjn>1,paste0('[',i,']'),''),":Tjup",ifelse(Tjn>1,paste0('[',i,']'),''),"){ // range: ",Tjlow[i],":",Tjup[i],", NperTcum=",NperTcum[i],", NperT=",NperT[i] ) )		
		    x <- c( x, paste0( "    for (p in 1:(",ifelse(N>1,"Tj[j]","Tj"),"-1)){ // range: 1:",uniqueTj[i]-1 ) )
		    x <- c( x, paste0( "      // Astarjp, Eq. 12" ) )
		    x <- c( x, paste0( "      for (i in 1:F){" ) )
		    x <- c( x, paste0( "        for (k in 1:F){" ) )
		    x <- c( x, paste0( "          AstarjpT",uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] = matrix_exp(to_matrix(AjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p])*deltajpT",uniqueTj[i],"[j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p])[i,k];" ) )
		    x <- c( x, paste0( "        }" ) )
		    x <- c( x, paste0( "      }" ) )
		    x <- c( x, paste0( "      // Ahashjp, Eq. 14" ) )
		    x <- c( x, paste0( "      for (i in 1:(F*F)){" ) )
		    x <- c( x, paste0( "        for (k in 1:(F*F)){" ) )
			x <- c( x, paste0( "          AhashjpT",uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] = (kronecker_product(to_matrix(AjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]),IF) + kronecker_product(IF,to_matrix(AjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p])))[i,k];" ) )
		    x <- c( x, paste0( "        }" ) )
		    x <- c( x, paste0( "      }" ) )
		    x <- c( x, paste0( "      // Sigmawjp, Eq. 14" ) )
		    x <- c( x, paste0( "      for (i in 1:F){" ) )
			x <- c( x, paste0( "        for (k in 1:F){" ) )
		    x <- c( x, paste0( "          SigmawjpT",uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] = unflatten_vector_to_matrix(-1*inverse(to_matrix(AhashjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p])) * flatten_matrix_rowwise(to_matrix(QjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p])),F,F)[i,k];" ) )
		    x <- c( x, paste0( "        }" ) )
		    x <- c( x, paste0( "      }" ) )
		    x <- c( x, paste0( "      // Sigmawjp Cholesky" ) )
		    x <- c( x, paste0( "      for (i in 1:F){" ) )
			x <- c( x, paste0( "        for (k in 1:F){" ) )
		    x <- c( x, paste0( "          SigmawjpCholT",uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] = cholesky_decompose( to_matrix(SigmawjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]) )[i,k];" ) )
		    x <- c( x, paste0( "        }" ) )
		    x <- c( x, paste0( "      }" ) )			
		    x <- c( x, paste0( "      // Qstarjp, Eq. 13" ) )
		    x <- c( x, paste0( "      for (i in 1:F){" ) )
			x <- c( x, paste0( "        for (k in 1:F){" ) )
		    x <- c( x, paste0( "          QstarjpT",uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] = unflatten_vector_to_matrix( -1*( matrix_exp(to_matrix(AhashjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p])*deltajpT",uniqueTj[i],"[j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]) - IF2 ) * flatten_matrix_rowwise(to_matrix(SigmawjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p])),F,F)[i,k];" ) )
		    x <- c( x, paste0( "        }" ) )
		    x <- c( x, paste0( "      }" ) )
		    x <- c( x, paste0( "      // Qstarjp Cholesky" ) )
		    x <- c( x, paste0( "      for (i in 1:F){" ) )
			x <- c( x, paste0( "        for (k in 1:F){" ) )
		    x <- c( x, paste0( "          QstarjpCholT",uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] = cholesky_decompose( to_matrix(QstarjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]) )[i,k];" ) )
		    x <- c( x, paste0( "        }" ) )
		    x <- c( x, paste0( "      }" ) )			
	                            # x <- c( x, paste0( "for (i in 1:F) {" ) )
                                # x <- c( x, paste0( "    for (k in 1:F) {" ) )
                                # x <- c( x, paste0( "        print(\"Qstarjp[\", i, \",\", k, \"] = \", QstarjpT",uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]);" ) )
                                # x <- c( x, paste0( "    }" ) )
                                # x <- c( x, paste0( "}" ) )
	                            # x <- c( x, paste0( "for (i in 1:F) {" ) )
                                # x <- c( x, paste0( "    for (k in 1:F) {" ) )
                                # x <- c( x, paste0( "        print(\"Sigmawjp[\", i, \",\", k, \"] = \", SigmawjpT",uniqueTj[i],"[i,k,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]);" ) )
                                # x <- c( x, paste0( "    }" ) )
                                # x <- c( x, paste0( "}" ) )		
			x <- c( x, paste0( "    }" ) )
		    x <- c( x, paste0( "  }" ) )

		}
		# end transformed parameters
		x <- c( x, paste0( "}" ) )
		
		## model
		x <- c( x, paste0( "model {" ) )
		x <- c( x, paste0( "  // ##### loops over person groups with same number of time points T #########################################" ) )
		for( i in 1:length(uniqueTj) ){
			x <- c( x, paste0( "  // #",i,"# loop over time points of persons with T=",uniqueTj[i] ) )
			x <- c( x, paste0( "  for (j in Tjlow",ifelse(Tjn>1,paste0('[',i,']'),''),":Tjup",ifelse(Tjn>1,paste0('[',i,']'),''),"){ // range: ",Tjlow[i],":",Tjup[i],", NperTcum=",NperTcum[i],", NperT=",NperT[i] ) )
			x <- c( x, paste0( "    // first time point, Eq. 17" ) )
								# x <- c( x, paste0( "        print( \"Sigmawjp: \", to_matrix(SigmawjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1]) );" ) )
			x <- c( x, paste0( "    to_vector( thetajpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1] ) ~ multi_normal_cholesky(to_vector(mujpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1]),to_matrix(SigmawjpCholT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",1]));" ) )
			x <- c( x, paste0( "    // loop beginning at second time points" ) )
			x <- c( x, paste0( "    for (p in 2:",ifelse(N>1,"Tj[j]","Tj"),"){ // range: 2:",uniqueTj[i] ) )
			x <- c( x, paste0( "      // time series, Eq. 15/16 " ) )
								# x <- c( x, paste0( "        print( \"QstarjpT: \", to_matrix(QstarjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1]) );" ) )
			x <- c( x, paste0( "      to_vector( thetajpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] ) ~ multi_normal_cholesky(to_matrix(AstarjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1])*to_vector(thetajpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1])+(IF - to_matrix(AstarjpT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1]))*to_vector(mujpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]),to_matrix(QstarjpCholT",uniqueTj[i],"[,,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p-1]));" ) )
			x <- c( x, paste0( "    }" ) )
			x <- c( x, paste0( "    // loop over all time points" ) )
			x <- c( x, paste0( "    for (p in 1:",ifelse(N>1,"Tj[j]","Tj"),"){ // range: 1:",uniqueTj[i] ) )
			x <- c( x, paste0( "      // measurement model, Eq. 18/19 " ) )
			x <- c( x, paste0( "      to_vector( yjpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p] ) ~ multi_normal_cholesky( Delta*to_vector(thetajpT",uniqueTj[i],"[,1,j-NperTcum",ifelse(Tjn>1,paste0('[',i,']'),''),",p]),SigmaepsChol);" ) )
			x <- c( x, paste0( "    }" ) )
			x <- c( x, paste0( "  }" ) )
		}
		x <- c( x, paste0( "  // epsAt, Eq. 4; epsQt, Eq. 5; epsQmut, Eq. 10" ) )
		x <- c( x, paste0( "  for( p in 1:Tunique ){" ) )
		x <- c( x, paste0( "    to_vector( epsAt[,1,p] )  ~ multi_normal_cholesky( to_vector(zerovecF2)  , SigmaepsAChol );" ) )
		x <- c( x, paste0( "    to_vector( epsQt[,1,p] )  ~ multi_normal_cholesky( to_vector(zerovecFF12), SigmaepsQChol );" ) )
		x <- c( x, paste0( "    to_vector( epsmut[,1,p] ) ~ multi_normal_cholesky( to_vector(zerovecF)   , SigmaepsmuChol );" ) )
		x <- c( x, paste0( "  }" ) )
		x <- c( x, paste0( "  // epsAt, Eq. 4; epsQt, Eq. 5; epsQmut, Eq. 10" ) )
		x <- c( x, paste0( "  for( j in 1:N ){" ) )
		x <- c( x, paste0( "    to_vector( muj[,1,j] ) ~ multi_normal_cholesky( to_vector(zerovecF), SigmamuChol );" ) )
		x <- c( x, paste0( "  }" ) )
		
		# priors for parameters of mixed structures
		# if( length( parameters.of.mixed.structures ) > 0 ){		
			# x <- c( x, paste0( "  // ##### priors for parameters of mixed structures #################" ) )
			# eval( parse( text=paste0("x <- c( x, '  ",parameters.of.mixed.structures," ~ normal( 1, 0.25 );' )" ) ) )
		# }
		# priors for covariance matrices
		# x <- c( x, paste0( "  // priors for covariance matrices" ) )
		# x <- c( x, paste0( "  Sigmaeps ~ inv_wishart(I+3, scale_matrix);" ) )
		# x <- c( x, paste0( "  SigmaepsA ~ inv_wishart(F*F+3, scale_matrix);" ) )
		# x <- c( x, paste0( "  SigmaepsQ ~ inv_wishart(F*(F+1)/2+3, scale_matrix);" ) )
		# x <- c( x, paste0( "  Q0 ~ inv_wishart(F+3, scale_matrix_F);" ) )
		# x <- c( x, paste0( "  Qchange ~ inv_wishart(F+3, scale_matrix_F);" ) )
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
		# O <- M <- 2
		dat <- list( "F"=F, "I"=I, "N"=N, "T"=T, "Tunique"=Tunique, "Tj"=Tj, "Tjn"=as.integer(Tjn), "Tjlow"=Tjlow, "Tjup"=Tjup, "NperT"=NperT, "NperTcum"=NperTcum, "IF"=IF, "IF2"=IF2, "S1"=S1, "S2"=S2, "S3"=S3, "tunique"=tunique, "zerovecF"=zerovecF, "zerovecF2"=zerovecF2, "zerovecFF12"=zerovecFF12 )
		# temporary
		# dat <- c( dat, list( "At"=At, "Qt"=Qt ) ) # "Ajp"=Ajp, "Qjp"=Qjp, "Sigmawjp"=Sigmawjp, "Astarjp"=Astarjp, "Qstarjp"=Qstarjp, "mujp"=mujp
		# dat <- c( dat, list( "Q0"=Q0, "Qchange"=Qchange, "SigmaepsA"=SigmaepsA, "SigmaepsQ"=SigmaepsQ, "Sigmaeps"=Sigmaeps, "A0"=A0, "Achange"=Achange, "Delta"=Delta ) )
		# yjpT, deltajpT, TEMP mujpT AjpT QjpT
		dat <- c( dat, yjpT, deltajpT, mujpT, ptuniquejpT ) # , AjpT, QjpT
		# add fixed structures
		if( length( wifis ) > 0 ){
			for( st in names( structure.type[wifis] ) ){
				eval( parse( text=paste0( "dat$", st, " <- ", st ) ) )
			}
		}
		
		## start values for parameters of mixed structures
		# https://discourse.mc-stan.org/t/how-to-define-initial-values-in-stan-in-r/16855
		starting.values <- rep(NA,length(parameters.of.mixed.structures))
		names( starting.values ) <- parameters.of.mixed.structures
		starting.values[grepl("^lambda.*$", names( starting.values ) )] <- 1
		starting.values[grepl("^sigmaeps[[:digit:]]+.*$", names( starting.values ) )] <- diag( keep.fixed$Sigmaeps )
		starting.values[grepl("^sigmaepsA[[:digit:]]+.*$", names( starting.values ) )] <- diag( keep.fixed$SigmaepsA )
		starting.values[grepl("^sigmaepsQ[[:digit:]]+.*$", names( starting.values ) )] <- diag( keep.fixed$SigmaepsQ )
		starting.values[grepl("^sigmaepsmu[[:digit:]]+.*$", names( starting.values ) )] <- diag( keep.fixed$Sigmaepsmu )
		
		# if( length( parameters.of.mixed.structures ) > 0 ){		
			# eval(parse(text=paste0( "init_fun <- function(...) c( list(  ",paste( paste0( "'", parameters.of.mixed.structures, "'=",starting.values[parameters.of.mixed.structures],"" ), collapse=", " ),"  ) )" ))) # , 'Q0'=keep.fixed$Q0, 'Qchange'=keep.fixed$Qchange
			# eval(parse(text=paste0( "init_fun <- function(...) c( list(  ",paste( paste0( "'", parameters.of.mixed.structures, "'=1" ), collapse=", " ),"  ) )" )))
			# eval(parse(text=paste0( "init_fun <- function(...) c( list(  ",paste( paste0( "'", parameters.of.mixed.structures, "'=1" ), collapse=", " ),"  ), s.val )" )))
		# } else {
			# init_fun <- "random"
		# }
		
epsAt[] <- 0
epsQt[] <- 0
Q0 <- keep.fixed$Q0
Q0[] <- 0
diag( Q0 ) <- 1
Qchange <- keep.fixed$Qchange
Qchange[] <- 0
diag( Qchange ) <- 10^-6
A0 <- keep.fixed$A0
A0[] <- 0
diag( A0 ) <- -0.75
Achange <- keep.fixed$Achange
Achange[] <- 0
Sigmamu <- keep.fixed$Sigmamu
Sigmamu[] <- 0
diag( Sigmamu ) <- 1
sigmaepsA.sv <- paste( paste0( "'", parameters.of.mixed.structures[grepl("^sigmaepsA.*$",parameters.of.mixed.structures)], "'=",starting.values[parameters.of.mixed.structures[grepl("^sigmaepsA.*$",parameters.of.mixed.structures)]],"" ), collapse=", " )
sigmaepsQ.sv <- paste( paste0( "'", parameters.of.mixed.structures[grepl("^sigmaepsQ.*$",parameters.of.mixed.structures)], "'=",starting.values[parameters.of.mixed.structures[grepl("^sigmaepsQ.*$",parameters.of.mixed.structures)]],"" ), collapse=", " )
sigmaepsmu.sv <- paste( paste0( "'", parameters.of.mixed.structures[grepl("^sigmaepsmu.*$",parameters.of.mixed.structures)], "'=",starting.values[parameters.of.mixed.structures[grepl("^sigmaepsmu.*$",parameters.of.mixed.structures)]],"" ), collapse=", " )
sigmaeps.sv <- paste( paste0( "'", parameters.of.mixed.structures[grepl("^sigmaeps[[:digit:]]+.*$",parameters.of.mixed.structures)], "'=",starting.values[parameters.of.mixed.structures[grepl("^sigmaeps[[:digit:]]+.*$",parameters.of.mixed.structures)]],"" ), collapse=", " )
lambda.sv <- paste( paste0( "'", parameters.of.mixed.structures[grepl("^lambda[[:digit:]]+.*$",parameters.of.mixed.structures)], "'=",starting.values[parameters.of.mixed.structures[grepl("^lambda[[:digit:]]+.*$",parameters.of.mixed.structures)]],"" ), collapse=", " )

		eval(parse(text=paste0( "init_fun <- function(...) c( list( 'epsAt'=epsAt, 'epsQt'=epsQt, 'A0'=A0, 'Achange'=Achange, 'Q0'=Q0, 'Qchange'=Qchange, 'Sigmamu'=Sigmamu,",sigmaepsA.sv,", ",sigmaepsQ.sv,", ",sigmaepsmu.sv,", ",sigmaeps.sv,", ",lambda.sv," ) )" )))



print(Tj);flush.console()
		# fit
		fit <- stan( file = syntax.path, data = dat, chains = 1, iter = 10, init=init_fun )
		
		print( fit )

		( summ <- summary(fit, pars = c("Delta", "A0"))$summary )


		# m <- as.matrix(fit)
		# colMeans( m[,grepl("^Delta.*$",colnames(m))] )
		# colMeans( m[,grepl("^A0.*$",colnames(m))] )
		# colMeans( m[,grepl("^Achange.*$",colnames(m))] )
		# colMeans( m[,grepl("^Q0\\[.*$",colnames(m))] )
		# colMeans( m[,grepl("^Qchange\\[.*$",colnames(m))] )
		# colMeans( m[,grepl("^SigmaepsA.*$",colnames(m))] )
		# colMeans( m[,grepl("^SigmaepsQ.*$",colnames(m))] )
		# colMeans( m[,grepl("^Sigmaeps\\[.*$",colnames(m))] )
		
		
		
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
