## Changelog:
# MH 2024-04-29:

# require packages
require( rstan )
# options( mc.cores = (parallel::detectCores()-1) )
options( mc.cores = 1 ); rstan_options(auto_write = TRUE)

library( conttime ) # 0.0.21 2024-04-29

### development
# library( mvtnorm )
# library( expm )
# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    # "Dropbox/139_conttime/conttime/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("") ]
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }

design.env <- gen.design(N=100,T=10,Tdiv=-2)
data.env <- gen.data( design.env=design.env )
stn <- gen.stan( data.env=data.env )

                              
start <- Sys.time()
fit <- stan( file = stn$syntax.path, data = stn$data, init=stn$init, chains = 14, iter = 10000 )
print( runtime <- Sys.time() - start )
save( fit, design.env, data.env, stn, runtime, file="c:/Users/martin/Dropbox/139_conttime/conttime/inst/examples/run2.Rdata" )
# load( file="c:/Users/martin/Dropbox/139_conttime/conttime/inst/examples/run1.Rdata" )

est <- get.stan( fit=fit, stn=stn, true.env=data.env )

# hist(est$bias)
est <- est[ order( est$bias, decreasing=TRUE ), ]


# print( fit )

# ( summ <- summary(fit, pars = c("Delta", "A0"))$summary )

# m <- as.matrix(fit)
# colMeans( m[,grepl("^Delta.*$",colnames(m))] )
# colMeans( m[,grepl("^A0.*$",colnames(m))] )
# colMeans( m[,grepl("^Achange.*$",colnames(m))] )
# colMeans( m[,grepl("^Q0\\[.*$",colnames(m))] )
# colMeans( m[,grepl("^Qchange\\[.*$",colnames(m))] )
# colMeans( m[,grepl("^SigmaepsA.*$",colnames(m))] )
# colMeans( m[,grepl("^SigmaepsQ.*$",colnames(m))] )
# colMeans( m[,grepl("^Sigmaeps\\[.*$",colnames(m))] )



### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
