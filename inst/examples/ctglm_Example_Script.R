
library( ctglm ) #  version 0.0.24 ... in s$call weiter unten ist die run syntax, dort stehen weitere Pakete die man braucht, nämlich:
require( 'rjags' )
require( 'doParallel' )
require( 'abind' )

# Daten
load( "n:/Dropbox/IQB/Projekte/ctirt/simdata/dats/sim109/01_simdat/simdat_charge1/J4T6A2Qt3M2S1r1.Rdata" )

### wahre Modellparameter 
load("n:/Dropbox/IQB/Projekte/ctirt/J4T1A2Qt3M2S1r1_true.Rdata")

### ggf. Datensatz reduzieren
id <- sample ( 1:1000, 400, FALSE)
d <- d[which(d[,"id"] %in% id),]
d <- d[which(d[,"time"] %in% 0:199),]

# Itemparameter, ein Item auf 0 für Modellidentifikation (unklar ob nötig für die JAGS-Implementation, schadet aber nicht, da true value des 13. Items eh 0)
beta <- iM
beta[] <- NA
rownames( beta ) <- paste0( "Y", 1:nrow(beta) )
# beta[13] <- 0

# Modell definieren
m <- ctglm.model( engine="jags", d=d, beta=beta, Lambda=LAMBDA, person.var=c("b"=TRUE,"mu.t1"=TRUE), track.person.par=c("bj","mu.t1.j"), measurement.model=binomial() )

# Run parameter, muss fuer richtigen run hochgesetzt werden, hinter hashtag was man mal probieren könnte
adapt <- 0
chains <- 1   # 8
iter <- 25000   # mehrere stufen: 100000; 250000, 500000
thin <- 125     # 250
burnin <- 100   # 100, 200, 400 Achtung, thin mit berücksichtigen
work.dir <- "n:/Dropbox/IQB/Projekte/ctirt/simdata/dats/sim109/10_simdat_25000_core8"
                                                                        
# Syntax generieren
s <- ctglm.syntax( m=m, model.name="TEST", cores=chains )
# s$call hat den call (inklusive Parallelisierung)
# s$syntax hat die JAGS Syntax

# Syntax speichern
# ctglm.save.syntax( s, "n:/Dropbox/IQB/Projekte/ctirt/simdata/dats/sim109/03_simdat_25000_core1/syntax_kopie" )

# Modell laufen lassen, braucht natürlich JAGS/rjags
r <- ctglm.run( s=s, work.dir=work.dir, iter=iter, thin=thin, chains=chains, adapt=adapt )

# get results
e <- ctglm.results( cores=1, r=r, burnin=burnin, plot.dir=work.dir, plot.person.par=FALSE )


### Hotfix: einlesen 
work.dirs <- paste0("n:/Dropbox/IQB/Projekte/ctirt/simdata/dats/sim109/",gsub(" ", "0", formatC(3:10,width=2)),"_simdat_25000_core", 1:8)
for ( i in work.dirs ) { 
      load(paste0(i, "/r.rda"))
      e <- ctglm.results( cores=1, r=r, burnin=burnin, plot.dir=i, plot.person.par=FALSE )
      write.csv2(e, paste0(i, "/results.csv"), na="", row.names = FALSE)}
      
      


