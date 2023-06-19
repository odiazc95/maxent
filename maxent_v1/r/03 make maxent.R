

# Looad libraries 
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, 
               dismo, usdm, ENMeval, ecospat, rJava, gtools, 
               glue, fs)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
tbl <- read_csv('../points./prsc_bckn_vars.csv')
occ <- tbl %>% filter(pb == 1)
bck <- tbl %>% filter(pb == 0)

vrs <- colnames(tbl)[4:ncol(tbl)]

# Climate data
fls <- list.files('../raster', full.names = TRUE)
fls <- grep(paste0(vrs, collapse = '|'), fls, value = TRUE)
fls <- mixedsort(fls)
stk <- raster::stack(fls)

# Cross - validation ------------------------------------------------------
fld_occ <- kfold(occ, k = 25)
fld_bck <- kfold(bck, k = 25)

# A simple model ----
tst <- occ[fld_occ == 1,]
trn <- occ[fld_occ != 1,]
tst_bck <- bck[fld_bck == 1,]
trn_bck <- bck[fld_bck != 1,]

env <- rbind(trn, trn_bck)
y   <- c(trn$pb, trn_bck$pb)

out <- '../maxent/run_1'
dir.create('../maxent/run_1', recursive = TRUE)

mxn <- maxent(env[,4:ncol(env)], y, arcgs = c('addsamplestobackground=true'),
              path = out)

rst <- raster::predict(mxn, stk, progress = 'txt')
plot(rst)
