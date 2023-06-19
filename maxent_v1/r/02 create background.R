

# Looad libraries 
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, 
               gtools, dismo, fs, glue)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data
prsc <- read_csv('../points/durangensis_rmOtl_vars.csv')
mask <- raster('../raster/bio_1.tif') * 0 + 1
vars <- colnames(prsc)[3:ncol(prsc)]

# Get the coordinates where we have presences -----------------------------
clls <- raster::extract(mask, prsc[,1:2], cellnumbers = TRUE)
plot(mask)
mask[clls[,1]] <- NA
plot(mask)

tble <- rasterToPoints(mask, spatial = FALSE)
tble <- as_tibble(tble)

bckn <- sample_n(tbl = tble, size = nrow(prsc) * 2, replace = FALSE)
bckn <- bckn[,1:2]
write.csv(bckn, '../points/background.csv', row.names = FALSE)

vars <- paste0(vars, '.tif')

# Load climate ------------------------------------------------------------
fles <- list.files('../raster', full.names = TRUE)
fles <- grep(paste0(vars, collapse = '|'), fles, value = TRUE)
fles <- mixedsort(fles)

stck <- raster::stack(fles)
names(stck)

# Get the values of the climate for background ----------------------------
bckn <- cbind(bckn, raster::extract(stck, bckn[,1:2]))
head(bckn)


# Prepare the tables ------------------------------------------------------
colnames(prsc)
colnames(bckn)

colnames(prsc) <- colnames(bckn)

# Add and ID 
prsc$pb <- 1
bckn$pb <- 0

# Relocate the columns
prsc <- dplyr::select(prsc, pb, everything())
bckn <- dplyr::select(bckn, pb, everything())

allt <- rbind(prsc, bckn)
nrow(allt)
table(allt$pb)

write.csv(allt, '../points/prsc_bckn_vars.csv', row.names = FALSE)
