

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, tidyverse, gtools,
               sf, fs, glue, readxl)

# Load data ---------------------------------------------------------------
pnts <- read_excel('../points/durangensis.xlsx')
pnts <- pnts[,2:3]

# Read climate -----------------------------------------------------------
fles <- list.files('../raster', full.names = TRUE, pattern = '.tif$')
fles <- mixedsort(fles)
stck <- raster::stack(fles)

# Create a mask 
mask <- stck[[1]] * 0 + 1

# Remove duplicated by cells ---------------------------------------------
clls <- raster::extract(mask, pnts[,1:2], cellnumbers = TRUE)
clls <- xyFromCell(mask, clls[,'cells'])
dupv <- duplicated(clls[,c('x', 'y')])
head(dupv)
table(dupv)

pnts <- pnts[!dupv,]
head(pnts)

write.csv(pnts, '../points/durangensis_rmv.csv', row.names = F)

# Remove outliers ---------------------------------------------------------
vles <- raster::extract(stck, pnts[,1:2])
vles <- cbind(pnts, vles)

table(is.na(vles$bio_1))
head(vles)

vles <- drop_na(vles)
vles <- mutate(vles, gid = 1:nrow(vles))

head(vles)

vles <- dplyr::select(vles, gid, X, Y, everything())
head(vles)

boxplot(vles$bio_1)

vle2 <- gather(vles, variable, valor, -gid, -X, -Y)
vle2 <- as_tibble(vle2)
vle2 <- mutate(vle2, variable = factor(variable, levels = paste0('bio_', 1:19)))

# Boxplot with ggplot2
gbox <- ggplot(data = vle2, aes(x = 1, y = valor))  +
  geom_boxplot() + 
  facet_wrap(.~ variable, scales = 'free_y') +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
gbox

# Possible outliers -------------------------------------------------------
vles
library(outliers)
norm <- scores(vles[,4:ncol(vles)], 'z')
norm_na <- norm
norm_na[abs(norm_na) > 3.5] <- NA

head(norm_na)
post <- rownames(norm_na[is.na(norm_na$bio_1),])
vles[post,]

normpoints <- cbind(vles[,c('X', 'Y')], norm_na) %>% 
  na.omit() %>% 
  as_tibble()
nrow(vles) 
nrow(normpoints)

normpoints <- normpoints[,1:2]

# Extract the values
pnts_vles <- raster::extract(stck, normpoints[,1:2])
pnts_vles <- cbind(normpoints[,1:2], pnts_vles)
head(pnts_vles)

write.csv(pnts_vles, '../points/durangensis_rmOtl.csv', row.names = F)

# Correlation plot
library(corrplot)
m <- cor(pnts_vles[,3:ncol(pnts_vles)], method = 'pearson')# spearman
corrplot(m, method = 'circle')

cor.test
m
m[which(m < 0.7)] <- NA
corrplot(m, method = 'circle')

# Analisis VIF 
library(usdm)

mtrx <- pnts_vles[,3:ncol(pnts_vles)]
nrow(mtrx)
nrow(drop_na(mtrx))

vars <- vifstep(x = mtrx, th = 10)
vars <- vars@results$Variables

pnts_vles <- pnts_vles %>% dplyr::select(X, Y, vars) %>% as_tibble()

write.csv(pnts_vles, '../points/durangensis_rmOtl_vars.csv', row.names = F)


