# create indexes
# adapted from: https://github.com/wachiuphd/CVI

# Install BiocManager for BioConductor Libraries
## Note: R >= 4.4 needed for BiocManager v. 3.19, 3.20 ; R >= 4.5 needed for later versions

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.20")

# Install, load BiocGenerics and S4Vectors

if (!require("BiocGenerics")) {BiocManager::install("BiocGenerics"); require("BiocGenerics")}
if (!require("S4Vectors")) {BiocManager::install("S4Vectors"); require("S4Vectors")}

# install, load CRAN libraries

if (!require("data.table")) {install.packages("data.table"); require("data.table")}
if (!require("toxpiR")) {install.packages("toxpiR"); require("toxpiR")}
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("GGally")) {install.packages("GGally"); require("GGally")}
if (!require("grid")) {install.packages("grid"); require("grid")}
if (!require("moments")) {install.packages("moments"); require("moments")}


######## Data post-processing
diagdir <- "Diagnostics"
if (!dir.exists(diagdir)) dir.create(diagdir)

# 10 colors, color-blind friendly
Tol_muted <- tolower(c('88CCEE', '44AA99', '117733', '332288', 'DDCC77', '999933','CC6677', '882255', 'AA4499', 'DDDDDD'))

# metadata used for labeling, imputation indicator, direction (i.e., adverse or not), etc.
#
## NOTE: rows for local data added manually - in row that corresponds to position in raw indicator data file
#
indicators.df<-fread("data-inter/CVI_indicators_current_locals_v1.csv")

# raw indicator values
## Note: 6 cols geo data, 116 cols indicator data (same as length of metadata data, indicators.df)
cvi.df<-fread("data-inter/det_indicator_data_tract_level_curated_geo_w_local.csv",
              keepLeadingZeros = TRUE,integer64 = "numeric")

# Diagnostics:
## Distributions, Q-Q Plot of raw values & log of values (if positive)
pdf(file.path(diagdir,"CheckDist.pdf"))
for (j in 1:nrow(indicators.df)) {
  par(mfcol=c(2,2))
  y <- cvi.df[[j+6]]
  mtitle <- paste("Row",j,"\n",indicators.df$Parameters[j],"\n",
                  paste(substr(names(summary(y)),1,4),collapse=" | "),"\n",
                  paste(signif(summary(y),3),collapse=" | "))
  y <- y[!is.na(y)]
  try({qqnorm(y,main="",pch=15,cex=0.2); qqline(y);})
  try(hist(y,main="",xlab="Value"))
  try({qqnorm(log(y[y>0]),main="",pch=15,cex=0.2); qqline(log(y[y>0]));})
  try(hist(log(y[y>0]),main="",xlab="Log Value [positive only]"))
  mtext(mtitle,outer=TRUE,line=-4,cex=0.75)
}
dev.off()

####  NAs / Missing values
## Whether NA's should be replaced with the median value or a 0 is indicated in the
## 'metadata' dataset (indicators.df), column "Replace NA with median"

# ID columns where NA should be replaced by median ("1")
nareplcols <- indicators.df$Parameters[indicators.df$`Replace NA with median`==1]
# View((base::apply(cvi.df,2,FUN=function(x) {sum(is.na(x))}))[nareplcols])

# review: number of NA's in each column where median will replace NA
print(as.numeric((base::apply(cvi.df,2,FUN=function(x) {sum(is.na(x))}))[nareplcols]))

##
# Replace NA with column median where indicated
cvi.df[, (nareplcols) := lapply(.SD, function(x) nafill(x, type = "const", fill = median(x, na.rm = TRUE)))
       , .SDcols = nareplcols]

# # review: number of NA's remaining in identified columns after replacement function
print(as.numeric((base::apply(cvi.df,2,FUN=function(x) {sum(is.na(x))}))[nareplcols]))

# ID columns where NA should be replaced by 0 ("0")
na0cols <- indicators.df$Parameters[indicators.df$`Replace NA with median`==0]

# other columns replace NA with 0
cvi.df[, (na0cols) := lapply(.SD, function(x) nafill(x, type = "const", fill = 0))
       , .SDcols = na0cols]
print(as.numeric((base::apply(cvi.df,2,FUN=function(x) {sum(is.na(x))}))))

cvi.dat.df <- cvi.df[,-(1:6)]

## Rank correlations
pdf(file.path(diagdir,"CVI-corr.pdf"))
cvi.abbr <- cvi.df[,-(1:6)]
names(cvi.abbr) <- abbreviate(names(cvi.abbr),minlength=8)
pp<-ggcorr(data=slice_sample(cvi.abbr,n=7000),  # sample does not make sense given there are fewer than 7000 obs
           method=c("pairwise.complete.obs","spearman"))
print(pp)
dev.off()

# Absolute value when adverse direction is NA (vs. 1 or -1)
na_adverse <- which(is.na(indicators.df$`Adverse Direction`))
if (length(na_adverse) > 0) {
  cvi.dat.df <- as.data.frame(cvi.dat.df)
  cvi.dat.df[,na_adverse] <- abs(cvi.dat.df[,na_adverse])
  cvi.dat.df <- as.data.table(cvi.dat.df)
  indicators.df$`Adverse Direction`[na_adverse] <- 1
}

# idcols for use in GUI
idcols_gui.df <- data.table(
  `row#` = 1:nrow(cvi.df),
  Name=paste0(cvi.df$GEOID.Tract), #(cvi.df$STATE,", ",cvi.df$County_Name,", ",cvi.df$GEOID.Tract),
  FIPS=cvi.df$GEOID.Tract,
  Source=cvi.df$LatLong
)

idcols_gis.df <- data.table(
  FIPS=cvi.df$GEOID.Tract,
  Name=paste0(cvi.df$GEOID.Tract), # (cvi.df$STATE,", ",cvi.df$County_Name,", ",cvi.df$GEOID.Tract),
  Source=cvi.df$LatLong
)

############ Percentiles
pctdir <- "CVI-pct"
if (!dir.exists(pctdir)) dir.create(pctdir)

# Standardize to percentile from 0 to 1
# ToxPi will treat NA as zero by default
cvi.pct.df<-sweep(cvi.dat.df,2,indicators.df$`Adverse Direction`,"*") # multiple by adverse direction
cvi.pct.df<-as.data.frame(base::apply(cvi.pct.df,2,rank,ties.method="min",na.last="keep")) # rank
cvi.pct.df<-sweep(cvi.pct.df-1,2,base::apply(cvi.pct.df,2,max,na.rm=T)-1,"/") # turn into percentile 0-1
pdf(file.path(pctdir,"CVI-pct.pdf"),height=10,width=7.5)
boxplot(as.list(cvi.pct.df),
        horizontal = TRUE,pars=list(outpch=15,cex=0.3))
dev.off()
cvi.pct.df<-cbind(idcols_gui.df, cvi.pct.df) # save for use by ToxPi GUI
fwrite(cvi.pct.df,file.path(pctdir,"CVI_data_pct.csv"),quote=TRUE)

# Simple ToxPi - by Category only - all indicators equal weights, each category equal weight
categories <- unique(indicators.df$`Category`)
indicators.bycat <- list()
for (i in 1:length(categories)) {
  onecat <- categories[i]
  indicators.bycat[[i]] <- indicators.df$Parameters[
    indicators.df$`Category`==onecat
  ]
}

f.slices <- TxpSliceList(Baseline.Health=
                           TxpSlice(txpValueNames = indicators.bycat[[1]]),
                         Baseline.SocialEconomic=
                           TxpSlice(txpValueNames = indicators.bycat[[2]]),
                         Baseline.Infrastructure=
                           TxpSlice(txpValueNames = indicators.bycat[[3]]),
                         Baseline.Environment=
                           TxpSlice(txpValueNames = indicators.bycat[[4]]),
                         ClimateChange.Health=
                           TxpSlice(txpValueNames = indicators.bycat[[5]]),
                         ClimateChange.SocialEconomic=
                           TxpSlice(txpValueNames = indicators.bycat[[6]]),
                         ClimateChange.ExtremeEvents=
                           TxpSlice(txpValueNames = indicators.bycat[[7]]))
f.model <-TxpModel(txpSlices = f.slices)

## Pct results
f.pct.results <- txpCalculateScores(model=f.model,
                                    input=cvi.pct.df,
                                    id.var="FIPS")

save(f.slices,f.model,f.pct.results,file=file.path(pctdir,"CVI-pct-allinone.Rdata"))
indx <- txpRanks(f.pct.results)<=10 |
  txpRanks(f.pct.results)>=(max(txpRanks(f.pct.results))-9)

# save for use by ToxPi GUI
cvi.pct.toxpi <- cbind(idcols_gui.df,
                       data.table(`ToxPi Score`=f.pct.results@txpScores),
                       data.table(f.pct.results@txpSliceScores))
fwrite(cvi.pct.toxpi,file.path(pctdir,"CVI-pct-allinone.csv"))
# save for use by ToxPi GIS
cvi.pct.toxpi.gis <- cbind(data.table(`ToxPi Score`=f.pct.results@txpScores),
                           idcols_gis.df,
                           data.table(f.pct.results@txpSliceScores))
slicenames <- names(f.slices@listData)
newslicenames <- paste0(slicenames,"!1","!0x",Tol_muted[1:length(slicenames)],"ff")
setnames(cvi.pct.toxpi.gis,slicenames,newslicenames)
fwrite(cvi.pct.toxpi.gis,file.path(pctdir,"CVI-pct-allinone.gis.csv"))

pdf(file.path(pctdir,"ToxPi-pct-allinone.pdf"),height=8,width=10)
plot(f.pct.results[indx,],fills=paste0("#",Tol_muted))
plot(f.pct.results,y=txpRanks(f.pct.results))
grid.text("Overall",y=0.95,x=0.5,just="top")
pp<-ggpairs(slice_sample(as.data.table(f.pct.results@txpSliceScores),
                         n=7000),
            lower = list(continuous = wrap("points", alpha=0.1, size=0.1)))
print(pp)
dev.off()

# Second level ToxPi - Subcategory ToxPis first

indicators.bysubcat.list <- list()
subcategories.list <- list()
f.slices.list <- list()
f.model.list <- list()
f.pct.results.list <- list()

# Each category separately
pdf(file.path(pctdir,"ToxPi-pct-subcat.pdf"),height=8,width=10)
for (i in 1:length(categories)) {
  onecat <- categories[i]
  subcategories <- unique(indicators.df$Subcategory[
    indicators.df$`Category`==onecat])
  indicators.bysubcat <- list()
  for (j in 1:length(subcategories)) {
    onesubcat <- subcategories[j]
    indicators.bysubcat[[j]] <- indicators.df$Parameters[
      indicators.df$`Category`==onecat &
        indicators.df$Subcategory==onesubcat
    ]
  }
  fcat.slices <- list()
  for (j in 1:length(subcategories)) {
    fcat.slices[[j]] <- TxpSlice(txpValueNames = indicators.bysubcat[[j]])
  }
  names(fcat.slices)<-subcategories
  fcat.slices<-as.TxpSliceList(fcat.slices)
  fcat.model <-TxpModel(txpSlices = fcat.slices)
  ## pct
  fcat.pct.results <- txpCalculateScores(model=fcat.model,
                                         input=cvi.pct.df,
                                         id.var="FIPS")
  save(fcat.slices,fcat.model,fcat.pct.results,file=file.path(pctdir,
                                                              paste0("CVI-pct-cat-",
                                                                     gsub(": ","-",onecat),".Rdata")))
  indx.pct <- txpRanks(fcat.pct.results)<=10 |
    txpRanks(fcat.pct.results)>=(max(txpRanks(fcat.pct.results))-9)
  plot(fcat.pct.results[indx.pct,],name=onecat,fills=paste0("#",Tol_muted))
  grid.text("Pct",y=0.05,x=0.5,just="top")
  plot(fcat.pct.results,y=txpRanks(fcat.pct.results),name=onecat)
  grid.text(paste("Pct",onecat),y=0.95,x=0.5,just="top")
  if (ncol(fcat.pct.results@txpSliceScores)>1) {
    pp<-ggpairs(slice_sample(as.data.table(fcat.pct.results@txpSliceScores),
                             n=7000),
                lower = list(continuous = wrap("points", alpha=0.1, size=0.1)))
    print(pp)
  }
  subcategories.list[[i]]<-subcategories
  indicators.bysubcat.list[[i]]<-indicators.bysubcat
  f.slices.list[[i]]<-fcat.slices
  f.model.list[[i]]<-fcat.model
  f.pct.results.list[[i]]<-fcat.pct.results
  # save for use by ToxPi GUI
  cvi.pct.toxpi.cat <- cbind(idcols_gui.df,
                             data.table(`ToxPi Score`=fcat.pct.results@txpScores),
                             data.table(fcat.pct.results@txpSliceScores))
  fwrite(cvi.pct.toxpi.cat,file.path(pctdir,
                                     paste0("CVI-pct-cat-",
                                            gsub(": ","-",onecat),".csv")))
  # save for use by ToxPi GIS
  cvi.pct.toxpi.cat.gis <- cbind(data.table(`ToxPi Score`=fcat.pct.results@txpScores),
                                 idcols_gis.df,
                                 data.table(fcat.pct.results@txpSliceScores))
  slicenames <- names(fcat.slices@listData)
  newslicenames <- paste0(slicenames,"!1","!0x",Tol_muted[1:length(slicenames)],"ff")
  setnames(cvi.pct.toxpi.cat.gis,slicenames,newslicenames)
  fwrite(cvi.pct.toxpi.cat.gis,file.path(pctdir,
                                         paste0("CVI-pct-cat-",
                                                gsub(": ","-",onecat),".gis.csv")))
}
dev.off()

## Combined
fcomb.slices <- TxpSliceList(Baseline.Health=
                               TxpSlice(txpValueNames = categories[1]),
                             Baseline.SocialEconomic=
                               TxpSlice(txpValueNames = categories[2]),
                             Baseline.Infrastructure=
                               TxpSlice(txpValueNames = categories[3]),
                             Baseline.Environment=
                               TxpSlice(txpValueNames = categories[4]),
                             ClimateChange.Health=
                               TxpSlice(txpValueNames = categories[5]),
                             ClimateChange.SocialEconomic=
                               TxpSlice(txpValueNames = categories[6]),
                             ClimateChange.ExtremeEvents=
                               TxpSlice(txpValueNames = categories[7]))
fcomb.model <-TxpModel(txpSlices = fcomb.slices)

pdf(file.path(pctdir,"ToxPi-pct-subcat-comb.pdf"),height=8,width=10)

## Pct
cvi.pct.cat.scores <- idcols_gui.df
for (i in 1:length(categories)) {
  cvi.pct.cat.scores <- cbind(cvi.pct.cat.scores,f.pct.results.list[[i]]@txpScores)
}
names(cvi.pct.cat.scores) <- c(names(idcols_gui.df),categories)
fcomb.pct.results <- txpCalculateScores(model=fcomb.model,
                                        input=cvi.pct.cat.scores,
                                        id.var="FIPS")
save(fcomb.slices,fcomb.model,fcomb.pct.results,
     file=file.path(pctdir,paste0("CVI-pct-comb.Rdata")))

indx.pct <- txpRanks(fcomb.pct.results)<=10 |
  txpRanks(fcomb.pct.results)>=(max(txpRanks(fcomb.pct.results))-9)

# save for use by ToxPi GUI
cvi.pct.toxpi.comb <- cbind(idcols_gui.df,
                            data.table(`ToxPi Score`=fcomb.pct.results@txpScores),
                            data.table(fcomb.pct.results@txpSliceScores))
fwrite(cvi.pct.toxpi.comb,file.path(pctdir,paste0("CVI-pct-comb.csv")))
# save for use by ToxPi GIS
cvi.pct.toxpi.comb.gis <- cbind(data.table(`ToxPi Score`=fcomb.pct.results@txpScores),
                                idcols_gis.df,
                                data.table(fcomb.pct.results@txpSliceScores))
slicenames <- names(fcomb.slices@listData)
newslicenames <- paste0(slicenames,"!1","!0x",Tol_muted[1:length(slicenames)],"ff")
setnames(cvi.pct.toxpi.comb.gis,slicenames,newslicenames)
fwrite(cvi.pct.toxpi.comb.gis,file.path(pctdir,paste0("CVI-pct-comb.gis.csv")))

plot(fcomb.pct.results[indx.pct,],fills=paste0("#",Tol_muted))
plot(fcomb.pct.results,y=txpRanks(fcomb.pct.results))
grid.text(paste("Pct","Overall"),y=0.95,x=0.5,just="top")
pp<-ggpairs(slice_sample(as.data.table(fcomb.pct.results@txpSliceScores),
                         n=7000),
            lower = list(continuous = wrap("points", alpha=0.1, size=0.1)))
print(pp)

dev.off()

## Just Baseline
## Combined
fcomb.slices <- TxpSliceList(Baseline.Health=
                               TxpSlice(txpValueNames = categories[1]),
                             Baseline.SocialEconomic=
                               TxpSlice(txpValueNames = categories[2]),
                             Baseline.Infrastructure=
                               TxpSlice(txpValueNames = categories[3]),
                             Baseline.Environment=
                               TxpSlice(txpValueNames = categories[4]))
fcomb.model <-TxpModel(txpSlices = fcomb.slices)

pdf(file.path(pctdir,"ToxPi-pct-subcat-comb-baseline.pdf"),height=8,width=10)

## Pct
cvi.pct.cat.scores <- idcols_gui.df
for (i in 1:length(categories)) {
  cvi.pct.cat.scores <- cbind(cvi.pct.cat.scores,f.pct.results.list[[i]]@txpScores)
}
names(cvi.pct.cat.scores) <- c(names(idcols_gui.df),categories)
fcomb.pct.results <- txpCalculateScores(model=fcomb.model,
                                        input=cvi.pct.cat.scores,
                                        id.var="FIPS")
save(fcomb.slices,fcomb.model,fcomb.pct.results,
     file=file.path(pctdir,paste0("CVI-pct-comb-baseline.Rdata")))

indx.pct <- txpRanks(fcomb.pct.results)<=10 |
  txpRanks(fcomb.pct.results)>=(max(txpRanks(fcomb.pct.results))-9)

# save for use by ToxPi GUI
cvi.pct.toxpi.comb <- cbind(idcols_gui.df,
                            data.table(`ToxPi Score`=fcomb.pct.results@txpScores),
                            data.table(fcomb.pct.results@txpSliceScores))
fwrite(cvi.pct.toxpi.comb,file.path(pctdir,paste0("CVI-pct-comb-baseline.csv")))
# save for use by ToxPi GIS
cvi.pct.toxpi.comb.gis <- cbind(data.table(`ToxPi Score`=fcomb.pct.results@txpScores),
                                idcols_gis.df,
                                data.table(fcomb.pct.results@txpSliceScores))
slicenames <- names(fcomb.slices@listData)
newslicenames <- paste0(slicenames,"!1","!0x",Tol_muted[1:length(slicenames)],"ff")
setnames(cvi.pct.toxpi.comb.gis,slicenames,newslicenames)
fwrite(cvi.pct.toxpi.comb.gis,file.path(pctdir,paste0("CVI-pct-comb-baseline.gis.csv")))

plot(fcomb.pct.results[indx.pct,],fills=paste0("#",Tol_muted))
plot(fcomb.pct.results,y=txpRanks(fcomb.pct.results))
grid.text(paste("Pct","Overall"),y=0.95,x=0.5,just="top")
pp<-ggpairs(slice_sample(as.data.table(fcomb.pct.results@txpSliceScores),
                         n=7000),
            lower = list(continuous = wrap("points", alpha=0.1, size=0.1)))
print(pp)

dev.off()

## Just Climate
## Combined
fcomb.slices <- TxpSliceList(ClimateChange.Health=
                               TxpSlice(txpValueNames = categories[5]),
                             ClimateChange.SocialEconomic=
                               TxpSlice(txpValueNames = categories[6]),
                             ClimateChange.ExtremeEvents=
                               TxpSlice(txpValueNames = categories[7]))
fcomb.model <-TxpModel(txpSlices = fcomb.slices)

pdf(file.path(pctdir,"ToxPi-pct-subcat-comb-climate.pdf"),height=8,width=10)

## Pct
cvi.pct.cat.scores <- idcols_gui.df
for (i in 1:length(categories)) {
  cvi.pct.cat.scores <- cbind(cvi.pct.cat.scores,f.pct.results.list[[i]]@txpScores)
}
names(cvi.pct.cat.scores) <- c(names(idcols_gui.df),categories)
fcomb.pct.results <- txpCalculateScores(model=fcomb.model,
                                        input=cvi.pct.cat.scores,
                                        id.var="FIPS")
save(fcomb.slices,fcomb.model,fcomb.pct.results,
     file=file.path(pctdir,paste0("CVI-pct-comb-climate.Rdata")))

indx.pct <- txpRanks(fcomb.pct.results)<=10 |
  txpRanks(fcomb.pct.results)>=(max(txpRanks(fcomb.pct.results))-9)

# save for use by ToxPi GUI
cvi.pct.toxpi.comb <- cbind(idcols_gui.df,
                            data.table(`ToxPi Score`=fcomb.pct.results@txpScores),
                            data.table(fcomb.pct.results@txpSliceScores))
fwrite(cvi.pct.toxpi.comb,file.path(pctdir,paste0("CVI-pct-comb-climate.csv")))
# save for use by ToxPi GIS
cvi.pct.toxpi.comb.gis <- cbind(data.table(`ToxPi Score`=fcomb.pct.results@txpScores),
                                idcols_gis.df,
                                data.table(fcomb.pct.results@txpSliceScores))
slicenames <- names(fcomb.slices@listData)
newslicenames <- paste0(slicenames,"!1","!0x",Tol_muted[1:length(slicenames)],"ff")
setnames(cvi.pct.toxpi.comb.gis,slicenames,newslicenames)
fwrite(cvi.pct.toxpi.comb.gis,file.path(pctdir,paste0("CVI-pct-comb-climate.gis.csv")))

plot(fcomb.pct.results[indx.pct,],fills=paste0("#",Tol_muted))
plot(fcomb.pct.results,y=txpRanks(fcomb.pct.results))
grid.text(paste("Pct","Overall"),y=0.95,x=0.5,just="top")
pp<-ggpairs(slice_sample(as.data.table(fcomb.pct.results@txpSliceScores),
                         n=7000),
            lower = list(continuous = wrap("points", alpha=0.1, size=0.1)))
print(pp)

dev.off()


## Percentile file for GIS
cvi.pct.df.namesfixed <- cvi.pct.df
names(cvi.pct.df.namesfixed)<-gsub(",","",names(cvi.pct.df.namesfixed))
names(cvi.pct.df.namesfixed)<-gsub("\"","",names(cvi.pct.df.namesfixed))
fwrite(cvi.pct.df.namesfixed,file.path(pctdir,"CVI_data_pct.gis.csv"))

