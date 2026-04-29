#!/usr/bin/Rscript

# Clear workspace
rm(list = ls())

#####################################################################
# Inclusion/Exclusion Criteria
#####################################################################

# Load 'data.table' package
if(!require("data.table")) install.packages("data.table"); library(data.table)

# Set input data directory
datadir <- scan(file = "Data/output_features.txt", what = "character", quiet = T)
if(!dir.exists(datadir)) stop("Directory must exists!")

cat("\nLoad data\n\n")
# Load data
dat_bs <- fread(file.path(datadir,"DepT0_allMRI.csv"))

# Data curation
# (Do not) Remove those with baseline depression
#dat_bs <- dat_bs[which(dat_bs$ksad_prnt_dep_T0==0),]
#dat_bs <- dat_bs[which(dat_bs$ksad_kid_dep_T0==0),]
# Remove those with bipolar diagnosis
dat_bs <- dat_bs[which(dat_bs$ksad_prnt_bip_T0==0),]
dat_bs <- dat_bs[which(dat_bs$ksad_kid_bip_T0==0),]
# Discard those with discordant parent/youth diagnosis
disc_idx <- which(dat_bs$ksad_prnt_dep_T0==0 & dat_bs$ksad_kid_dep_T0==1)
if(length(disc_idx)>0) dat_bs <- dat_bs[-disc_idx,]
# Remove those without parental history
prhist_idx <- which(dat_bs$father_his_dep==1 | dat_bs$mother_his_dep==1)
dat_bs <- dat_bs[prhist_idx,]

# T2 specific scrubbing
# Read T2 data
dat_T2 <- fread(file.path(datadir,"PrntKid_NDA_DepT2.txt"))
dat_T2 <- dat_T2[which(!is.na(dat_T2$ksad_prnt_dep_T2)),]
# Match datasets
id_match <- match(dat_bs$subjectkey,dat_T2$subjectkey)
# Remove NA's
dat_bs <- dat_bs[which(!is.na(id_match)),]
id_match <- match(dat_bs$subjectkey,dat_T2$subjectkey)
identical(dat_bs$subjectkey,dat_T2$subjectkey[id_match])
dat_bs <- cbind(dat_T2$ksad_prnt_dep_T2[id_match],dat_bs)
names(dat_bs)[1] <- "ksad_prnt_dep_T2"
# Remove those with baseline depression
dat_bs <- dat_bs[which(dat_bs$ksad_prnt_dep_T0==0),]
# Discard those with discordant parent/youth diagnosis
disc_idx <- which(dat_T2$ksad_prnt_dep_T2==0 & dat_T2$ksad_kid_dep_T2==1)
if(length(disc_idx)>0) dat_T2 <- dat_T2[-disc_idx,]

# Remove variables that are not going to be used in the prediction
dat_bs <- dat_bs[, -c(2:11,15:47), with = F]
# Remove variables with excesive NA's
for(ii in 1:ncol(dat_bs)) if(sum(is.infinite(dat_bs[[ii]]))>0) dat_bs[[ii]][which(is.infinite(dat_bs[[ii]]))] <- NA
na1 <- colSums(is.na(dat_bs))/nrow(dat_bs)
dat_bs <- dat_bs[, which(na1<0.05), with = F]
# Remove NA's
dat_bs <- dat_bs[complete.cases(dat_bs),]
# Remove sites without positive cases
site_ok <- c(names(which(table(dat_bs$ksad_prnt_dep_T2,dat_bs$site)[2,]>1)))
dat_bs <- dat_bs[which(!is.na(match(dat_bs$site, site_ok))),]
# Set factor levels
dat_bs$ksad_prnt_dep_T2 <- factor(dat_bs$ksad_prnt_dep_T2)
dat_bs$site <- factor(dat_bs$site)

# Set output directory
findir <- scan(file = "Data/output_samples.txt", what = "character", quiet = T)
if(!dir.exists(findir)) dir.create(findir, recursive = T)
# Save final sample
fwrite(dat_bs, file.path(findir,"HighRisk_T2.csv"))
