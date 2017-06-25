#Data frame preparation code for Brook's camera/plot work

# Packages (Install) ----
install.packages("magrittr")
install.packages("digest")
install.packages("lubridate")
install.packages("splitstackshape")
install.packages("data.table")
install.packages("devtools") #Requires strawberry perl to be installed (mac and linux users already have this)

# Packages (Library) ----
library(data.table)
library(lubridate)
library(splitstackshape)
library(stringr)
library(ggplot2)
library(ggmap) #I doubt we need this package anymore
library(rgdal) #what would we have been using this package for? Used for projections (maps)?
library(raster)
library(dplyr)
devtools::install_github("paleolimbot/exifr") #Requires devtools to be installed
library(exifr)

#Directories ----
imagedir<-"c:/Users/Stanleyr/Dropbox/CISP/"
imagedir <- "c:/Users/BeauliauBrook/Dropbox/CISP/"
imagedir <- "c:/Users/Tavery44/Dropbox/CISP?"

#Important Tutorials ----
####http://genomicsclass.github.io/book/pages/dplyr_tutorial.html

#Site Data Frame Upload and Summary Statistics ----
#setwd("~/CISP")
sites <- read.csv("CamSites.csv", stringsAsFactors = F)

 ##General Checks
str(sites)
table(sites$site)
table(sites$cam)
table(sites$lat)
table(sites$long)
table(sites$northing)
table(sites$easting)
table(sites$datum)
table(sites$utm)
table(sites$inst_d)
table(sites$inst_m)
table(sites$inst_y)
table(sites$camrem_d)
table(sites$camrem_m)
table(sites$camrem_y)
table(sites$camrem)
table(sites$siterem)
table(sites$location)
table(sites$purpose)
table(sites$streamside)test<-testblank %>%
  filter(date != "", image == "") %>%
  data.frame()

#with(sites, table(c(cam, lat, long, northing, easting, datum, utm, inst_d, inst_m, inst_y, camrem_d, camrem_m, camrem_y, camrem, sitenew, siterem)))

##Initial survey period preparations
###Merging install and removal dates and converting to date format
sites$instdate <- as.Date(with(sites, paste(inst_y, inst_m, inst_d,sep="-")), "%Y-%m-%d")
str(sites$instdate)

sites$remdate <- as.Date(with(sites, paste(camrem_y, camrem_m, camrem_d,sep="-")), "%Y-%m-%d")
str(sites$remdate)

###Duplicating removal date column and filling NAs with cut off date
sites$remdatefill <- sites$remdate

sites$remdatefill[is.na(sites$remdatefill)] <- "2015-05-07"

###Calculating date difference between camera install and removal times
sites$daysdeployed <- as.integer(difftime(sites$remdatefill, sites$instdate, units = "days"))

###Calculating total days cameras were deployed (across all sites)
sites <- sites %>%
  mutate(daysdeployed_total = sum(daysdeployed)) #unnecessary line?

##Deployment times, means, stdev, range by camera, site, total
###Deployment periods per site and per site per camera
propsites <- sites %>%
  group_by(site, cam) %>%
  summarise(daysdeployed_cam = sum(daysdeployed)) %>%
  mutate(daysdeployed_site = sum(daysdeployed_cam)) %>%
  mutate(daysdeployed_total = sum(sites$daysdeployed)) %>%
  mutate(daysdeployed_camsite_percent = daysdeployed_cam/daysdeployed_site*100) %>%
  mutate(daysdeployed_camtotal_percent = daysdeployed_cam/daysdeployed_total*100) %>%
  mutate(daysdeployed_sitetotal_percent = daysdeployed_site/daysdeployed_total*100)%>%
  data.frame() #per camera and per site proportions of deployment total

###Deployment ranges, mean, stdev
statswithinsites <- propsites %>%
  group_by(site) %>%
  summarise(cammin = min(daysdeployed_cam), cammax = max(daysdeployed_cam), cammean = mean(daysdeployed_cam), camsd = sd(daysdeployed_cam)) %>%
  data.frame()

totaltable <- propsites %>%
  select(site, daysdeployed_site) %>% 
  data.frame()

totaltable <- totaltable[!duplicated(totaltable), ] #row numbers are awkward

statsbetweensites <- totaltable %>%
  summarise(sitemin = min(daysdeployed_site), sitemax = max(daysdeployed_site), sitemean = mean(daysdeployed_site), sitesd = sd(daysdeployed_site)) %>%
  data.frame() #desire range() instead?

##Deployment times, means, stdev, range by location, purpose, streamside
###Creating necessary data frame of sites, daysdeployed per site, and total daysdeployed
groupedsites <- sites %>%
  select(site, cam, location, purpose, streamside, daysdeployed, daysdeployed_total)

groupedsites <- groupedsites %>%
  group_by(site) %>%
  mutate(daysdeployed_site = sum(daysdeployed)) %>%
  data.frame()
  
groupedsites <- groupedsites[!duplicated(groupedsites$site), ]
groupedsites$cam <- NULL
groupedsites$daysdeployed <- NULL
groupedsites <-groupedsites[, c("site", "location", "purpose", "streamside", "daysdeployed_site", "daysdeployed_total")]
####The above is a crude method of accomplishing what I needed, if anyone has a better way of doing it let me know

###Deployment proportion for site per location, purpose, streamside and per location, purpose, streamside
locationprop <- groupedsites %>%
  group_by(location) %>%
  mutate(daysdeployed_location = sum(daysdeployed_site)) %>%
  mutate(daysdeployed_camlocation_percent = daysdeployed_site/daysdeployed_location*100) %>%
  mutate(daysdeployed_locationtotal_percent = daysdeployed_location/daysdeployed_total*100) %>%
  data.frame()

purposeprop <- groupedsites %>%
  group_by(purpose) %>%
  mutate(daysdeployed_purpose = sum(daysdeployed_site)) %>%
  mutate(daysdeployed_campurpose_percent = daysdeployed_site/daysdeployed_purpose*100) %>%
  mutate(daysdeployed_purposetotal_percent = daysdeployed_purpose/daysdeployed_total*100) %>%
  data.frame()

streamsideprop <- groupedsites %>%
  group_by(streamside) %>%
  mutate(daysdeployed_streamside= sum(daysdeployed_site)) %>%
  mutate(daysdeployed_camstreamside_percent = daysdeployed_site/daysdeployed_streamside*100) %>%
  mutate(daysdeployed_streamsidetotal_percent = daysdeployed_streamside/daysdeployed_total*100) %>%
  data.frame()

###Deployment sum, mean, sd, range per location, purpose, streamside
locationstats <- groupedsites %>%
  group_by(location) %>%
  summarise(daysdeployed_location = sum(daysdeployed_site), locationmin = min(daysdeployed_site), locationmax = max(daysdeployed_site), locationmean = mean(daysdeployed_site), locationsd = sd(daysdeployed_site)) %>%
  data.frame()

purposestats <- groupedsites %>%
  group_by(purpose) %>%
  summarise(daysdeployed_purpose = sum(daysdeployed_site), purposemin = min(daysdeployed_site), purposemax = max(daysdeployed_site), purposemean = mean(daysdeployed_site), purposesd = sd(daysdeployed_site)) %>%
  data.frame()

streamsidestats <- groupedsites %>%
  group_by(streamside) %>%
  summarise(daysdeployed_streamside = sum(daysdeployed_site), streamsidemin = min(daysdeployed_site), streamsidemax = max(daysdeployed_site), streamsidemean = mean(daysdeployed_site), streamsidesd = sd(daysdeployed_site)) %>%
  data.frame()

#Image Data Frame Test Code ----

setwd("~/Dropbox/CISP")

##Checking for hidden files
###Build df with hidden and without
baseimagedftest <- as.data.frame((list.files("imagesin", full.names = T, recursive = T)))
names(baseimagedftest) <- "x"
baseimagedftest$x <- as.character(baseimagedftest$x)

baseimagedftesthidden <- as.data.frame(list.files("imagesin", all.files = T, full.names = T, recursive = T))
names(baseimagedftesthidden) <- "x"
baseimagedftesthidden$x <- as.character(baseimagedftesthidden$x)
baseimagedftesthidden <- as.data.frame(as.character(baseimagedftesthidden$x))

###Compare hidden vs no hidden to see if only difference
hiddentest <- anti_join(baseimagedftesthidden, baseimagedftest) ####Only difference is .test.txt, confirm produced image data frame has no hidden files with below line

hiddentest <- filter(baseimagedftesthidden, grepl("/.", x, fixed=TRUE)) #USE TO CHECK MAIN IMAGE DATA FRAME

###Test for blank folders
####Build data frame showing image list with directory rows, split camera date out of it
baseimagedftestblank <- as.data.frame(list.files("imagesin", full.names = T, recursive = T, include.dirs = T))
names(baseimagedftestblank) <- "directory"

testblanksplit <- as.data.frame(str_split_fixed(baseimagedftestblank$directory, "/", 4))
names(testblanksplit) <- c("containing", "camera", "cameradate", "image")
testblankdate <- as.data.frame(str_split_fixed(testblanksplit$cameradate, "_", 2))
testblankdate$V1 <- NULL

testblank <- cbind(testblanksplit, testblankdate)
names(testblank) <- c("containing", "camera", "cameradate", "image", "date")

testblank$camera <- as.character(testblank$camera)
testblank$cameradate <- as.character(testblank$cameradate)
testblank$image <- as.character(testblank$image)
testblank$date <- as.character(testblank$date)

####Attempts to check for blank folders
#####Head row of each folder
folderheads <- testblank %>%
  filter(date != "", image == "") %>%
  data.frame()

#####Blank folder check
folderrowssum <- testblank %>%
  group_by(camera,cameradate) %>%
  summarise(nimage=n()) %>%
  data.frame()

blankfolders <- folderrowssum %>%
  filter(cameradate != "", nimage == 1)

###Build exif data using image list
baseimagedftestdir <- (list.files("imagesin", full.names = T,recursive = T, include.dirs = T))

dfout <- NULL

starti <- 1
chunksize <- 200

while(starti <= length(baseimagedftestdir)) {
  chunkfiles <- baseimagedftestdir[starti:min(starti+chunksize, length(baseimagedftestdir))]
  message("Processing chunk ", starti, ":", min(starti+chunksize, length(baseimagedftestdir)))
  starti <- starti + chunksize + 1
  tryCatch({
    if(is.null(dfout)) {
      dfout <- exifr(chunkfiles, quiet=F)
    } else {
      chunkfiles <- chunkfiles[!(chunkfiles %in% dfout$FileName)]
      dfout <<- plyr::rbind.fill(dfout, exifr(chunkfiles, quiet=F))
    }
  }, error=function(err) {message("Error processing chunk prior to index ", starti)})
}

imageexifdftestdir <- dfout ####Note errors when running dfout

str(imageexifdftestdir)

###Tests for 0kb files, .txt files, other file types
table(imageexifdftestdir$FileSize)
test <- filter(imageexifdftestdir, FileSize <= 41)
table(imageexifdftestdir$Error)
test <- filter(imageexifdftestdir, Error != "")
table(imageexifdftestdir$FileType)
test <- filter(imageexifdftestdir, FileType == "") ####Few different strategies of trying to detect other issues

###Tests for rows with no collection date
table(imageexifdftestdir$DateTimeOriginal)
test <- filter(imageexifdftestdir, DateTimeOriginal == "")

###Investigating exifr errors
####Only errors if non-images ran through
####Only chunks with non-images showed warning messages
####Date extraction from images post non-image lines are fine (sometiems off by 1 second), assume no added issues having non .jpg rows

#Create image data frame and attempt to join with site data frame
##Build image data frame again
rm(list = ls())

imagedftest <- (list.files("imagesin", full.names = T,recursive = T))

dfout <- NULL

starti <- 1
chunksize <- 200

while(starti <= length(imagedftest)) {
  chunkfiles <- imagedftest[starti:min(starti+chunksize, length(imagedftest))]
  message("Processing chunk ", starti, ":", min(starti+chunksize, length(imagedftest)))
  starti <- starti + chunksize + 1
  tryCatch({
    if(is.null(dfout)) {
      dfout <- exifr(chunkfiles, quiet=F)
    } else {
      chunkfiles <- chunkfiles[!(chunkfiles %in% dfout$FileName)]
      dfout <<- plyr::rbind.fill(dfout, exifr(chunkfiles, quiet=F))
    }
  }, error=function(err) {message("Error processing chunk prior to index ", starti)})
}

imagedftest <- dfout
str(imagedftest)

imagedftest <- imagedftest[, c("Directory", "FileName", "DateTimeOriginal")] ####Removing some unneccessary columns during initial tests

###Modify datetimeorignal to be posixct
imagedftest$DateTimeOriginal<-ymd_hms(imagedftest$DateTimeOriginal, tz="America/Halifax")
######Do we need to change file names? Do we need to reorder columns? Do we need to reorder rows?
#####Split directory into camera and folder date and add back in
imagedftestfolder <- as.data.frame(str_split_fixed(imagedftest$Directory, "/", 3))
table(imagedftest$Directory)
table(imagedftestfolder$V3) #####Check that the two columns match
imagedftestfolderdate <- as.data.frame(str_split_fixed(imagedftestfolder$V3, "_", 2))
table(imagedftestfolder$V3)
table(imagedftestfolderdate$V2)
names(imagedftestfolderdate) <- c("cam", "folderdate")
imagedftest <- cbind(imagedftest, imagedftestfolderdate)
imagedftest$Directory <- NULL


#####Fix cam column
camcolfix <- as.data.frame(str_split_fixed(imagedftest$cam, "Camera", 2))
camcolfix$V1 <- NULL
names(camcolfix) <- "camtest"
imagedftest<- cbind(imagedftest, camcolfix)
imagedftest$cam <- NULL
names(imagedftest) <- c("FileName", "DateTimeOriginal", "folderdate",  "cam")

#####Convert folderdate to date format
imagedftest$folderdate <- dmy(imagedftest$folderdate)
str(imagedftest)

#####Rearrange data frame
imagedftest <- imagedftest[, c("cam", "folderdate", "FileName", "DateTimeOriginal")]
imagedftest <- arrange(imagedftest, cam, folderdate, FileName)

##Build site data frame and prepare it
sites <- read.csv("~/CISP/CamSites.csv")
sites$cam <- as.factor(sites$cam)
str(sites)

###Merging install and removal dates and converting to date format
sites$instdate <- as.Date(with(sites, paste(inst_y, inst_m, inst_d,sep="-")), "%Y-%m-%d")
str(sites$instdate)

sites$remdate <- as.Date(with(sites, paste(camrem_y, camrem_m, camrem_d,sep="-")), "%Y-%m-%d")
str(sites$remdate)

###Duplicating removal date column and filling NAs with cut off date
sites$remdatefill <- sites$remdate

sites$remdatefill[is.na(sites$remdatefill)] <- "2015-05-07"

###Clear some unneeded columns from site data frame for testing
sites <- sites[, c("site", "cam", "northing", "easting", "instdate", "remdatefill")]

######Temporary Cheat
names(imagedftest) <- c("cam", "instdate", "image", "imagedate")
imagedftest$remdatefill <- imagedftest$instdate

##Attempting to set up a rolling join
setDT(imagedftest) #set data.table x
setDT(sites) #set data.table y
setkey(sites, cam, instdate, remdatefill) #set a key with trap_id for match, and date range as last two variables
imagesite<-foverlaps(imagedftest, sites, type="within")
imagesite<-as.data.frame(imagesite)
str(imagesite) #Check structure, note i.variables

#####write.csv(test, "test")
####Above code correctly paired lines from both data frames
#Creating folder start column
##Renaming columns, clearing unnecessary, and reorganizing columns
names(imagesite) <- c("cam", "site", "northing", "easting", "instdate", "remdate", "folderend", "image", "imagedate", "folderend2")
imagesite <- imagesite[, c("site", "cam", "northing", "easting", "instdate", "remdate", "image", "imagedate", "folderend")] #####Okay to remove columns this way?
str(imagesite)

#Image Data Frame building code ----
##Initial image data frame list
imagesite <- imagesite %>%
  group_by(site) %>%
  mutate(teststart = lag(folderend, 1)) %>%
  data.frame()
-----------------------------
#2) create endpdate for trapsite range
trapsite2<-trapsite2 %>% group_by(trap_id) %>% 
  mutate(endpdate = lead(startpdate, 1)) %>% 
  data.frame()

#3) fill in NA in endpdate with last sampling date
#For now, 1 Dec 2016 (it doesn't really matter, as long as after last sampling date)
trapsite2<-trapsite2 %>%
  mutate(endpdate = ifelse(is.na(endpdate),as.Date("2016-12-01"),endpdate)) %>%
  mutate(endpdate = as.Date(.[,"endpdate"], origin = "1970-01-01")) %>% 
  data.frame()

#Check to see if date ranges worked!
#Startdate/enddate are dates when traps were at specific coordinates
#Traps changed positions, this shows between what dates
ep<-data.frame(trapsite2$trap_id,trapsite2$startpdate,trapsite2$endpdate)
---------------------------------------------------------
##Attaching exif data to images
#Note: See "brook_thesiscodeall_6may2016.Rmd" for Ryan's code for this stuff, in case we decide we wish to utilise that.

setwd("Z:/Animal Passage Files")
setwd("/media/Brook/Animal Passage Files/")

camerafilesraw <- list.files("/media/Brook/Animal Passage Files/FieldCameras", full.names=T, recursive = T)

write.table(camerafilesraw, "camerafilesraw")

dfout <- NULL

starti <- 1
chunksize <- 200

while(starti <= length(camerafilesraw)) {
chunkfiles <- camerafilesraw[starti:min(starti+chunksize, length(camerafilesraw))]
message("Processing chunk ", starti, ":", min(starti+chunksize, length(camerafilesraw)))
starti <- starti + chunksize + 1
tryCatch({
if(is.null(dfout)) {
dfout <- exifr(chunkfiles, quiet=F)
} else {
chunkfiles <- chunkfiles[!(chunkfiles %in% dfout$FileName)]
dfout <<- plyr::rbind.fill(dfout, exifr(chunkfiles, quiet=F))
}
}, error=function(err) {message("Error processing chunk prior to index ", starti)})
}

##Expanding dates, cleaning image data frame
###Checking output data frame for possible errors
#Possible code for checking for errors in image data frame ----
#Is cameradate blank?
blanks <- images[images$cameradate == "", ]

#Querying for instances of folders with no images
empty <- rownames(table(images$cameradate)[which(table(images$cameradate) == 1)])

#Non .jpg checks
wtf<- filter(images, !grepl(".JPG", image))

#Ryan code for detecting blank entries
####Ryan code for detecting instances of blank entries (folders with no data). Not all of this is needed, but lots of it would be for retesting. Included all here till can parse it out.
#Ryan's Code#
blankscheck <- images[!duplicated(images$cameradate), ] #Just a quick check that the first line for each entry in cameradate is indeed blank in the image column. Comparing the the images data frame, the few rows checked from the blanscheck data frame were indeed blank in the rows from the images data frame. Should have worked.#

subimages <- images[duplicated(images$cameradate),"cameradate"] #Returns a string of repeated cameradate entries (exluding ones not duplicated). This effectively removes all non duplicated cameradate entries.#

subimages <- subset(images,cameradate %in% subimages) #Subset the dataframe to just those entries contained in the character string, which will exclude all rows that only exist once. appears larger that above line because it is including the first row for each cameradate entry where image is blank#

trueempty <- subset(images, !(cameradate %in% subimages)) #running a reverse of the above line to see all folders that are empty# Manually checked a few of the indicated folders, were empty. Suggests working as intended.

subimages <- subimages[!duplicated(subimages$cameradate),] #Pullout first entry of each duplicated cameradate, excluding single entries due to the above lines.#

Now that we have a data frame of the rows that are not desired in the images data frame. We need to remove them.
```{r, eval= FALSE}
#DO NOT RUN NORMALLY# images$index=row.names(images) # this will create a vector of row.names which corresponds to the subimages

row.names(images)=NULL # get rid of the confusing row names which are a vestiage of the old dataframe
images <- images[which(!images$index %in% as.numeric(row.names(subimages))),] #Saying return all rows where images$index does not line up with row.names of subimages (essentially)#

1020 rows were removed, which is the amount in subimages. Also, the first several rows that were supposed to be removed are indeed removed, suggesting this worked correctly.

Now to create some data frames where .JPG and "" are removed.

noimages <- filter(images, !grepl(".JPG", image))

noblanks <- filter(noimages, !grepl("", image)) #Not working#

Noticed that there were files that required removal (the older .DS_Store and other hidden files). These will need to be removed and the data frame rebuilt.

Exporting the noblanks data frame to use as a basis for investigating texts and blank folders.

write.csv(noimages, "blankfolders.csv")

Resolving the differences between FieldCamera folders in server and backup

cameradirserv <- "Z:/Animal Passage Files/FieldCameras"
cameradirbackup <- "W:/BBBackup/FieldCameras"

imagesserv <- as.data.frame(list.files(cameradirserv, all.files = TRUE, full.names = FALSE, recursive = TRUE, include.dirs = TRUE))

write.csv(imagesserv, file = "servercameraimages")

imagesbackup <- as.data.frame(list.files(cameradirbackup, all.files = TRUE, full.names = FALSE, recursive = TRUE, include.dirs = TRUE))

write.csv(imagesbackup, file = "backupcameraimages")

###Splitting data frame directory levels
names(images) <- "directory"
imagessplit<-as.data.frame(str_split_fixed(images$directory, "/", 3))
names(imagessplit) <- c("camera", "cameradate", "image")
head(imagessplit)

###Isolating date from camera date
datesplit <- as.data.frame(str_split_fixed(imagessplit$cameradate, "_", 2))
head(datesplit)

###Dropping the Camera column#
datesplit$V1 <- NULL

###Binding 'datesplit' to 'imagessplit'
datebind <- cbind(imagessplit, datesplit)
names(datebind) <- c("camera", "cameradate", "image", "date")
datebind$camera <- as.character(datebind$camera)
datebind$cameradate <- as.character(datebind$cameradate)
datebind$image <- as.character(datebind$image)
datebind$date <- as.character(datebind$date)
parseddate <- as.data.frame(dmy(datebind$date))
names(parseddate) <- ("date")
datesplit <- as.data.frame(str_split_fixed(parseddate$date, "-", 3))
names(datesplit) <- c("year", "month", "day")
mount -t cifs -o username=bbeauliua password=8T3^gam8ty //kraken.acadiau.ca/bbeauliua /media/Brook
imagessplit <- cbind(imagessplit, datesplit)

#Random lines of code ----
ggsave(paste(imagedir,"imagename.png",""), ggplotimage)
#Placeholder for Trevor's demonstration line, delete or modify later

#Potentially useful things in "brook_thesiscodeall_6may2016.Rmd"
-code to check differences between the image server and a backup file (not sure if completed)

-Test code for examinining images pre/post a certain date.

Side note: To check file types, would it be logical to do a str_split based on "." to create a column of file types? Should we be creating that column anyways?
#Data frame preparation code for Brook's camera/plot work

# Packages (Install) ----
install.packages("magrittr")
install.packages("digest")
install.packages("lubridate")
install.packages("splitstackshape")
install.packages("data.table")
install.packages("devtools") #Requires strawberry perl to be installed (mac and linux users already have this)

# Packages (Library) ----
library(data.table)
library(lubridate)
library(splitstackshape)
library(stringr)
library(ggplot2)
library(ggmap) #I doubt we need this package anymore
library(rgdal) #what would we have been using this package for? Used for projections (maps)?
library(raster)
library(dplyr)
devtools::install_github("paleolimbot/exifr") #Requires devtools to be installed
library(exifr)

#Directories ----
imagedir<-"c:/Users/Stanleyr/Dropbox/CISP/"
imagedir <- "c:/Users/BeauliauBrook/Dropbox/CISP/"
imagedir <- "c:/Users/Tavery44/Dropbox/CISP?"

#Important Tutorials ----
####http://genomicsclass.github.io/book/pages/dplyr_tutorial.html

#Site Data Frame Upload and Summary Statistics ----
#setwd("~/CISP")
sites <- read.csv("CamSites.csv", stringsAsFactors = F)

##General Checks
str(sites)
table(sites$site)
table(sites$cam)
table(sites$lat)
table(sites$long)
table(sites$northing)
table(sites$easting)
table(sites$datum)
table(sites$utm)
table(sites$inst_d)
table(sites$inst_m)
table(sites$inst_y)
table(sites$camrem_d)
table(sites$camrem_m)
table(sites$camrem_y)
table(sites$camrem)
table(sites$siterem)
table(sites$location)
table(sites$purpose)
table(sites$streamside)test<-testblank %>%
  filter(date != "", image == "") %>%
  data.frame()

#with(sites, table(c(cam, lat, long, northing, easting, datum, utm, inst_d, inst_m, inst_y, camrem_d, camrem_m, camrem_y, camrem, sitenew, siterem)))

##Initial survey period preparations
###Merging install and removal dates and converting to date format
sites$instdate <- as.Date(with(sites, paste(inst_y, inst_m, inst_d,sep="-")), "%Y-%m-%d")
str(sites$instdate)

sites$remdate <- as.Date(with(sites, paste(camrem_y, camrem_m, camrem_d,sep="-")), "%Y-%m-%d")
str(sites$remdate)

###Duplicating removal date column and filling NAs with cut off date
sites$remdatefill <- sites$remdate

sites$remdatefill[is.na(sites$remdatefill)] <- "2015-05-07"

###Calculating date difference between camera install and removal times
sites$daysdeployed <- as.integer(difftime(sites$remdatefill, sites$instdate, units = "days"))

###Calculating total days cameras were deployed (across all sites)
sites <- sites %>%
  mutate(daysdeployed_total = sum(daysdeployed)) #unnecessary line?

##Deployment times, means, stdev, range by camera, site, total
###Deployment periods per site and per site per camera
propsites <- sites %>%
  group_by(site, cam) %>%
  summarise(daysdeployed_cam = sum(daysdeployed)) %>%
  mutate(daysdeployed_site = sum(daysdeployed_cam)) %>%
  mutate(daysdeployed_total = sum(sites$daysdeployed)) %>%
  mutate(daysdeployed_camsite_percent = daysdeployed_cam/daysdeployed_site*100) %>%
  mutate(daysdeployed_camtotal_percent = daysdeployed_cam/daysdeployed_total*100) %>%
  mutate(daysdeployed_sitetotal_percent = daysdeployed_site/daysdeployed_total*100)%>%
  data.frame() #per camera and per site proportions of deployment total

###Deployment ranges, mean, stdev
statswithinsites <- propsites %>%
  group_by(site) %>%
  summarise(cammin = min(daysdeployed_cam), cammax = max(daysdeployed_cam), cammean = mean(daysdeployed_cam), camsd = sd(daysdeployed_cam)) %>%
  data.frame()

totaltable <- propsites %>%
  select(site, daysdeployed_site) %>% 
  data.frame()

totaltable <- totaltable[!duplicated(totaltable), ] #row numbers are awkward

statsbetweensites <- totaltable %>%
  summarise(sitemin = min(daysdeployed_site), sitemax = max(daysdeployed_site), sitemean = mean(daysdeployed_site), sitesd = sd(daysdeployed_site)) %>%
  data.frame() #desire range() instead?

##Deployment times, means, stdev, range by location, purpose, streamside
###Creating necessary data frame of sites, daysdeployed per site, and total daysdeployed
groupedsites <- sites %>%
  select(site, cam, location, purpose, streamside, daysdeployed, daysdeployed_total)

groupedsites <- groupedsites %>%
  group_by(site) %>%
  mutate(daysdeployed_site = sum(daysdeployed)) %>%
  data.frame()

groupedsites <- groupedsites[!duplicated(groupedsites$site), ]
groupedsites$cam <- NULL
groupedsites$daysdeployed <- NULL
groupedsites <-groupedsites[, c("site", "location", "purpose", "streamside", "daysdeployed_site", "daysdeployed_total")]
####The above is a crude method of accomplishing what I needed, if anyone has a better way of doing it let me know

###Deployment proportion for site per location, purpose, streamside and per location, purpose, streamside
locationprop <- groupedsites %>%
  group_by(location) %>%
  mutate(daysdeployed_location = sum(daysdeployed_site)) %>%
  mutate(daysdeployed_camlocation_percent = daysdeployed_site/daysdeployed_location*100) %>%
  mutate(daysdeployed_locationtotal_percent = daysdeployed_location/daysdeployed_total*100) %>%
  data.frame()

purposeprop <- groupedsites %>%
  group_by(purpose) %>%
  mutate(daysdeployed_purpose = sum(daysdeployed_site)) %>%
  mutate(daysdeployed_campurpose_percent = daysdeployed_site/daysdeployed_purpose*100) %>%
  mutate(daysdeployed_purposetotal_percent = daysdeployed_purpose/daysdeployed_total*100) %>%
  data.frame()

streamsideprop <- groupedsites %>%
  group_by(streamside) %>%
  mutate(daysdeployed_streamside= sum(daysdeployed_site)) %>%
  mutate(daysdeployed_camstreamside_percent = daysdeployed_site/daysdeployed_streamside*100) %>%
  mutate(daysdeployed_streamsidetotal_percent = daysdeployed_streamside/daysdeployed_total*100) %>%
  data.frame()

###Deployment sum, mean, sd, range per location, purpose, streamside
locationstats <- groupedsites %>%
  group_by(location) %>%
  summarise(daysdeployed_location = sum(daysdeployed_site), locationmin = min(daysdeployed_site), locationmax = max(daysdeployed_site), locationmean = mean(daysdeployed_site), locationsd = sd(daysdeployed_site)) %>%
  data.frame()

purposestats <- groupedsites %>%
  group_by(purpose) %>%
  summarise(daysdeployed_purpose = sum(daysdeployed_site), purposemin = min(daysdeployed_site), purposemax = max(daysdeployed_site), purposemean = mean(daysdeployed_site), purposesd = sd(daysdeployed_site)) %>%
  data.frame()

streamsidestats <- groupedsites %>%
  group_by(streamside) %>%
  summarise(daysdeployed_streamside = sum(daysdeployed_site), streamsidemin = min(daysdeployed_site), streamsidemax = max(daysdeployed_site), streamsidemean = mean(daysdeployed_site), streamsidesd = sd(daysdeployed_site)) %>%
  data.frame()

#Image Data Frame Test Code ----

setwd("~/Dropbox/CISP")

##Checking for hidden files
###Build df with hidden and without
baseimagedftest <- as.data.frame((list.files("imagesin", full.names = T, recursive = T)))
names(baseimagedftest) <- "x"
baseimagedftest$x <- as.character(baseimagedftest$x)

baseimagedftesthidden <- as.data.frame(list.files("imagesin", all.files = T, full.names = T, recursive = T))
names(baseimagedftesthidden) <- "x"
baseimagedftesthidden$x <- as.character(baseimagedftesthidden$x)
baseimagedftesthidden <- as.data.frame(as.character(baseimagedftesthidden$x))

###Compare hidden vs no hidden to see if only difference
hiddentest <- anti_join(baseimagedftesthidden, baseimagedftest) ####Only difference is .test.txt, confirm produced image data frame has no hidden files with below line

hiddentest <- filter(baseimagedftesthidden, grepl("/.", x, fixed=TRUE)) #USE TO CHECK MAIN IMAGE DATA FRAME

###Test for blank folders
####Build data frame showing image list with directory rows, split camera date out of it
baseimagedftestblank <- as.data.frame(list.files("imagesin", full.names = T, recursive = T, include.dirs = T))
names(baseimagedftestblank) <- "directory"

testblanksplit <- as.data.frame(str_split_fixed(baseimagedftestblank$directory, "/", 4))
names(testblanksplit) <- c("containing", "camera", "cameradate", "image")
testblankdate <- as.data.frame(str_split_fixed(testblanksplit$cameradate, "_", 2))
testblankdate$V1 <- NULL

testblank <- cbind(testblanksplit, testblankdate)
names(testblank) <- c("containing", "camera", "cameradate", "image", "date")

testblank$camera <- as.character(testblank$camera)
testblank$cameradate <- as.character(testblank$cameradate)
testblank$image <- as.character(testblank$image)
testblank$date <- as.character(testblank$date)

####Attempts to check for blank folders
#####Head row of each folder
folderheads <- testblank %>%
  filter(date != "", image == "") %>%
  data.frame()

#####Blank folder check
folderrowssum <- testblank %>%
  group_by(camera,cameradate) %>%
  summarise(nimage=n()) %>%
  data.frame()

blankfolders <- folderrowssum %>%
  filter(cameradate != "", nimage == 1)

###Build exif data using image list
baseimagedftestdir <- (list.files("imagesin", full.names = T,recursive = T, include.dirs = T))

dfout <- NULL

starti <- 1
chunksize <- 200

while(starti <= length(baseimagedftestdir)) {
  chunkfiles <- baseimagedftestdir[starti:min(starti+chunksize, length(baseimagedftestdir))]
  message("Processing chunk ", starti, ":", min(starti+chunksize, length(baseimagedftestdir)))
  starti <- starti + chunksize + 1
  tryCatch({
    if(is.null(dfout)) {
      dfout <- exifr(chunkfiles, quiet=F)
    } else {
      chunkfiles <- chunkfiles[!(chunkfiles %in% dfout$FileName)]
      dfout <<- plyr::rbind.fill(dfout, exifr(chunkfiles, quiet=F))
    }
  }, error=function(err) {message("Error processing chunk prior to index ", starti)})
}

imageexifdftestdir <- dfout ####Note errors when running dfout

str(imageexifdftestdir)

###Tests for 0kb files, .txt files, other file types
table(imageexifdftestdir$FileSize)
test <- filter(imageexifdftestdir, FileSize <= 41)
table(imageexifdftestdir$Error)
test <- filter(imageexifdftestdir, Error != "")
table(imageexifdftestdir$FileType)
test <- filter(imageexifdftestdir, FileType == "") ####Few different strategies of trying to detect other issues

###Tests for rows with no collection date
table(imageexifdftestdir$DateTimeOriginal)
test <- filter(imageexifdftestdir, DateTimeOriginal == "")

###Investigating exifr errors
####Only errors if non-images ran through
####Only chunks with non-images showed warning messages
####Date extraction from images post non-image lines are fine (sometiems off by 1 second), assume no added issues having non .jpg rows

#Create image data frame and attempt to join with site data frame
##Build image data frame again
#####setwd("~/Dropbox/CISP")
rm(list = ls())

imagedftest <- (list.files("imagesin", full.names = T,recursive = T))

dfout <- NULL

starti <- 1
chunksize <- 200

while(starti <= length(imagedftest)) {
  chunkfiles <- imagedftest[starti:min(starti+chunksize, length(imagedftest))]
  message("Processing chunk ", starti, ":", min(starti+chunksize, length(imagedftest)))
  starti <- starti + chunksize + 1
  tryCatch({
    if(is.null(dfout)) {
      dfout <- exifr(chunkfiles, quiet=F)
    } else {
      chunkfiles <- chunkfiles[!(chunkfiles %in% dfout$FileName)]
      dfout <<- plyr::rbind.fill(dfout, exifr(chunkfiles, quiet=F))
    }
  }, error=function(err) {message("Error processing chunk prior to index ", starti)})
}

imagedftest <- dfout
str(imagedftest)

imagedftest <- imagedftest[, c("Directory", "FileName", "DateTimeOriginal")] ####Removing some unneccessary columns during initial tests

###Modify datetimeorignal to be posixct
imagedftest$DateTimeOriginal<-ymd_hms(imagedftest$DateTimeOriginal, tz="America/Halifax")

####Split directory into camera and folder date and add back in
imagedftestfolder <- as.data.frame(str_split_fixed(imagedftest$Directory, "/", 3))
table(imagedftest$Directory)
table(imagedftestfolder$V3) #####Check that the two columns match
imagedftestfolderdate <- as.data.frame(str_split_fixed(imagedftestfolder$V3, "_", 2))
table(imagedftestfolder$V3)
table(imagedftestfolderdate$V2)
names(imagedftestfolderdate) <- c("cam", "folderdate")
imagedftest <- cbind(imagedftest, imagedftestfolderdate)
imagedftest$Directory <- NULL


####Fix cam column
camcolfix <- as.data.frame(str_split_fixed(imagedftest$cam, "Camera", 2))
camcolfix$V1 <- NULL
names(camcolfix) <- "camtest"
imagedftest<- cbind(imagedftest, camcolfix)
imagedftest$cam <- NULL
names(imagedftest) <- c("FileName", "DateTimeOriginal","folderdate", "cam")

####Convert folderdate to date format
imagedftest$folderdate <- dmy(imagedftest$folderdate)
str(imagedftest)

#####Rearrange data frame
imagedftest <- imagedftest[, c("cam", "folderdate", "FileName", "DateTimeOriginal")]
imagedftest <- arrange(imagedftest, cam, folderdate, FileName)

##Build site data frame and prepare it
sites <- read.csv("~/CISP/CamSites.csv", stringsAsFactors = F)
sites$cam<-as.factor(sites$cam)
str(sites)

###Merging install and removal dates and converting to date format
sites$instdate <- as.Date(with(sites, paste(inst_y, inst_m, inst_d,sep="-")), "%Y-%m-%d")
str(sites$instdate)

sites$remdate <- as.Date(with(sites, paste(camrem_y, camrem_m, camrem_d,sep="-")), "%Y-%m-%d")
str(sites$remdate)

###Duplicating removal date column and filling NAs with cut off date
sites$remdatefill <- sites$remdate

sites$remdatefill[is.na(sites$remdatefill)] <- "2015-05-07"

###Simplifications to sites data frame for testing
sites2 <- arrange(sites, site, cam, instdate) %>%
  data.frame()
sites2 <- sites2 [, c("site", "cam", "location", "purpose", "streamside", "instdate", "remdatefill")]

###Mirror date range columns from sites data frame for image data frame
imagedftest$instdate <- imagedftest$folderdate
imagedftest$remdatefill <- imagedftest$instdate
str(imagedftest)

##Attempting to set up a rolling join (Ensure camera columns are factors!)
setDT(imagedftest) #set data.table x
setDT(sites2) #set data.table y
setkey(sites2, cam, instdate, remdatefill) #set a key with trap_id for match, and date range as last two variables
test<-foverlaps(imagedftest, sites2, type = "within")
test<-as.data.frame(test)
str(test) #Check structure, note i.variables

test$i.instdate <- NULL
test$i.remdatefill <- NULL

write.csv(test, "test")

testl <- test %>%
  group_by(site, folderdate) %>%
  summarise(NROW(site)) %>%
  data.frame()

test2 <- left_join(test, testl)

test3 <- test2 %>% 
  group_by(site, folderdate) %>% 
  mutate(testdate = lag(folderdate, n = test3$NROW.site.)) %>% 
  data.frame()

#
test2<-test[-(1:335),]
ttt<-test2%>%group_by(site)%>%mutate(startpdate = lag(i.remdatefill, 1))%>%
  group_by(cam)%>%mutate(startpdate = ifelse(is.na(startpdate),instdate,startpdate))%>%
  mutate(startpdate = as.Date(startpdate, origin = "1970-01-01")) %>% 
  data.frame()
str(ttt)
  
#top_n
#n()
#lead()
#lag()


#Image Data Frame building code ----
##Initial image data frame list

##Attaching exif data to images
#Note: See "brook_thesiscodeall_6may2016.Rmd" for Ryan's code for this stuff, in case we decide we wish to utilise that.

setwd("Z:/Animal Passage Files")
setwd("/media/Brook/Animal Passage Files/")

camerafilesraw <- list.files("/media/Brook/Animal Passage Files/FieldCameras", full.names=T, recursive = T)

write.table(camerafilesraw, "camerafilesraw")

dfout <- NULL

starti <- 1
chunksize <- 200

while(starti <= length(camerafilesraw)) {
  chunkfiles <- camerafilesraw[starti:min(starti+chunksize, length(camerafilesraw))]
  message("Processing chunk ", starti, ":", min(starti+chunksize, length(camerafilesraw)))
  starti <- starti + chunksize + 1
  tryCatch({
    if(is.null(dfout)) {
      dfout <- exifr(chunkfiles, quiet=F)
    } else {
      chunkfiles <- chunkfiles[!(chunkfiles %in% dfout$FileName)]
      dfout <<- plyr::rbind.fill(dfout, exifr(chunkfiles, quiet=F))
    }
  }, error=function(err) {message("Error processing chunk prior to index ", starti)})
}

##Expanding dates, cleaning image data frame
###Checking output data frame for possible errors
#Possible code for checking for errors in image data frame ----
#Is cameradate blank?
blanks <- images[images$cameradate == "", ]

#Querying for instances of folders with no images
empty <- rownames(table(images$cameradate)[which(table(images$cameradate) == 1)])

#Non .jpg checks
wtf<- filter(images, !grepl(".JPG", image))

#Ryan code for detecting blank entries
####Ryan code for detecting instances of blank entries (folders with no data). Not all of this is needed, but lots of it would be for retesting. Included all here till can parse it out.
#Ryan's Code#
blankscheck <- images[!duplicated(images$cameradate), ] #Just a quick check that the first line for each entry in cameradate is indeed blank in the image column. Comparing the the images data frame, the few rows checked from the blanscheck data frame were indeed blank in the rows from the images data frame. Should have worked.#

subimages <- images[duplicated(images$cameradate),"cameradate"] #Returns a string of repeated cameradate entries (exluding ones not duplicated). This effectively removes all non duplicated cameradate entries.#

subimages <- subset(images,cameradate %in% subimages) #Subset the dataframe to just those entries contained in the character string, which will exclude all rows that only exist once. appears larger that above line because it is including the first row for each cameradate entry where image is blank#

trueempty <- subset(images, !(cameradate %in% subimages)) #running a reverse of the above line to see all folders that are empty# Manually checked a few of the indicated folders, were empty. Suggests working as intended.

subimages <- subimages[!duplicated(subimages$cameradate),] #Pullout first entry of each duplicated cameradate, excluding single entries due to the above lines.#

Now that we have a data frame of the rows that are not desired in the images data frame. We need to remove them.
```{r, eval= FALSE}
#DO NOT RUN NORMALLY# images$index=row.names(images) # this will create a vector of row.names which corresponds to the subimages

row.names(images)=NULL # get rid of the confusing row names which are a vestiage of the old dataframe
images <- images[which(!images$index %in% as.numeric(row.names(subimages))),] #Saying return all rows where images$index does not line up with row.names of subimages (essentially)#

1020 rows were removed, which is the amount in subimages. Also, the first several rows that were supposed to be removed are indeed removed, suggesting this worked correctly.

Now to create some data frames where .JPG and "" are removed.

noimages <- filter(images, !grepl(".JPG", image))

noblanks <- filter(noimages, !grepl("", image)) #Not working#

Noticed that there were files that required removal (the older .DS_Store and other hidden files). These will need to be removed and the data frame rebuilt.

Exporting the noblanks data frame to use as a basis for investigating texts and blank folders.

write.csv(noimages, "blankfolders.csv")

Resolving the differences between FieldCamera folders in server and backup

cameradirserv <- "Z:/Animal Passage Files/FieldCameras"
cameradirbackup <- "W:/BBBackup/FieldCameras"

imagesserv <- as.data.frame(list.files(cameradirserv, all.files = TRUE, full.names = FALSE, recursive = TRUE, include.dirs = TRUE))

write.csv(imagesserv, file = "servercameraimages")

imagesbackup <- as.data.frame(list.files(cameradirbackup, all.files = TRUE, full.names = FALSE, recursive = TRUE, include.dirs = TRUE))

write.csv(imagesbackup, file = "backupcameraimages")

###Splitting data frame directory levels
names(images) <- "directory"
imagessplit<-as.data.frame(str_split_fixed(images$directory, "/", 3))
names(imagessplit) <- c("camera", "cameradate", "image")
head(imagessplit)

###Isolating date from camera date
datesplit <- as.data.frame(str_split_fixed(imagessplit$cameradate, "_", 2))
head(datesplit)

###Dropping the Camera column#
datesplit$V1 <- NULL

###Binding 'datesplit' to 'imagessplit'
datebind <- cbind(imagessplit, datesplit)
names(datebind) <- c("camera", "cameradate", "image", "date")
datebind$camera <- as.character(datebind$camera)
datebind$cameradate <- as.character(datebind$cameradate)
datebind$image <- as.character(datebind$image)
datebind$date <- as.character(datebind$date)
parseddate <- as.data.frame(dmy(datebind$date))
names(parseddate) <- ("date")
datesplit <- as.data.frame(str_split_fixed(parseddate$date, "-", 3))
names(datesplit) <- c("year", "month", "day")
mount -t cifs -o username=bbeauliua password=8T3^gam8ty //kraken.acadiau.ca/bbeauliua /media/Brook
imagessplit <- cbind(imagessplit, datesplit)

#Random lines of code ----
ggsave(paste(imagedir,"imagename.png",""), ggplotimage)
#Placeholder for Trevor's demonstration line, delete or modify later

#Potentially useful things in "brook_thesiscodeall_6may2016.Rmd"
-code to check differences between the image server and a backup file (not sure if completed)

-Test code for examinining images pre/post a certain date.

Side note: To check file types, would it be logical to do a str_split based on "." to create a column of file types? Should we be creating that column anyways?
