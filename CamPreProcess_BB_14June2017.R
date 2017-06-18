#Data frame preparation code for Brook's camera/plot work

# Packages (Install) ----
install.packages("magrittr")
install.packages("digest")
install.packages("lubridate")
install.packages("splitstackshape")
install.packages("devtools") #Requires strawberry perl to be installed (mac and linux users already have this)

# Packages (Library) ----
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

#Important Tutorials
####http://genomicsclass.github.io/book/pages/dplyr_tutorial.html

#Site Data Frame Upload and Summary Statistics
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
table(sites$streamside)

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
sites$datediff <- as.integer(difftime(sites$remdatefill, sites$instdate, units = "days"))

###Calculating total days cameras were deployed (across all sites)
sites <- sites %>%
  mutate(daysdeployed_total = sum(datediff))

##Deployment times, means, stdev, range by camera, site, total
###Deployment periods per site and per site per camera
groupedsites <- sites %>%
  group_by(site, cam) %>%
  summarise(daysdeployed_cam = sum(datediff)) %>%
  mutate(daysdeployed_site = sum(daysdeployed_cam)) %>%
  mutate(daysdeployed_total = sum(sites$datediff)) %>%
  mutate(daysdeployed_camsite_percent = daysdeployed_cam/daysdeployed_site*100) %>%
  mutate(daysdeployed_camtotal_percent = daysdeployed_cam/daysdeployed_total*100) %>%
  mutate(daysdeployed_sitetotal = daysdeployed_site/daysdeployed_total*100)%>%
  data.frame() #per camera and per site proportions of deployment total

###Deployment ranges, mean, stdev
statswithinsites <- groupedsites %>%
  group_by(site) %>%
  summarise(cammin = min(daysdeployed_cam), cammax = max(daysdeployed_cam), cammean = mean(daysdeployed_cam), camsd = sd(daysdeployed_cam)) %>%
  data.frame()

totaltable <- groupedsites %>%
  select(site, daysdeployed_site) %>% 
  data.frame()

totaltable <- totaltable[!duplicated(totaltable), ]

statsbetweensites <- totaltable %>%
  summarise(sitemin = min(daysdeployed_site), sitemax = max(daysdeployed_site), sitemean = mean(daysdeployed_site), sitesd = sd(daysdeployed_site)) %>%
  data.frame() #desire range() instead?

##Deployment times, means, stdev, range by location, purpose, streamside
groupedsitestest <- sites %>%
  select(site, cam, location, purpose, streamside, datediff, daysdeployed_total)

groupedsitestest <- groupedsitestest %>%
  group_by(site, cam) %>%
  select(site, cam, location, purpose, streamside, datediff, daysdeployed_total)
  summarise(daysdeployed_cam = sum(datediff)) %>%
  mutate(daysdeployed_site = sum(daysdeployed_cam))
  #######Trying to sum dateddiff by site, so that I can then group_by later for summary statistics by location, purpose, streamside, how do I preserve those columns?

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

##Code for determining camera survey time
site$startdate <- as.Date(with(site, paste(inst_y, inst_m, inst_d,sep="-")), "%Y-%m-%d")
Error in with(site, paste(inst_y, inst_m, inst_d, sep = "-")) : 
  object 'site' not found
sites$startdate <- as.Date(with(sites, paste(inst_y, inst_m, inst_d,sep="-")), "%Y-%m-%d")
View(sites)
str(sites$startdate)
Date[1:85], format: "2013-06-06" "2013-06-06" "2013-06-19" "2013-06-19" "2013-06-19" "2013-06-19" "2013-07-12" "2013-07-17"
sites$enddate <- as.Date(with(sites, paste(camrem_y, camrem_m, camrem_d,sep="-")), "%Y-%m-%d")
View(sites)
sites$date_diff <- as.Date(as.character(sites$enddate), format="%Y/%m/%d")-
  +     as.Date(as.character(sites$startdate), format="%Y/%m/%d")
View(sites)
sites$date_diff <- as.Date(sites$enddate, format="%Y/%m/%d")-
  +     as.Date(sites$startdate, format="%Y/%m/%d")
View(sites)
sites$date_diff <- sites$date_diff + 1
View(sites)

#Potentially useful things in "brook_thesiscodeall_6may2016.Rmd"
-code to check differences between the image server and a backup file (not sure if completed)

-Test code for examinining images pre/post a certain date.

Side note: To check file types, would it be logical to do a str_split based on "." to create a column of file types? Should we be creating that column anyways?


#Test Data Area
####Area for temporarily testing code, before correct portions are included above.
##Building test image list
testimagelist<- list.files("imagesin", all.files=F, full.names=T, recursive = T, include.dirs=T)
testimagelisthidden<- list.files("imagesin", all.files = TRUE, full.names=T, recursive = T, include.dirs=T) #includes hidden files

testimagelistdf <- as.data.frame(testimagelist)
testimagelisthiddendf <- as.data.frame(testimagelisthidden)

names(testimagelistdf) <- 'x'
names(testimagelisthidden) <- 'x'

hiddenfiles <- anti_join(testimagelisthiddendf$x, testimagelistdf$x) #comparing data frames to check for hidden row

dfouttest <- NULL

starti <- 1
chunksize <- 200

while(starti <= length(testimagelist)) {
  chunkfiles <- testimagelist[starti:min(starti+chunksize, length(testimagelist))]
  message("Processing chunk ", starti, ":", min(starti+chunksize, length(testimagelist)))
  starti <- starti + chunksize + 1
  tryCatch({
    if(is.null(dfouttest)) {
      dfouttest <- exifr(chunkfiles, quiet=F)
    } else {
      chunkfiles <- chunkfiles[!(chunkfiles %in% dfouttest$FileName)]
      dfouttest <<- plyr::rbind.fill(dfouttest, exifr(chunkfiles, quiet=F))
    }
  }, error=function(err) {message("Error processing chunk prior to index ", starti)})
}

