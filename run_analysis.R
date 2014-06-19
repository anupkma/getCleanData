#Assumption is that file was downloaded from internet and available in the working directory
#with name original-dataset.zip

local.data.file <- './original-dataset.zip'
local.data.dir <- './UCI HAR Dataset'
tidy.avgs.data.file <- './tidy-UCI-HAR-avgs-dataset.txt'


## Utility Function to get the file Path
getFilePath <- function(...){ paste(...,sep='/')}

if (! file.exists(local.data.file)) {
        stop(paste(local.data.file, 'must be present in working directory.'))
}

##Unzip the file

if (! file.exists(local.data.dir)) {
        unzip(local.data.file)
}

# Read activity labels

acts <- read.table(getFilePath(local.data.dir, 'activity_labels.txt'),
                   header = FALSE, col.names=c('id', 'name'))



# Read feature labels
feature <- read.table(getFilePath(local.data.dir,'features.txt'),
                    header = FALSE,col.names=c('id', 'name'))




# Read the data files and assign column names
train.X <- read.table(getFilePath(local.data.dir,'train', 'X_train.txt'),
                      header = FALSE,col.names=feature$name)

train.y <- read.table(getFilePath(local.data.dir,'train', 'y_train.txt'),
                       header = FALSE,col.names=c('activity'))

train.subject <- read.table(getFilePath(local.data.dir,'train','subject_train.txt'),
                            header = FALSE,col.names=c('subject'))


test.X <- read.table(getFilePath(local.data.dir, 'test', 'X_test.txt'),
                     header = FALSE,col.names=feature$name)

test.y <- read.table(getFilePath(local.data.dir, 'test', 'y_test.txt'),
                      header = FALSE,col.names=c('activity'))

test.subject <- read.table(getFilePath(local.data.dir, 'test', 'subject_test.txt'),
                           header = FALSE,col.names=c('subject'))

# First Major Step. Merge Training and Test data sets

X <- rbind(train.X, test.X)
y <- rbind(train.y, test.y)
subject <- rbind(train.subject, test.subject)


# Discard everything else except mean and sd
X <- X[, grep('mean|std', feature$name)]

#Convert activity digits to activity names
y$activity <- acts[y$activity,]$name


#Get everything together
tidy.data.set <- cbind(subject, y, X)


#Use Aggregate function to subset, group and calculate mean
# subset-data set- 3rd column till end of line
# by- subject and activity
tidy.avgs.data.set <- aggregate(tidy.data.set[, 3:length(tidy.data.set)],
                                list(subject=tidy.data.set$subject,
                                     activity=tidy.data.set$activity),
                                mean)
#Write back the cleaned file
write.csv(tidy.avgs.data.set, tidy.avgs.data.file)