## ################################# ##
## EXPLORE PATTERN IN UDIFF_DF       ##
##                                   ##
## Author: Yi Zhang                  ##
## Date: March/15/2015               ##
## ################################# ##
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggvis)

## ################################## ##
## ENVIRONMENT SETUP                  ##
## ################################## ##
dir        <- list()
dir$root   <- "/Users/beingzy/Documents/Projects/phd/"
dir$data   <- paste(dir$root, "data/",   sep="")
dir$images <- paste(dir$root, "images/", sep = "")

## ################################## ##
## CUSTOM FUNCTIONS                   ##
## ################################## ##
get_file_path <- function(filename, dir_path){
  ## ****************************** ##
  ## Return the full path pointing  ##
  ## to the file                    ##
  ## ****************************** ##
  res <- paste(dir_path, filename, sep="")
  return(res)
}

## ################################# ##
## LOAD DATA                         ##
## ################################# ##
df <- read.csv(file=get_file_path("udiff_selective.csv", dir_path=dir$data), header=TRUE, stringsAsFactors = FALSE)


## #################### ##
## DATA TRANSFORMATION  ##
## #################### ##
set.seed(1234)
# process data for visualization
melt_df        <- melt(data=df, id=c("ua_id", "ub_id", "class"))
melt_df        <- filter(melt_df, !is.na(value))
melt_df$class  <- as.factor(melt_df$class)
# generate sample data
sample_idx     <- sample(x=1:nrow(melt_df), size=10000)
melt_df_sample <- melt_df[sample_idx, ]

## #################### ##
## DATA VISUALIZATION   ##
## #################### ##
images <- list()

images$density <- ggplot(data=melt_df, aes(x=value, colour=class, fill=class)) + 
                  geom_density(alpha=.5) + 
                  facet_wrap(~variable)

images$boxplot <- ggplot(data=melt_df, aes(x=class, y=value, fill=class)) + 
                  geom_boxplot() + 
                  facet_wrap(~variable)

## #################### ##
## EXPORT IMAGES
ggsave(filename=get_file_path("density_variable.png", dir_path=dir$images), plot=images$density)
ggsave(filename=get_file_path("boxplot_variable.png", dir_path=dir$images), plot=images$boxplot)

## ####################### ##
## CLUSTERING ANALYSIS     ##
## ####################### ##
df4vis <- filter(melt_df, variable == "birthday")
ggplot(data=df4vis, aes(value=value, fill=class)) + geom_histogram()
