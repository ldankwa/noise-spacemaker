#######################


# Spacemaker Noise Surrogate Model


#####################
# Packages & Libraries
###################

# Installing and reading packages

# install.packages("jsonlite")      ## Reading data
# install.packages("stringr")       ## Data wrangling
# install.packages("tidyr")         ## data clean up

# install.packages("ggplot2")       ## ggplot
# install.packages("RcppCNPy")      ## for npyload
# install.packages("Matrix")        ## for nnzero
# install.packages("dplyr")         ## to use %>% 
# install.packages("caret")         ## to partition datasets and train function
# install.packages("kernlab")
# install.packages("tree")
# install.packages("gam")

library(ggplot2)      
library(RcppCNPy)      
library(Matrix)        
library(dplyr)         
library(jsonlite)       ## for json read
library(stringr)        ## for str split
library(tidyr)          ## for spread function

library(caret)
library(kernlab)
library(randomForest)
library(tree)
library(gam)

# Projects consists of 7 parts: 
# PART A: Read Data - 3 Sections
# PART B: Data Exploration
# PART C: Create Features
# PART D: Feature Exploration - 3 Sections
# PART E: Create Train/Test Sets
# PART F: Model Implementation - 8 Models
# PART G: Apply Best Model on Test Site data


######################################
#  PART A: Read Data
#####################################

# Unzip the data file. Code assumes zip file in located in the working directory.
unzip("data.zip")

# Create empty directory to save all plots
dir.create("graphs")



# Section 1 of 3: Reading the non-specific training data


json1<-fromJSON(txt="data/non_specific_training_data.json")


# Using the unlist function to change the list format and to add the data as rows
json1<-data.frame(number = unlist(json1))



# Also gives rownames that have all the strings attached by "."
# Using strsplit by . and transform into columns

json1$scenario<-lapply(strsplit(as.character(rownames(json1)),"\\."), "[", 1)
json1$id<-lapply(strsplit(as.character(rownames(json1)), "\\."), "[", 2)
json1$path<-lapply(strsplit(as.character(rownames(json1)), "\\."), "[", 3)
rownames(json1)<-NULL


# Use spread function from tidyr to change rows into columns by specific to each entry

json_non_specific<-spread(json1, path, number)
json_non_specific$fraction_yellow_zone <- as.numeric(as.character(json_non_specific$fraction_yellow_zone))



# Section 2 of 3:  Reading the specific training data


json2<-fromJSON(txt="data/specific_training_data.json")
json2<-data.frame(number = unlist(json2))
json2$id<-lapply(strsplit(as.character(rownames(json2)),"\\."), "[", 1)
json2$path<-lapply(strsplit(as.character(rownames(json2)), "\\."), "[", 2)
rownames(json2)<-NULL
json_specific<-spread(json2, path, number)
json_specific$fraction_yellow_zone <- as.numeric(as.character(json_specific$fraction_yellow_zone))




# Section 3 of 3:  Reading the test data


json3<-fromJSON(txt="data/test_data.json")
json3<-data.frame(number = unlist(json3))
json3$id<-lapply(strsplit(as.character(rownames(json3)),"\\."), "[", 1)
json3$path<-lapply(strsplit(as.character(rownames(json3)), "\\."), "[", 2)
rownames(json3)<-NULL
json_test<-spread(json3, path, number)
json_test$fraction_yellow_zone <- as.numeric(as.character(json_test$fraction_yellow_zone))


rm(json1, json2, json3)




#####################################
#  PART B: Data Exploration
#####################################


# Using the non-specific data to make plots of some of the 9 scenarios


json_non_specific$scenario<-as.character(json_non_specific$scenario)

p <- json_non_specific %>%
  group_by(scenario) %>%
  summarize(scenario=first(scenario),avg_frac = mean(fraction_yellow_zone), .groups='drop') %>%
  arrange(scenario)



# Plotting fraction yellow zone across different scenarios and saving the plots into a new directory 
# called graphs


p<-json_non_specific%>%
  group_by(scenario)%>%
  summarize(avg_frac=mean(fraction_yellow_zone), .groups='drop')%>%arrange(scenario)


jpeg('graphs/frac_scenarios.jpg')
ggplot(p,aes(scenario,avg_frac, fill = scenario))+
  geom_col()+
  xlab("")+
  ylab("Average Fraction Yellow Zone")+
  ggtitle("Fraction Yellow Zone Across Different Scenarios")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))
dev.off()




# Plotting buildings against fraction yellow zone across scenarios 6 and 9 


p1<-json_non_specific%>%
  filter(scenario %in% c("scenario_6", "scenario_9"))%>%
  select(scenario, fraction_yellow_zone)
p1$n<-c(1:500, 1:500)



jpeg('graphs/frac_bldgs.jpg')

p1 %>%
  ggplot(aes(n, fraction)) +
  geom_point(aes(col=scenario)) +
  xlab("Different Buildings") +
  ylab("Fraction Yellow Zone") +
  ggtitle("Fraction Yellow Zone for Scenarios 6 & 9") +
  theme(legend.position="bottom")
dev.off()



#   Site configurations using the non-specific dataset

fraction_summary<- json_non_specific %>%
  group_by(scenario) %>%
  summarise(max_frac=max(fraction_yellow_zone), min_frac=min(fraction_yellow_zone), 
            src=source_grid_path[fraction_yellow_zone==max_frac], 
            bldg_max_npy=building_grid_path[fraction_yellow_zone==max_frac], 
            bldg_min_npy=building_grid_path[fraction_yellow_zone==min_frac],
            .groups='drop')

#   Site configurations using the specific dataset

fraction_summary_spec<- json_specific%>%
  summarise(max_frac=max(fraction_yellow_zone), min_frac=min(fraction_yellow_zone), 
            src=source_grid_path[fraction_yellow_zone==max_frac], 
            bldg_max_npy=building_grid_path[fraction_yellow_zone==max_frac], 
            bldg_min_npy=building_grid_path[fraction_yellow_zone==min_frac], 
            .groups='drop')

#   Site configurations using the test dataset
fraction_summary_test<- json_test%>%
  summarise(max_frac=max(fraction_yellow_zone), min_frac=min(fraction_yellow_zone), 
            src=source_grid_path[fraction_yellow_zone==max_frac], 
            bldg_max_npy=building_grid_path[fraction_yellow_zone==max_frac], 
            bldg_min_npy=building_grid_path[fraction_yellow_zone==min_frac], 
            .groups='drop')




# Creating a function to create grid areas of minimum and maximum fraction yellow zones


create_maps <- function(x, y, z, i) {
  input_path1 <- paste("data/", x, sep="")         #create the path
  print(input_path1)
  srcdata <- npyLoad(input_path1, "integer")       # open the file
  src_data<-ifelse(srcdata==85, 25, 0)
  
  input_path2 <- paste("data/", y, sep="")                       
  print(input_path2)
  max_bldg_data <- npyLoad(input_path2, "integer") 
  
  input_path3 <- paste("data/", z, sep="")                       
  print(input_path3)
  min_bldg_data <- npyLoad(input_path3, "integer") 
  
  max_file_name<-paste("graphs/maxplot_", i, ".jpeg", sep="")
  jpeg(max_file_name)
  image(src_data+max_bldg_data, main="Grid area for max fraction yellow zone")
  dev.off()
  
  min_file_name<-paste("graphs/minplot_", i, ".jpeg", sep="")
  jpeg(min_file_name)
  image(src_data+min_bldg_data, main="Grid area for min fraction yellow zone")
  dev.off()
  
  return("Done")
}


# i is used to create the names of the images saved

mapply(create_maps, fraction_summary$src, fraction_summary$bldg_max_npy, fraction_summary$bldg_min_npy, fraction_summary$scenario)
i<-10
mapply(create_maps, fraction_summary_spec$src, fraction_summary_spec$bldg_max_npy, fraction_summary_spec$bldg_min_npy, i)
i<-11
mapply(create_maps, fraction_summary_test$src, fraction_summary_test$bldg_max_npy, fraction_summary_test$bldg_min_npy, i)




#####################################
#  PART C: Create  Features
#####################################


# Function to create distance features

# arguments get both *npy files src (source), bldg(building)

# for the purposes of the distance features, each building has been treated as a collection of 
# buildings at adjacent locations

create_features <- function(x,y) {
  src_path <- paste("data/", x, sep="")
  print(src_path)
  src_file <- npyLoad(src_path, "integer")
  bldg_path <- paste("data/", y, sep="")
  print(bldg_path)
  bldg_file <- npyLoad(bldg_path, "integer")
  
  cov_rd <- nnzero(src_file)/length(src_file)          #calculate the coverage
  cov_bldg <- nnzero(bldg_file)/length(bldg_file)      #calculate the coverage
  cov_ratio<-as.numeric(cov_bldg)/as.numeric(cov_rd)   #calculate ratio of covereages
  
  bldg_avg<-mean(bldg_file[bldg_file>0])               # calculate average building height(excludes the zeros)
  bldg_median<-median(bldg_file[bldg_file>0])          # calculate median building height(excludes the zeros)
  bldg_max<-max(bldg_file)                             # calculate max building height(excludes the zeros)
  bldg_min<-min(bldg_file[bldg_file>0])                # calculate min building height(excludes the zeros)
  bldg_diff<-bldg_max-bldg_min                         # calculate difference of max and min building height
  
  
  #initialize arr (array) to save a list of locations that have noise source- to null
  arr<-NULL
  for (i in 1:51){
    for (j in 1:51) {
      if (src_file[i,j] > 0) {
        arr<-rbind(arr, c(i,j))
      }   
    }
  }
  
  
  # define distance as the distance array that has distances between one location of building to all noise locations
  
  distance <- NULL
  D <- matrix(0, ncol=ncol(bldg_file), nrow=nrow(bldg_file)) 
  
  # D is the matrix which has each building's minimum distance to road
  for (i in 1:51){
    for (j in 1:51) {
      if (bldg_file[i,j] > 0) {
        for (k in 1:nrow(arr)) {
          distance[k]<-sqrt((arr[k,1]-i)^2+(arr[k,2]-j)^2)    #calculate distance to the array of noise locations
        }
        D[i,j]<-min(distance)            #find minimum distance and save in D
      }
    }
  }
  
  
  avg_dist_rd<-mean(D[D>0])                       # average of all minimum distance gives average distance from building to road
  min_dist_rd<-min(D[D>0])                        # minimum distance from building to road
  ht_min_dist_rd<-mean(bldg_file[D==min_dist_rd]) # height of closest building to road
  
  
  # get building coverage for 4 buffer zones to portray density from road
  
  bldg_cov_rd_12<-length(D[D>0 & D<=12])/length(bldg_file)      # Zone 1 for distance less than 12 units
  bldg_cov_rd_24<-length(D[D>12 & D<=24])/length(bldg_file)     # Zone 2 for distance between 12 and 24 units
  bldg_cov_rd_36<-length(D[D>24 & D<=36])/length(bldg_file)     # Zone 3 for distance between 24 and 36 units
  bldg_cov_rd_51<-length(D[D>36])/length(bldg_file)             # Zone 4 for distance greater than 36 units
  
  
  
  # get average building heights within the for 4 different buffer zones 
  
  ht_avg_dist_rd_12<-ifelse(bldg_cov_rd_12!=0, mean(bldg_file[D>0 & D<=12]), 0)
  ht_avg_dist_rd_24<-ifelse(bldg_cov_rd_24!=0, mean(bldg_file[D>12 & D<=24]), 0)
  ht_avg_dist_rd_36<-ifelse(bldg_cov_rd_36!=0, mean(bldg_file[D>24 & D<=36]), 0)
  ht_avg_dist_rd_51<-ifelse(bldg_cov_rd_51!=0, mean(bldg_file[D>36]), 0)
  
  
  # get maximum building heights within the 4 different buffer zones 
  
  ht_max_dist_rd_12<-ifelse(bldg_cov_rd_12!=0, max(bldg_file[D>0 & D<=12]), 0)
  ht_max_dist_rd_24<-ifelse(bldg_cov_rd_24!=0, max(bldg_file[D>12 & D<=24]), 0)
  ht_max_dist_rd_36<-ifelse(bldg_cov_rd_36!=0, max(bldg_file[D>24 & D<=36]), 0)
  ht_max_dist_rd_51<-ifelse(bldg_cov_rd_51!=0, max(bldg_file[D>36]), 0)
  
  
  
  return(c(cov_rd=cov_rd, cov_bldg=cov_bldg, cov_ratio=cov_ratio, 
           bldg_avg=bldg_avg, bldg_median=bldg_median,           
           bldg_max=bldg_max, bldg_min=bldg_min, bldg_diff=bldg_diff, 
           avg_dist_rd=avg_dist_rd, min_dist_rd=min_dist_rd, ht_min_dist_rd=ht_min_dist_rd, 
           cov_buffer1=bldg_cov_rd_12,     cov_buffer2=bldg_cov_rd_24,        #cov_buffer* measures the building covereage in the different zones
           cov_buffer3=bldg_cov_rd_36,     cov_buffer4=bldg_cov_rd_51,
           ht_buffer1=ht_avg_dist_rd_12,   ht_buffer2=ht_avg_dist_rd_24,       #ht_buffer* measures the average height of the building in different zones
           ht_buffer3=ht_avg_dist_rd_36,   ht_buffer4=ht_avg_dist_rd_51,
           htmax_buffer1=ht_max_dist_rd_12,   htmax_buffer2=ht_max_dist_rd_24, #htmax_buffer* measures the maximum height of the building in different zones
           htmax_buffer3=ht_max_dist_rd_36,   htmax_buffer4=ht_max_dist_rd_51)
  )
}

# Creating the distance features for non-specific, specific and test data

non_spec_dist_stats<-mapply(create_features, json_non_specific$source_grid_path, json_non_specific$building_grid_path)
spec_dist_stats<-mapply(create_features, json_specific$source_grid_path, json_specific$building_grid_path)
test_dist_stats<-mapply(create_features, json_test$source_grid_path, json_test$building_grid_path)


# Switching the columns and the rows

non_spec_dist_stats_t<-t(non_spec_dist_stats)
spec_dist_stats_t<-t(spec_dist_stats)
test_dist_stats_t<-t(test_dist_stats)


# Combining the features to non-specific, specific and test data

non_spec_final_set<-cbind(json_non_specific, non_spec_dist_stats_t)
spec_final_set<-cbind(json_specific, spec_dist_stats_t)
test_final_set<-cbind(json_test, test_dist_stats_t)


rm(non_spec_dist_stats, spec_dist_stats, test_dist_stats, 
   non_spec_dist_stats_t, spec_dist_stats_t, test_dist_stats_t)

#########################################################################################



#####################################
#  PART D: Feature Exploration
#####################################


# Section 1 of 2: Plots

# Using the non-specific final dataset to create plots of noise coverage, building coverage  
# and average distance to road with fraction yellow zone 


jpeg('graphs/frac_covrd.jpg')
non_spec_final_set%>%
  ggplot(aes(cov_rd, fraction_yellow_zone, col=scenario))+
  geom_point()+
  xlab("Noise coverage")+
  ylab("Fraction ")+
  ggtitle("Fraction Yellow Zone Vs Noise Coverage")
dev.off()


jpeg('graphs/frac_covbldg.jpg')
non_spec_final_set%>%
  ggplot(aes(cov_bldg, fraction_yellow_zone, col=scenario))+
  geom_point()+
  xlab("Building coverage")+
  ylab("Fraction ")+
  ggtitle("Fraction Yellow Zone Vs Building Coverage")
dev.off()

jpeg('graphs/frac_dist.jpg')
non_spec_final_set%>%
  ggplot(aes(avg_dist_rd, fraction_yellow_zone, col=scenario))+
  geom_point()+
  xlab("Average distance to road")+
  ylab("Fraction ")+
  ggtitle("Fraction Yellow Zone Vs Average Distance to Road")
dev.off()

jpeg('graphs/frac_spec.jpg')
spec_final_set%>%
  ggplot(aes(fraction_yellow_zone, cov_bldg, col="Building"))+
  geom_point()+
  geom_point(aes(fraction_yellow_zone, cov_rd, col="Noise"))+
  ylab("Building/Noise coverage")+
  xlab("Fraction ")+
  ggtitle("Fraction Yellow Zone Vs Coverage of Specific Site")+
  theme(legend.position = "bottom")
dev.off()


jpeg('graphs/frac_spec_dist.jpg')
spec_final_set%>%
  ggplot(aes(fraction_yellow_zone, avg_dist_rd))+
  geom_point(col="blue")+
  ylab("Average distance to road")+
  xlab("Fraction ")+
  ggtitle("Fraction Yellow Zone Vs Average distance to road of Specific Site")
dev.off()



# # Section 2 of 2: Correlation table and plots


cor_table<-cor(non_spec_final_set[,c(5:28)])
cor_table_frac<-as.data.frame(cor(non_spec_final_set[,c(6:28)], non_spec_final_set[,5]))
cor_table_frac$feature<-rownames(cor_table_frac)
rownames(cor_table_frac)<-NULL
colnames(cor_table_frac)<-c("cor_frac","feature")


jpeg('graphs/cor_frac.jpg')
cor_table_frac%>%
  ggplot(aes(factor(feature),cor_frac, colour=cor_frac))+
  geom_point(size =4)+
  scale_color_gradient2(cor_table_frac$cor_frac, low= "blue", mid="white", high= "red")+
  theme(panel.background = element_rect(fill = "slategrey",
                                        colour = "slategrey",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")
  )+
  theme(legend.title = element_blank())+
  labs(fill = "Correlation")+
  xlab("Features")+
  ylab("Correlation")+
  ggtitle("Correlation Across Different Features")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1))
dev.off()

col<- colorRampPalette(c("blue", "white", "red"))(20)
jpeg('graphs/cor_heat.jpg')
heatmap(cor_table, col=col, symm=TRUE)
dev.off()


# Listing correlations by absolute value

#cor_table_frac%>%arrange(desc(abs(cor_frac)))



#####################################
#  PART E: Create Train/Test Sets
#####################################


# Below is the complete list of all the 23 features 


# cov_rd, cov_bldg, cov_ratio, bldg_avg, bldg_median, bldg_max, bldg_min, bldg_diff, avg_dist_rd, min_dist_rd,
# ht_min_dist_rd, cov_buffer1, cov_buffer2, cov_buffer3, cov_buffer, ht_buffer1, ht_buffer2, ht_buffer3, 
# ht_buffer4, htmax_buffer1, htmax_buffer2, htmax_buffer3, htmax_buffer4


# Creating a vector of x values from 10 of them 

xvalues<-c("cov_rd",
           "cov_bldg",
           "cov_ratio",
           "htmax_buffer1",
           "htmax_buffer3",
           "htmax_buffer4",
           "cov_buffer1",
           "cov_buffer3",
           "cov_buffer4",
           "avg_dist_rd")


set_x<-rbind(non_spec_final_set[,xvalues], spec_final_set[,xvalues])
set_y<-c(non_spec_final_set[,"fraction_yellow_zone"], spec_final_set[,"fraction_yellow_zone"])


combined_set<-data.frame(y=set_y, set_x)

set.seed(123)

t_index<-createDataPartition(y= combined_set$y, p=0.2, list = FALSE)

testset<-combined_set[t_index,]
trainset<-combined_set[-t_index,]


#To be used only during final RMSE calulations
test_set_x<-test_final_set[,xvalues]
test_set_y<-test_final_set[,"fraction_yellow_zone"]

rm(t_index, set_x, set_y, col, p, p1, i)





#####################################
#  PART F: Model Implementation
#####################################


# 1) creates the set of all the models and generates all the RMSEs for comparison
# 2) fine tunes all the models and generates all the RMSEs for comparison
# 3) includes cross validation where seemed fit


# Uses BOTH the non-specific train dataset and the specific train dataset combined together as 
# the train set and partitions it into train and test dataset



# 8 models: Average Baseline, Guess, KNN, SVM Radial, Regression Tree, Random Forest, Linear Regression and GamLoess                    



# Function to calculate RMSE 

create_rmse<-function(x,y){
  rmse<-sqrt(sum((x-y)^2)/length(x))
  return(rmse)
}


#rmse_all to be used as a summary of all rmses. Initialize 
rmse_c_all<-NULL


# Model Number 1 : Guess model (model_guess)  RMSE: rmse_c_guess

model_c_guess<-runif(length(testset$y))

rmse_c_guess<-create_rmse(model_c_guess, testset$y)

# create a table to store all model results for comparison
rmse_c_all<-data.frame(method="Guess", tuned="N/A", RMSE=rmse_c_guess)



# Model Number 2: Baseline Model : Average Model (model_c_avg)  RMSE: rmse_c_avg

y_c_mean <- mean(trainset$y)
model_c_avg<-rep(y_c_mean, length(testset$y))

rmse_c_avg<-create_rmse(model_c_avg, testset$y)

# each rbind statement appends the new RMSE result at the end
rmse_c_all<-rbind(rmse_c_all, data.frame(method ="Average", tuned ="N/A", RMSE = rmse_c_avg))

# kable() from knitr package makes the summary appear as an aligned table, which is easy to read.
rmse_c_all %>% knitr::kable()


# Model Number 3 : KNN regression model (model_knn)  RMSE: rmse_knn using train function

model_knn<-train(trainset[,-1], trainset$y, method="knn",
                 tuneGrid = data.frame(k = seq(3, 50, 2))) 
model_knn$bestTune
ggplot(model_knn, highlight=TRUE)
knn_y<-predict(model_knn, testset[,-1])
rmse_knn<-create_rmse(knn_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="KNN", tuned="Y", RMSE=rmse_knn))
rmse_c_all %>% knitr::kable()


# Model Number 4 : Linear regression model (model_c_lm)  RMSE: rmse_c_lm
model_c_lm<-lm(trainset$y ~ ., data=trainset)
lm_c_y<-predict(model_c_lm, testset)
rmse_c_lm<-create_rmse(lm_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="Linear Reg.", tuned="N/A", RMSE=rmse_c_lm))
rmse_c_all %>% knitr::kable()


# Model Number 5a : GamLoess regression model (model_c_loess)  RMSE: rmse_c_loess

model_c_loess<-train(trainset[,-1], trainset$y, method="gamLoess") 
loess_c_y<-predict(model_c_loess, testset)
rmse_c_loess<-create_rmse(loess_c_y, testset$y)

rmse_c_all<-rbind(rmse_c_all, data.frame(method="GamLoess", tuned="N", RMSE=rmse_c_loess))
rmse_c_all %>% knitr::kable()



# Tuned Model Number 5b : GamLoess regression model (model_c_loess) with fine tuning for hyper parameter span 
#                         RMSE: rmse_c_loess2

model_c_loess2<-train(trainset[,-1], trainset$y, method="gamLoess", 
                      tuneGrid = data.frame(span = seq(0.025, 0.75, 0.025), degree = 1),
                      trControl = trainControl(method = "repeatedcv", number = 10, repeats=3))
model_c_loess2$bestTune

ggplot(model_c_loess2, highlight=TRUE)
loess2_c_y<-predict(model_c_loess2, testset)
rmse_c_loess2<-create_rmse(loess2_c_y, testset$y)

rmse_c_all<-rbind(rmse_c_all, data.frame(method="GamLoess", tuned="Y", RMSE=rmse_c_loess2))
rmse_c_all %>% knitr::kable()


# Model Number 6a : Regression Tree model (model_c_regtree)  RMSE: rmse_c_regtree

set.seed(123)
model_c_regtree <- tree(trainset$y~., trainset)
regtree_c_y<-predict(model_c_regtree, testset)

rmse_c_regtree<-create_rmse(regtree_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="Reg Tree", tuned="N", RMSE=rmse_c_regtree))
rmse_c_all %>% knitr::kable()

# Tuning Model Number 6b : Regression Tree Model (model_c_tree2)  with fine tuning for complex parameter cp
#                          RMSE: rmse_c_tree2


model_c_tree2<-train(trainset[,-1], trainset$y, method="rpart", 
                     tuneGrid = data.frame(cp = seq(0, 0.05, len=25)),
                     trControl = trainControl(method = "repeatedcv", number = 10, repeats=3),) 
model_c_tree2$bestTune

ggplot(model_c_tree2, highlight=TRUE)

tree2_c_y<-predict(model_c_tree2, testset)

rmse_c_tree2<-create_rmse(tree2_c_y, testset$y)

rmse_c_all<-rbind(rmse_c_all, data.frame(method="Reg Tree", tuned="Y", RMSE=rmse_c_tree2))
rmse_c_all %>% knitr::kable()


# Model Number 7a : Support Vector Machine model (model_c_svm)  RMSE: rmse_c_svm)


model_c_svm<-train(trainset[,-1], trainset$y, method="svmRadial") 
svm_c_y<-predict(model_c_svm, testset[,-1])

rmse_c_svm<-create_rmse(svm_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="SVM Radial", tuned="N", RMSE=rmse_c_svm))
rmse_c_all %>% knitr::kable()


# Model Number 7b : Support Vector Machine model (model_c_svm) with fine tuning for hyper parameters: tunelength
#                   RMSE: rmse_c_svm2)


model_c_svm2<-train(trainset[,-1], trainset$y, method="svmRadial",
                    trControl = trainControl("cv", number = 10),
                    tuneLength = 10) 
svm2_c_y<-predict(model_c_svm2, testset[,-1])

rmse_c_svm2<-create_rmse(svm2_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="SVM Radial", tuned="Y", RMSE=rmse_c_svm2))
rmse_c_all %>% knitr::kable()



# Model Number 8a : Random Forest Tree model (model_rf)  RMSE: RMSE=rmse_c_rforest

set.seed(123)
model_c_rforest <- randomForest(trainset$y~., trainset)
rforest_c_y<-predict(model_c_rforest, testset)

rmse_c_rforest<-create_rmse(rforest_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="Random Forest", tuned="N", RMSE=rmse_c_rforest))
rmse_c_all %>% knitr::kable()


# Variable Importance Plot
imp <- as.data.frame(varImpPlot(model_c_rforest))
imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL  

jpeg('graphs/forest_varImp.jpg')
ggplot(imp, aes(x=reorder(varnames, -IncNodePurity), weight=IncNodePurity, fill=varnames)) + 
  geom_bar() +
  scale_fill_discrete(name="Variable Group") +
  ylab("IncNodePurity") +
  xlab("Variable Name") +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 45, hjust = 1))+
  theme(legend.position = "none")+
  ggtitle("Variable Importance of Random Forest")
dev.off()

# Model Number 8b : Random Forest Tree model (model_rf) with fine tuning for hyper parameters: mtry, ntree
#                   RMSE: RMSE=rmse_c_rforest2

plot(model_c_rforest)

t <- tuneRF(trainset[, -1], trainset[, 1],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 400,
            trace = TRUE,
            improve = 0.01)

# Tuned model - lowest mtry = 6, ntree = 400
model_c_rforest2 <- randomForest(trainset$y~., trainset, ntree = 400, mtry = 6)

rforest2_c_y<-predict(model_c_rforest2, testset)

rmse_c_rforest2<-create_rmse(rforest2_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="Random Forest", tuned="Y", RMSE=rmse_c_rforest2))
rmse_c_all %>% knitr::kable()



####################################################
#  PART G: Model Performance on final Test Site Data
####################################################

# Using the Test site data to predict the fraction yellow zone
site_y<-predict(model_c_rforest2, test_set_x)

# Final RMSE results:
rmse_site<-create_rmse(site_y, test_set_y)
rmse_site

# Ensemble Model: Average model using Regression Tree, SVM and Random Forest
# predict fraction yellow zone with the 3 models
site_y_rf<-predict(model_c_rforest2, test_set_x)
site_y_tree<-predict(model_c_tree2, test_set_x)
site_y_svm<-predict(model_c_svm2, test_set_x)

# Compute new fraction yellow zone as average of the above three predictions
site_avg_y<-(site_y_rf+site_y_tree+site_y_svm)/3

# Final Ensemble Model results
rmse_site_avg<-create_rmse(site_avg_y, test_set_y)
rmse_site_avg
