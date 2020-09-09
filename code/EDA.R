# Load required packages

pacman::p_load(pacman, rio, tidyverse, dplyr, ggplot2, rlang)

# Examine the dataframe

df <- read.csv('./data/PM_train.txt', header = FALSE, sep = ' ')
head(df)

# The dataframe contains information on engine failures
# id = aircraft engine identifier
# cycle = time in cycles
# setting 1,2,3 - operational settings
# s1-s21 - sensor measurements
# Each engine similar but different wear and tear and manufacturing condition
# Last cycle = failure point

# Get rid of V27 and V28, i/o error
df <- select(df,-V28,-V27)
head(df)

colnames(df)
# Let's change these column names
colnames(df) <- c('engine_id', 'cycle',	'setting1',	'setting2',	'setting3',	's1',	's2',	's3',	's4',	's5',
                  's6',	's7',	's8',	's9', 's10',	's11',	's12',	's13',	's14',	's15',	's16',	's17',	
                  's18',	's19',	's20',	's21')
head(df)

gdf <- group_by(df, engine_id)
enginemeans <- summarise_all(gdf, mean)

enginesds <- summarise_all(gdf, sd)

# Setting 3, s1, s5, s10, s16, s18, s19 never change and thus we can drop them from our observations

red_df <- select(df, -c('setting3', 's1', 's5', 's10', 's16', 's18', 's19'))
head(red_df)

enginemeans <- red_df %>% group_by(engine_id) %>%
  summarise_all(mean)

typeof(enginemeans)

# PCA on the engine means to inspect clustering engines together

head(enginemeans)

engmeans_scaled <- scale(select(enginemeans, -'engine_id'))
engmeans_scaled
engmeans_pca <- prcomp(engmeans_scaled)
engmeans_pca
summary(engmeans_pca)
# 81% of variance in PC1 and PC2
engmeans_pca$x[,2]

library(ggfortify)
autoplot(engmeans_pca, label=TRUE, label.size=5, alpha=0.3, size=10) + 
  theme_grey(base_size = 16)

# Looks like there are some clusters possible - 5 to 6 visually. Let's try k-means

set.seed(4)
groupss <- function(k) {
  kmeans(engmeans_scaled, k, nstart = 10 )$tot.withinss
}
maxk=15
groupss_v <- map_dbl(1:maxk, groupss)

elbow <-data.frame(1:maxk, groupss_v)

ggplot(elbow, aes(x = X1.15, y = groupss_v)) +
  geom_point(size=4) +
  geom_line(size=1) +
  theme_grey(base_size = 16) +
  xlab('Number of Clusters') +
  ylab('Total within cluster SS') +
  scale_x_continuous(breaks = seq(1, 20, by = 1))
ggplot(aes(x=1:15, y=groupss_v))+geom_point()

autoplot(kmeans(engmeans_scaled, 5), data=engmeans_scaled, label=TRUE, label.size=5, alpha=0.3, size=10) + 
  theme_grey(base_size = 16)


# Feature engineering, create lagged features
head(red_df)

test <-
  red_df %>%
  group_by(engine_id) %>%
  mutate('lag.value' = dplyr::lag(setting1, n=3, default=NA))
test
filter(test, engine_id==2)

features <- c('setting1',	'setting2',	's2',	's3',	's4',
              's6',	's7',	's8',	's9', 's11',	's12',	's13',	's14',	's15',	's17',	
              's20',	's21')

lagged_df <- data.frame(red_df)

for (i in features){
  for (j in 1:7){
    #print(paste(i,j,sep='_lag_'))
    tempv <- paste(i,j,sep='_lag_')
    lagged_df <-
      lagged_df %>%
      group_by(engine_id) %>%
      mutate(!!sym(tempv) := dplyr::lag(!!sym(i), n=j, default=NA))
  }
}

# Save the lagged dataframe
write.csv(lagged_df, './data/lagged_data.csv')

# Drop rows with NA
lagged_df_nona <- na.omit(lagged_df)

# Save the lagged dataframe
write.csv(lagged_df_nona, './data/lagged_data_nomissing.csv')

# Engineer the target column - will the engine fail in 7 cycles
lagged_df_nona <-
  lagged_df_nona %>%
  group_by(engine_id) %>%
  mutate(target = ifelse((cycle > max(cycle) - 7),1,0))

# Save the lagged dataframe with target
write.csv(lagged_df_nona, './data/lagged_data_nomissing.csv')

# end program cleanup
# Clear data
rm(list = ls())
detach("package:datasets", unload = T)
p_unload(all)  

graphics.off()

cat("\014") 

