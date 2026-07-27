library(readxl)
library(dplyr)
library(tidyr)

mortalitycauses <- read_excel("global_mortality.xlsx")
lifeexpectancy <- read.csv("life-expectancy.csv")
gdppercapita <- read.csv("gdp-per-capita-worldbank.csv")

str(gdppercapita) #checking column names for the datasets
str(lifeexpectancy)
str(mortalitycauses)

gdppercapita %>%
  count(Entity, Year) %>%
  filter(n > 1)  #verifying uniqueness of country-year combos in the datasets

lifeexpectancy %>%
  count(Entity, Year) %>%
  filter(n > 1)

mortalitycauses %>%
  count(Entity, Year) %>%
  filter(n > 1)

setdiff(mortalitycauses$Entity, gdppercapita$Entity)

n_distinct(mortalitycauses$Code) #checking number of distinct codes. Can check more

analysis <- mortalitycauses %>%
  filter(!is.na(Code)) %>%
  inner_join(
    gdppercapita,
    by = c("Code", "Year")
  ) %>%
  inner_join(
    lifeexpectancy,
    by = c("Code", "Year")
  )

#Datasets were merged using ISO-3 country codes rather than country names to avoid inconsistencies in naming conventions."
#Example: Czechia vs Czech Republic

#View(analysis)

#View(analysis %>%
#  filter(Entity != Entity.x | Entity != Entity.y)) #seeing country names that differ
#with the same code.

analysis <- analysis %>%
  select(-Entity, -Entity.y) %>%
  rename(Entity = Entity.x) #removing superfluous columns

#View(analysis)

analysis %>%
  count(Code, Year) %>%
  filter(n > 1) #uniqueness survived the joining

#scope of the data set

range(analysis$Year) #1990 to 2016

n_distinct(analysis$Code) #186 countries

n_distinct(analysis$Entity) #186 again

analysis %>%
  count(Entity) %>%
  count(n) #checking how many years of data we have for each country
#179 countries have the full 27 years, and 7 have some smaller number of years

analysis %>%
  count(Entity) %>%
  filter(n < 27) #seeing which countries we have incomplete data for

analysis_clean <- analysis %>%
  group_by(Entity) %>%
  filter(n() == 27) %>%
  ungroup()

sum(is.na(analysis_clean)) #1294 missing entries

colSums(is.na(analysis_clean)) %>%
  sort(decreasing = TRUE)

analysis_clean <- analysis_clean %>%
  mutate(
    `Conflict (%)` = replace_na(`Conflict (%)`, 0),
    `Terrorism (%)` = replace_na(`Terrorism (%)`, 0)
  ) #assigned 0 to missing conflict and terrorism entries.

# Note that Cause-of-death percentages were retained as provided by the source. 
#The sum of reported causes varied slightly around 100%, likely due to 
#independent estimation and rounding, or just not all causes being accounted for.

#View(analysis_clean)

mortality_cols <- names(analysis_clean)[
  grepl("\\(%\\)", names(analysis_clean)) 
] #gets all the cause of death column names

analysis_clean %>%
  summarise(across(all_of(mortality_cols), mean)) %>%
  pivot_longer(
    everything(),
    names_to = "cause",
    values_to = "average_percent"
  ) %>%
  arrange(desc(average_percent)) #this is of limited value since it's not a 
#population weighted average of course


#PCA on mortality causes. Want to reduce the 32 causes to like 3, 4, or 5 dimensions
# Can do cross-validation on this choice, or look for the elbow in percent variance explained

mortality_data <- analysis_clean %>%
  select(all_of(mortality_cols))

mortality_pca <- prcomp(
  mortality_data,
  scale. = TRUE
)

summary(mortality_pca)

library(ggplot2)

variance <- mortality_pca$sdev^2 /
  sum(mortality_pca$sdev^2)

ggplot(
  data.frame(
    PC = 1:length(variance),
    Variance = variance
  ),
  aes(x = PC, y = Variance)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Scree Plot of Principal Components",
    x = "Principal Component",
    y = "Proportion of Variance Explained"
  ) +
  theme_minimal()

eigenvalues <- mortality_pca$sdev^2

eigenvalues #Kaiser criterion says to keep components based on the number of 
#eigenvalues of size at least 1. That gives 8 here. The 9th is .98173, so
#that's a judgment call. I think 5 is also reasonable based on the scree plot

#check the loadings for the principal components

analysis_pca <- bind_cols(
  analysis_clean,
  as.data.frame(mortality_pca$x)
) #now we have the PCA stuff as extra columns

loadings <- mortality_pca$rotation

loadings

loadings[,1] %>%
  sort(decreasing = TRUE)
#high loadings for PC1:infectious disease, childhood mortality, maternal mortality, and malnutrition
#low loadings: Cancer, aging-related chronic disease. This is reasonably interpretable

cor(analysis_pca$PC1, log(analysis_pca$`GDP.per.capita`)) #-0.8172
cor(analysis_pca$PC1, analysis_pca$`Life.expectancy`) #-0.8916

p1 <- ggplot(
  analysis_pca,
  aes(
    x = log(`GDP.per.capita`),
    y = PC1
  )
) +
  geom_point(alpha = 0.30, size = 1.2) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 1
  ) +
  labs(
    x = "Log GDP per Capita",
    y = "First Principal Component (PC1)"
  ) +
  theme_classic(base_size = 14)

p1 #plots to show this strong negative correlation

p2 <- ggplot(
  analysis_pca,
  aes(
    x = `Life.expectancy`,
    y = PC1
  )
) +
  geom_point(alpha = 0.30, size = 1.2) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 1
  ) +
  labs(
    x = "Life Expectancy (Years)",
    y = "First Principal Component (PC1)"
  ) +
  theme_classic(base_size = 14)

p2 # now vs Life.expectancy



#now we look at the second PC, PC2. 

loadings[,2] %>%
  sort(decreasing = TRUE)
#high loadings for PC2: drowning, road accidents, fire, homicide, so somewhat of
#an "injury axis", but also Diabetes, Kidney disease, Liver disease, etc,
#so some chronic diseases are part of this axis

cor(analysis_pca$PC2, log(analysis_pca$`GDP.per.capita`)) #-0.1176459
cor(analysis_pca$PC2, analysis_pca$`Life.expectancy`) #0.0310787. No longer 
#clear correlations

ggplot(
  analysis_pca,
  aes(
    x = log(`GDP.per.capita`),
    y = PC2
  )
) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") #plots PC2 vs log(GDP.per.capita)

p3 <- ggplot(
  analysis_pca,
  aes(
    x = `Life.expectancy`,
    y = PC2
  )
) +
  geom_point(alpha = 0.30, size = 1.2) +
  geom_smooth(
    method = "loess",
    se = TRUE,
    linewidth = 1
  ) +
  coord_cartesian(
    xlim = c(40, 85)
  ) +
  labs(
    x = "Life Expectancy (Years)",
    y = "Second Principal Component (PC2)"
  ) +
  theme_classic(base_size = 14)

p3 # now vs Life.expectancy 
#This plot is interesting since we see some non-monotone behavior!
# PC2 increases wrt Life expectancy at first, up to about age 66, then
# it begins to decrease wrt Life expectancy. 

loadings[,3] %>%
  sort(decreasing = TRUE)
#strong loadings: Kidney disease, Respiratory diseases, Digestive diseases
#Dementia, Parkinson's disease, Cancers

ggplot(
  analysis_pca,
  aes(
    x = `Life.expectancy`,
    y = PC3
  )
) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") # PC3 vs Life.expectancy
#not as interesting a relationship as we got doing this plot for PC2


#Now let's do a K-means clustering analysis using PC1-PC5.
#Since our observations are based on country-year, we can actually see
#if and when countries change their cluster. 

#ultimately just treated this as exploratory and only presented the
#hierarchical clustering in the report

cluster_data <- analysis_pca %>%
  select(PC1:PC5)

library(factoextra)

set.seed(123)

fviz_nbclust(
  cluster_data,
  kmeans,
  method = "wss"
) #getting an idea of the number of clusters to use. 
# look for diminishing returns

set.seed(123)

fviz_nbclust(
  cluster_data,
  kmeans,
  method = "silhouette"
) #higher is better

#both methods suggest K=6 is reasonable, and K=4 at least, but this can be
#seed dependent.

set.seed(123)

silhouette_scores <- data.frame(
  k = 2:10,
  silhouette = NA
)

for(k in 2:10){
  
  set.seed(123)
  
  km <- kmeans(
    cluster_data,
    centers = k,
    nstart = 50
  )
  
  sil <- cluster::silhouette(
    km$cluster,
    dist(cluster_data)
  )
  
  silhouette_scores$silhouette[k-1] <- mean(sil[,3])
}

silhouette_scores

set.seed(123)

kmeans_model <- kmeans(
  cluster_data,
  centers = 4,
  nstart = 50
)

analysis_clustered <- analysis_pca %>%
  mutate(
    cluster = factor(kmeans_model$cluster)
  ) #adding cluster labels

ggplot(
  analysis_clustered,
  aes(
    x = PC1,
    y = PC2,
    color = cluster
  )
) +
  geom_point(alpha = 0.5) #visualize clusters by plotting with PC1 and PC2

ggplot(
  analysis_clustered,
  aes(
    x = PC1,
    y = PC3,
    color = cluster
  )
) +
  geom_point(alpha = 0.5) #now try using PC1 and PC3


#Let's view which clusters our country-year observations are in

country_year_cluster = analysis_clustered %>%
  select(Entity, Year, cluster)

#View(country_year_cluster)

analysis_clustered %>%
  filter(cluster == 6) %>% #change this number as you wish, 1 to 6
  count(Entity, name = "years_in_cluster") %>%
  arrange(desc(years_in_cluster)) %>%
  print(n=50)
  #Going cluster by cluster to see
# which countries are in which and for how long



#For the sake of interpretation, it could be interesting to try K=4 as well. 

set.seed(123)

kmeans_model <- kmeans(
  cluster_data,
  centers = 4,
  nstart = 50
)

analysis_clustered <- analysis_pca %>%
  mutate(
    cluster = factor(kmeans_model$cluster)
  ) #adding cluster labels

ggplot(
  analysis_clustered,
  aes(
    x = PC1,
    y = PC2,
    color = cluster
  )
) +
  geom_point(alpha = 0.5) #visualize clusters by plotting with PC1 and PC2

ggplot(
  analysis_clustered,
  aes(
    x = PC1,
    y = PC3,
    color = cluster
  )
) +
  geom_point(alpha = 0.5) #now try using PC1 and PC3


#Let's view which clusters our country-year observations are in

country_year_cluster = analysis_clustered %>%
  select(Entity, Year, cluster)

#View(
#  country_year_cluster
#)

analysis_clustered %>%
  filter(cluster == 2) %>% #change this number as you wish, 1 to 4
  count(Entity, name = "years_in_cluster") %>%
  arrange(desc(years_in_cluster)) %>%
  print(n=54)
#Going cluster by cluster to see
# which countries are in which and for how long


###################################################################


#We can also consider hierarchical clustering. 

dist_mat <- dist(cluster_data) #distance matrix from PC1-PC5 values

hc <- hclust(
  dist_mat,
  method = "ward.D2"
) #Ward's method linkage 

sort(-hc$height) #checking the merge heights

plot(
  hc,
  labels = FALSE,
  hang = -1,
  main = "Hierarchical Clustering of Mortality Profiles",
  sub = NULL,
  xlab = "",
  ylab = "Ward Linkage Height",
  cex.main = 1.2
)

abline(
  h = 105,
  lty = 2,
  lwd = 2
) #dendrogram plot
#Looks like splitting into 5 clusters is interesting (height around 105 on dendrogram)

hc_clusters <- cutree(hc, k = 5)

analysis_hc <- analysis_pca %>%
  mutate(cluster = hc_clusters) #attach back into analysis

table(kmeans_model$cluster)
table(hc_clusters) #can compare sizes of the clusters between methods


table(
  KMeans = kmeans_model$cluster,
  Hierarchical = hc_clusters
) #roughly strong on the diagonal is a good sign this isn't an artifact of the
#clustering method, though "on the diagonal" depends on the labeling!



#New plots. With more clusters, need to try to fix the colors to see better

analysis_hc <- analysis_hc %>%
  mutate(cluster = factor(cluster)) #fixing a type issue

library(viridis)

ggplot(
  analysis_hc,
  aes(PC1, PC2, color = cluster)
) +
  geom_point(alpha = 0.5) +
  scale_color_manual(
    values = c(
      "#007BFF",  # Cobalt Blue
      "#FF6347",  # Tomato Red
      "#28A745",  # Emerald Green
      "#8A2BE2",  # Blue Violet
      "#FFB000"   # Amber
    )
  ) +
  theme_classic()

#ggsave(
#"HierarchicalClusters_PC1_PC2.png",
#width = 8,
#height = 5,
#dpi = 300
#)

ggplot(
  analysis_hc,
  aes(PC1, PC3, color = cluster)
) +
  geom_point(alpha = 0.5) +
  scale_color_manual(
    values = c(
      "#007BFF",  # Cobalt Blue
      "#FF6347",  # Tomato Red
      "#28A745",  # Emerald Green
      "#8A2BE2",  # Blue Violet
      "#FFB000"   # Amber
    )
  ) +
  theme_classic()

#Now we will try to characterize the clusters. 

analysis_hc %>%
  count(cluster) %>%
  arrange(cluster) #counts for each

analysis_hc %>%
  group_by(cluster) %>%
  summarise(
    countries = n_distinct(Entity),
    observations = n()
  ) #counts and number of unique countries for each cluster

analysis_hc %>%
  group_by(cluster) %>%
  summarise(
    across(PC1:PC5, mean)
  ) #can see, for instance, that cluster 2 has the smallest PC1 and PC2 means

cluster_summary <- analysis_hc %>%
  group_by(cluster) %>%
  summarise(
    Countries = n_distinct(Entity),
    Count = n(),
    Average_Log_GDP = mean(log(`GDP.per.capita`), na.rm = TRUE),
    Average_Life_Expectancy = mean(`Life.expectancy`, na.rm = TRUE),
    .groups = "drop"
  )

cluster_summary

#View(analysis_hc %>%
#  filter(cluster == 1) %>% #change this to view different clusters
#  count(
#    World.region.according.to.OWID,
#    Entity,
#    name = "years_in_cluster"
#  ) %>%
#  arrange(
#    desc(years_in_cluster),
#    World.region.according.to.OWID,
#    Entity
#  ))
 #seeing which countries are in each cluster most of the time


countries_changed <- analysis_hc %>%
  group_by(Entity) %>%
  summarise(
    n_clusters = n_distinct(cluster),
    clusters = paste(sort(unique(cluster)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n_clusters > 1) %>%
  arrange(desc(n_clusters), Entity)

countries_changed #seeing which countries change clusters

#View(analysis_hc %>%
#  arrange(Entity, Year) %>%
#  group_by(Entity) %>%
#  mutate(
#    changed = cluster != lag(cluster)
#  ) %>%
#  filter(changed | is.na(changed)) %>%
#  select(Entity, Year, cluster))

analysis_hc %>%
  arrange(Entity, Year) %>%
  group_by(Entity) %>%
  mutate(previous_cluster = lag(cluster)) %>%
  filter(!is.na(previous_cluster),
         cluster != previous_cluster) %>%
  dplyr::select(Entity, Year, previous_cluster, cluster) %>%
  print(n = 30)


#Can actually make a directed graph for all the transitions (exclude Timor fluctuating)
#Just did this in LaTeX itself

analysis_hc %>%
  group_by(cluster) %>%
  summarise(
    mean_life = mean(Life.expectancy),
    mean_gdp = mean(log(GDP.per.capita))
  ) #more data to describe each cluster



#Now let's build a regression model to predict life-expectancy based on the
#principal component variables. 

library(MASS)

full_model <- lm(
  Life.expectancy ~ .,
  data = analysis_pca %>%
    dplyr::select(Life.expectancy, starts_with("PC"))
)

null_model <- lm(
  Life.expectancy ~ 1,
  data = analysis_pca
)

forward_aic <- stepAIC(
  null_model,
  scope = list(
    lower = null_model,
    upper = full_model
  ),
  direction = "forward",
  trace = TRUE
)

summary(forward_aic) #this selected 28 variables. Might try BIC instead to
#push for more parsimony

forward_bic <- stepAIC(
  null_model,
  scope = list(
    lower = null_model,
    upper = full_model
  ),
  direction = "forward",
  k = log(nrow(analysis_pca)),
  trace = TRUE
)

summary(forward_bic) #this selected 24 variables with essentially the same
#goodness of fit

#try LASSO:

library(glmnet)

x <- as.matrix(
  analysis_pca[, paste0("PC",1:32)]
)

y <- analysis_pca$Life.expectancy

set.seed(123)

lasso_pc <- cv.glmnet(
  x,
  y,
  alpha = 1,
  standardize = FALSE
)

coef(lasso_pc, s = "lambda.min") #this chose 31/32 vars to keep

set.seed(123) #needed since the cross validation to choose lambda has randomness

lasso_pc <- cv.glmnet(
  x,
  y,
  alpha = 1,
  standardize = FALSE
)

coef(lasso_pc, s = "lambda.1se") #changing the LASSO choice. this kept 26/32


#What about trying the original mortality variables for LASSO?

mortality_variables <- setdiff(
  names(analysis_clean),
  c(
    "Entity",
    "Code",
    "Year",
    "Life.expectancy",
    "GDP.per.capita",
    "Log.GDP",
    "World.region.according.to.OWID"
  )
)

mortality_variables


z <- as.matrix(
  analysis_pca[, mortality_variables]
)

lasso_mortality <- cv.glmnet(
  z,
  y,
  alpha = 1
)

coef(lasso_mortality, s = "lambda.min") #kept all 32 vars

coef(lasso_mortality, s = "lambda.1se") #kept 27/32 vars


pc_sds <- sapply(analysis_pca[paste0("PC", 1:32)], sd)

pc_sds #finding that, despite the large coefficients for PC31 and 32 are large,
#those don't have large sds, so the overall effect in the model is modest.

#### Now let's try to get a more parsimonious model. Here we do another
#forward selection algorithm, but we will use a CV approach instead of BIC

library(caret)

set.seed(123)

ctrl <- trainControl(
  method = "cv",
  number = 10
)

all_pcs <- paste0("PC", 1:32)

selected_pcs <- character(0)
remaining_pcs <- all_pcs

cv_forward_results <- data.frame(
  step = integer(),
  added_PC = character(),
  RMSE = numeric(),
  RMSE_SE = numeric(),
  Rsquared = numeric()
)

for(step in 1:length(all_pcs)){
  
  candidate_results <- data.frame(
    PC = remaining_pcs,
    RMSE = NA,
    RMSE_SE = NA,
    Rsquared = NA
  )
  
  for(i in seq_along(remaining_pcs)){
    
    candidate_pc <- remaining_pcs[i]
    
    predictors <- c(selected_pcs, candidate_pc)
    
    formula <- as.formula(
      paste(
        "Life.expectancy ~",
        paste(predictors, collapse = " + ")
      )
    )
    
    fit <- train(
      formula,
      data = analysis_pca,
      method = "lm",
      trControl = ctrl
    )
    
    candidate_results$RMSE[i] <- fit$results$RMSE
    candidate_results$RMSE_SE[i] <- fit$results$RMSESD
    candidate_results$Rsquared[i] <- fit$results$Rsquared
  }
  
  # choose best addition
  best_candidate <- candidate_results[
    which.min(candidate_results$RMSE), 
  ]
  
  selected_pcs <- c(selected_pcs, best_candidate$PC)
  remaining_pcs <- setdiff(remaining_pcs, best_candidate$PC)
  
  cv_forward_results <- rbind(
    cv_forward_results,
    data.frame(
      step = step,
      added_PC = best_candidate$PC,
      RMSE = best_candidate$RMSE,
      RMSE_SE = best_candidate$RMSE_SE,
      Rsquared = best_candidate$Rsquared
    )
  )
  
  print(cv_forward_results[nrow(cv_forward_results),])
}

# Find minimum RMSE and its standard error
min_rmse <- min(cv_forward_results$RMSE)

se_min <- cv_forward_results$RMSE_SE[
  which.min(cv_forward_results$RMSE)
]

# One-standard-error threshold
threshold <- min_rmse + se_min

threshold

# Smallest model within one standard error
cv_forward_results[
  cv_forward_results$RMSE <= threshold,
][1,]


#Let's get a graph showing the CV performance curve as we add PCs:
# Minimum CV RMSE
min_row <- which.min(cv_forward_results$RMSE)

min_rmse <- cv_forward_results$RMSE[min_row]
min_se <- cv_forward_results$RMSE_SE[min_row]

# One-standard-error cutoff
one_se_cutoff <- min_rmse + min_se

# Selected 9-PC model
selected_row <- which(cv_forward_results$added_PC == "PC4") 
# (PC4 is the 9th component added in your CV procedure)

# Plot

highlight_points <- rbind(
  transform(subset(cv_forward_results, step == 27),
            Model = "Minimum RMSE (27 PCs)"),
  transform(subset(cv_forward_results, step == 9),
            Model = "Selected model (9 PCs)")
)

ggplot(cv_forward_results, aes(x = step, y = RMSE)) +
  geom_line() +
  geom_point() +
  geom_point(
    data = highlight_points,
    aes(color = Model),
    size = 3
  ) +
  geom_hline(
    yintercept = min(cv_forward_results$RMSE + cv_forward_results$RMSE_SE),
    linetype = "dashed",
    color = "black"
  ) +
  scale_color_manual(
    values = c(
      "Minimum RMSE (27 PCs)" = "red",
      "Selected model (9 PCs)" = "blue"
    ),
    name = NULL
  ) +
  labs(
    x = "Number of Principal Components",
    y = "10-fold Cross-Validated RMSE"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.78, 0.82)
  )

#ggsave(
#  "cv_rmse_curve.png",
#  width = 7,
#  height = 5,
#  dpi = 300
#)



# PCs selected by the 1-SE cross-validated forward selection rule
#Now we're going to use those variables only, but train on the full data set. 
selected_pcs_9 <- c(
  "PC1", "PC3", "PC7", "PC31",
  "PC8", "PC9", "PC10", "PC15", "PC4"
)

# Construct the regression formula
formula_9 <- as.formula(
  paste(
    "Life.expectancy ~",
    paste(selected_pcs_9, collapse = " + ")
  )
)

# Fit the final model on the full dataset
cv9_model <- lm(
  formula_9,
  data = analysis_pca
)

# Summary statistics
summary(cv9_model)

#at this point, we have discovered that PC31 may be particularly interesting since
#it enters the models really early in forward selection.
loadings[,31] %>%
  sort(decreasing = TRUE)




#Now I want to use a change of basis to put my life expectancy model using only
#9 PC variables back in terms of the original 32 mortality variables:

# Selected PCs
selected_pcs <- c("PC1", "PC3", "PC7", "PC31", "PC8",
                  "PC9", "PC10", "PC15", "PC4")

# Regression coefficients for selected PCs
beta <- coef(cv9_model)[selected_pcs]

# PCA loadings for selected PCs
L <- mortality_pca$rotation[, selected_pcs]

# Original variable standard deviations
s <- mortality_pca$scale

# Original variable means
mu <- mortality_pca$center

# Coefficients on original variables
gamma <- as.numeric((L %*% beta) / s)

names(gamma) <- rownames(L)

mortality_coefs <- data.frame(
  Variable = names(gamma),
  Estimate = gamma
)

mortality_coefs
mortality_coefs_sorted <- mortality_coefs[order(mortality_coefs$Estimate, decreasing = TRUE), ]
mortality_coefs_sorted

#Now the intercept

alpha0 <- coef(cv9_model)[1] - sum(gamma * mortality_pca$center)

alpha0

#Verification:

X <- as.matrix(mortality_data[, mortality_coefs$Variable])

pred_original <- alpha0 + X %*% mortality_coefs$Estimate

max(abs(predict(cv9_model) - pred_original)) #basically zero. success.




### Let's add in a human readable decision tree to help clarify with some
# heuristics what the clusters are. 

library(rpart)
library(rpart.plot)
library(caret)

# Response variable
tree_data <- analysis_clean %>%
  mutate(
    cluster = factor(analysis_hc$cluster)
  )

# Decision tree using original mortality variables
set.seed(123)

train_index <- createDataPartition(
  tree_data$cluster,
  p = 0.8,
  list = FALSE
)

train_data <- tree_data[train_index, ]

test_data <- tree_data[-train_index, ]

cluster_tree <- rpart(
  cluster ~ .,
  data = train_data %>%
    dplyr::select(all_of(mortality_cols), cluster),
  method = "class",
  control = rpart.control(
    maxdepth = 3,
    minsplit = 30,
    cp = 0.01
  )
)

tree_predictions <- predict(
  cluster_tree,
  newdata = test_data,
  type = "class"
)

confusionMatrix(
  tree_predictions,
  test_data$cluster
)

rpart.plot(
  cluster_tree,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE
)

cluster_tree

#pdf(
#  "mortality_cluster_decision_tree.pdf",
#  width = 10,
#  height = 7
#)   saving a plot

#rpart.plot(
#  cluster_tree,
#  type = 2,
#  extra = 104,
#  fallen.leaves = TRUE
#)

#dev.off()




#What about comparing our 9 PC model to the full 32 mortality variable model
#that doesn't first run through PC geometry?

full_mortality_model <- lm(
  Life.expectancy ~ .,
  data = analysis_pca %>%
    dplyr::select(
      Life.expectancy,
      all_of(mortality_cols)
    )
)
summary(full_mortality_model)
