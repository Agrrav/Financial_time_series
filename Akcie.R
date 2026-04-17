## Working directory
setwd("C:/Users/Karolina/Documents/Doktorat/Danko_clanek")

## Libraries
library(ggplot2)
library(tseries)
library(dtwclust)
library(rlang) #for aes in ggplot
library(caret) #normalising data
library(clusterCrit)
library(dplyr)
library(patchwork)
library(tidyverse)

## Data
data_raw <- read.csv2("inputKarolinaNew2withoutNA.csv")
data_raw[,2] <- as.Date.character(data_raw[,2])

data <- as.data.frame(t(data_raw[,-c(1,2)]))
colnames(data) <- data_raw[,2]

trade_names <- colnames(data_raw)[-c(1,2)]
time <- data_raw[,2]

trade_plots <- list()

for(i in 1:47){
  trade_plots[[i]] <- ggplot(data_raw, aes_string(x = "date", y = trade_names[i])) +
    geom_line() + 
    labs(title = trade_names[i],
         x = "Date", y = "Trading price")
}

trade_plots[[1]]+trade_plots[[2]]+trade_plots[[3]]

################################################################################

process <- preProcess(t(data), method = c("center", "scale"))
data_norm <- as.data.frame(t(predict(process, t(data))))

dtw_distmat_norm <- as.matrix(proxy::dist(data_norm, method = "dtw", upper = TRUE, diag = TRUE))

## Windows
step <- 9
n <- 2599 - step
trade_windows <- list()

for(i in 1:n){
  trade_windows[[i]] <- data_norm[,c(i:(i+step))]
  print(i)
}


clustering_func <- function(n,k){
  result <- list()
  result[[1]] <- lapply(c(1:n), function(x)
    tsclust(trade_windows[[x]], type = "h", k = k, # so that the first list element isnt NULL 
            distance = "dtw_basic", 
            seed = 42,
            control = hierarchical_control(method = "complete")))
    
  result[[2]] <- unlist(lapply(c(1:n), function(x)
    intCriteria(as.matrix(data_norm), as.integer(result[[1]][[x]]@cluster), c("Calinski_Harabasz"))))
  return(result)
}

test <- clustering_func(n, 2)


## Visualising the time series in each group

group_plot_func <- function(i){
  df <- as.data.frame(t(trade_windows[[i]]))
  
  df_long <- df %>%
    mutate(time = colnames(trade_windows[[i]])) %>%
    pivot_longer(-time, names_to = "series", values_to = "value")
  
  cluster_df <- data.frame(
    series = trade_names,
    cluster = as.factor(test[[1]][[i]]@cluster)
  )
  
  df_long <- df_long %>%
    left_join(cluster_df, by = "series")
  
  ggplot(df_long, aes(x = time, y = value, group = series, color = series)) +
    geom_line(alpha = 0.4) +
    facet_wrap(~ cluster,
               labeller = labeller(cluster = function(x) paste("Cluster", x))) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

group_plot_func(1)

## Regression line
comp_coef <- c()
models_list <- list()

for(i in 1:n){
  models_list[[i]] <- list()
  
  regres_data1 <- colMeans(trade_windows[[i]][which(test[[1]][[i]]@cluster == 1),])
  regres_data2 <- colMeans(trade_windows[[i]][which(test[[1]][[i]]@cluster == 2),])
  
  regres_data <- data.frame(Time = as.Date.character(colnames(trade_windows[[i]])),
                            k1 = regres_data1,
                            k2 = regres_data2)
  
  models_list[[i]][[1]] <- lm(k1 ~ Time, data = regres_data)
  models_list[[i]][[2]] <- lm(k2 ~ Time, data = regres_data)
  
  intercept1 <- models_list[[i]][[1]]$coefficients[2]
  intercept2 <- models_list[[i]][[2]]$coefficients[2]
  
  comp_coef[i] <- (intercept1*test[[1]][[i]]@clusinfo$size[1]+
                  intercept2*test[[1]][[i]]@clusinfo$size[2])/47
  
  lm_model_data <- data.frame(Time = as.Date.character(colnames(trade_windows[[i]])),
                              k1 = regres_data1,
                              k2 = regres_data2,
                              fit1 = models_list[[i]][[1]]$fitted.values,
                              fit2 = models_list[[i]][[2]]$fitted.values)
  
  models_list[[i]][[3]] <- ggplot(lm_model_data, aes(x= Time)) +
    geom_point(aes(y = k1)) + 
    geom_point(aes(y = k2)) + 
    geom_line(aes(y = fit1, color = "1"), linewidth = 1) +
    geom_line(aes(y = fit2, color = "2"), linewidth = 1) +
    labs(y = "Prototype function") +
    scale_color_manual(name = "Cluster", values = c("1" = "#B3CC76", "2" = "#8f76cc"))
}

models_list[[1]][[3]]

## Coefficient plot
plot(y = comp_coef, x = time[4:(n+3)], type = "l")

which(comp_coef == min(comp_coef))
which(comp_coef == max(comp_coef))
length(comp_coef)
length(time[4:(n+3)])
