
library(ggplot2)
acc<-read.csv(file = "../Data/Accuracy.csv")
  ###### HISTOGRAM ########
g <- ggplot(data=acc, aes(acc$Accuracy)) + 
    geom_histogram(aes(y =..density..), 
                   breaks=seq(.74, .84, by = 0.01), 
                   col="red", 
                   fill="green", 
                   alpha = .2) + 
    geom_density(col=2) + 
    labs(title="Histogram for Accuracy") +
    labs(x="Accuracy", y="Density")
ggsave(g,"../Results/Accuracy_Boxplot_Fancy.jpg")
####### BOXPLOT #####
h <- ggplot(acc, aes(acc[[1]] , acc$Accuracy)) +
    geom_boxplot(col="red", 
                 fill="green", 
                 alpha = .2)
ggsave(h,"../Results/Accuracy_Histogram_Fancy.jpg")




