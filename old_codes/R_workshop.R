install.packages("ggplot2")
install.packages("datasets")
install.packages("reshape2")

library(ggplot2)
library(datasets)
library(reshape2)

ggplot(iris)+
  geom_point(aes(Sepal.Length, Sepal.Width, color = Species, shape = Species))

se <- function(x) sqrt(var(x)/length(x))

means <- setNames(aggregate(Sepal.Width~Species, data = iris, FUN = mean),nm=c("Species","mean_width"))
means$se <- aggregate(Sepal.Width~Species, data=iris, FUN=se)[,-1]
ggplot(means, aes(Species, mean_width))+
  geom_point()+
  geom_errorbar(aes(ymin=mean_width-se,ymax=mean_width+se),width=0.2)

ggplot(iris)+
  geom_line(aes(Sepal.Length, Sepal.Width, color=Species),size=1)
ggplot(iris)+
  geom_path(aes(Sepal.Length, Sepal.Width, color=Species),size=1)

ggplot(iris)+
  geom_histogram(aes(x=Sepal.Width),bins=20)

ggplot(iris)+
  geom_boxplot(aes(x=Species,y=Petal.Length))

ggplot(iris,aes(x=Species,y=Petal.Length))+
  geom_violin(trim =FALSE)+
  stat_summary(fun=median, geom ="point", shape =20,size=2)
ggplot(iris,aes(x=Species,y=Petal.Length))+
  geom_violin(trim =FALSE)+
  stat_boxplot(width = 0.1, outlier.size=0.9)

data.cor <- cor(iris[sapply(iris, is.numeric)])
data.cor1 <- melt(data.cor)
ggplot(data.cor1, aes(x = Var1, y=Var2,fill=value))+
  geom_tile()+
  scale_fill_gradient(high="darkgreen",low="white")
ggplot(data.cor1, aes(x = Var1, y=Var2,fill=value))+
  geom_tile()+
  scale_fill_distiller(palette="Greens")

ggplot(iris,  aes(x=Species))+
  geom_bar()
ggplot(iris,  aes(x=Species,y=Sepal.Width))+
  geom_bar(stat="identity")

ggplot(iris,aes(Sepal.Length,Sepal.Width,color=Species))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Species,nrow=3)
ggplot(iris,aes(Sepal.Length,Sepal.Width,color=Species))+
  geom_point()+
  geom_smooth()+
  facet_grid(rows = vars(Species))

ggplot(iris,aes(Sepal.Length,Sepal.Width,color=Species))+
  geom_line()+
  geom_point(aes(Petal.Length, Petal.Width))
ggplot(iris,aes(Sepal.Length,Sepal.Width,color=Species))+
  geom_line()+
  geom_point(aes(Petal.Length, Petal.Width),inherit.aes=FALSE)

iris$Population <- rep(c("A","B","C"), 50)
ggplot(iris,aes(x=Sepal.Width, y=Sepal.Length,color=Species))+
  geom_point()+
  geom_smooth(method ="lm",aes(group=interaction(Population,Species),color=Species,linetype=Population),se=FALSE)

df <- data.frame(Species = levels(iris$Species),label=c("A","B","C"),y=rep(4.5,3))
ggplot(iris,aes(x=Species,y=Sepal.Width))+
  geom_boxplot()+
  geom_text(data=df,aes(y=y),label=c("A","B","A"),size=6)

ggplot(iris,aes(x=Species,y=Sepal.Width))+
  geom_boxplot()+
  annotate(geom="text",x=levels(iris$Species),y=rep(4.5,3),label=c("A","B","A"),size=6)

plot <- ggplot(iris,aes(x=Species,y=Sepal.Width))+
  geom_boxplot()+
  annotate(geom="text",x=levels(iris$Species),y=rep(4.5,3),label=c("A","B","A"),size=6)+
  ggtitle("Sepal Width")+
  theme_classic()
plot

ggsave("workshop_plot.png",plot,height = 7,width=9.55,units = "in",device = "png",dpi=600)
