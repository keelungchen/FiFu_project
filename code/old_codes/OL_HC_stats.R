####LIBRARIES####
library(tidyverse) # you need to load the packages each time you use R

####DATA####

#import your .csv file
pa_data <- read_csv("OL_HC_plots.csv")

####CLASS CHECK####

summary(pa_data)
pa_data$sites <- as.factor(pa_data$sites)
pa_data$status <- as.factor(pa_data$status)
pa_data$year <- as.factor(pa_data$year)
pa_data$ID <- as.factor(pa_data$ID)
pa_data$surface_area <- as.numeric(pa_data$surface_area)
pa_data$sa_change <- as.numeric(pa_data$sa_change)
pa_data$change_ratio <- as.numeric(pa_data$change_ratio)

summary(pa_data)
head(pa_data)

hist(pa_data$surface_area)
hist(log(pa_data$surface_area))
hist(pa_data$sa_change)
hist(pa_data$ps_ratio[pa_data$year=='2020'])
hist(pa_data$ps_ratio)

#製作周長面積比的分組

pa_data$ps_class[pa_data$ps_ratio<=50 ] <- 'S'
pa_data$ps_class[pa_data$ps_ratio>50 & pa_data$ps_ratio<=90 ] <- 'M'
pa_data$ps_class[pa_data$ps_ratio>90 ] <- 'L'
pa_data$ps_class <- as.factor(pa_data$ps_class)



# Scatter plot



# calculate the proportion of fates (MBIO610)
aggregate(data_2022[[surface_area]], by=list(Category=data_2022[[status]]), FUN=sum)

sum = data_2022 %>% 
  group_by(status) %>% 
  summarise(Area = sum(surface_area))
t = sum(sum$Area, na.rm=TRUE)
p = sum$Area/t
p

sum = data_2022 %>% 
  group_by(status) %>% 
  summarise(Area = sum(abs(sa_change)))
sum
t = sum(sum$Area, na.rm=TRUE)
p = sum$Area/t
p


####PLOT####

#畫百分比長條圖

pa_data$status <- factor(pa_data$status, levels = c("G", "S", "D", "Fi","Fu","Dis","N"))

p1 = ggplot(filter(pa_data, year=='2022'), aes(x = sites, fill = status,na.rm = TRUE)) + 
  geom_bar(position = "fill") +
  xlab("Site") + 
  ylab("Proportion of colonies") +
  scale_fill_manual(values = c("#33FF33", "#FF3333", "#E93EFF", "#FFFF77","#FFFF77","#FFFF77","#66FFFF"))

#畫變動區域的面積長條圖

p2 = ggplot(data = filter(pa_data, year=='2022'), aes(x = sites, y = abs(sa_change)*10000, fill = status)) +
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_manual(values = c("#33FF33", "#FF3333", "#E93EFF", "#FFFF77","#FFFF77","#FFFF77","#66FFFF")) + 
  ylab("Change in area")
p2

#體型與生長變動散佈圖 覺得有問題

p3 = ggplot(filter(pa_data, year=='2022',status %in% c('G','S','Fu','Fi','Dis')), aes(x=surface_area,y=sa_change))+
  geom_point()+
  geom_smooth(method=lm)
p3


#計算不同體型的數量

sum(pa_data$year == '2020' & pa_data$surface_area < 0.007853982, na.rm=TRUE)

#依條件設定體長及畫體長

pa_data$size_class[pa_data$surface_area<=0.002827433 ] <- 'S' #size smaller than 6 cm is juvenile
pa_data$size_class[pa_data$surface_area<=0.007853982 & pa_data$surface_area > 0.002827433] <- 'M'
pa_data$size_class[pa_data$surface_area > 0.007853982] <- 'L'
pa_data$size_class <- as.factor(pa_data$size_class)
summary(pa_data)

# plot the p/a ratio distribution with size (MBIO610)
data_2020 = filter(pa_data, year=='2020')
data_2020
plot(log10(data_2020$surface_area), log10(data_2020$ps_ratio),
     pch = 10,
     col = factor(data_2020$ps_class))
abline(v=log10(pi*0.03^2))
abline(h=log10(45))

data_2022 = filter(pa_data, year=='2022')
data_2022
plot(data_2022$surface_area, data_2022$ps_ratio,
     pch = 10,
     col = factor(data_2022$size_class))

data_2022 %>% 
  filter(size_class != 'S') %>% 
  ggplot(aes(x = surface_area, y = ps_ratio, col = size_class, label = ID)) + 
  geom_point(pch = 10) +
  geom_text(hjust = 0.5, vjust = -1) +
  labs(color = "Size Class")

# calculate mean p/a ratio of each size



pa_data
plot2 <-
  ggplot(filter(pa_data, year=='2020'), aes(x = size_class, y = sa_change, fill = size_class)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "black",
              position = position_jitter(0.21)) +
  theme_classic()

plot2

#看死亡的哪種尺寸占最多

p4 = ggplot(filter(pa_data, year=='2020'), aes(x = status, fill = size_class,na.rm = TRUE)) + 
  geom_bar(position = "fill") +
  xlab("Status") + 
  ylab("Proportion of colonies")+
  geom_text(aes(label = ..count..), stat = "count", position = "fill")
p4

#計算D/S ratio 看與命運的關係

p5 = ggplot(filter(pa_data, year=='2020'), aes(x = status, y = ps_ratio)) +
  geom_boxplot() + # 箱形圖
  xlab("status") +   # X 軸標示文字
  ylab("ps_ratio")   # Y 軸標示文字
p5

#計算D/S ratio 看與面積變化的關係

p6 = ggplot(filter(pa_data, year=='2020',status %in% c('G','S','Fu','Fi','Dis')), aes(x=ps_ratio,y=sa_change))+
  geom_point()+
  geom_smooth(method=lm)
p6

#計算尺寸與生長率有無差異
p7 <-
  ggplot(filter(pa_data, year=='2020'), aes(x = size_class, y = sa_change, fill = size_class)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "black",
              position = position_jitter(0.21)) +
  theme_classic()

p7

p8 <-
  ggplot(filter(pa_data, year=='2020'), aes(x = size_class, y = change_ratio, fill = size_class)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "black",
              position = position_jitter(0.21)) +
  theme_classic()
p8

#ANALYSIS
#simple one-way ANOVA
anova <- aov(sa_change~size_class, data = filter(pa_data, year=='2020'))
summary(anova)

check_model(anova) 

#post hoc contrasts (Tukey test)
TukeyHSD(anova)

#ratio
anova2 <- aov(change_ratio~size_class, data = filter(pa_data, year=='2020'))
summary(anova2)

check_model(anova2) 

#post hoc contrasts (Tukey test)
TukeyHSD(anova2)

##命運和PS ratio分組的關係

p9 = ggplot(filter(pa_data, year=='2020'), aes(x = status, fill = ps_class,na.rm = TRUE)) + 
  geom_bar(position = "fill") +
  xlab("Status") + 
  ylab("Proportion of colonies")+
  geom_text(aes(label = ..count..), stat = "count", position = "fill")
p9


