# Graduate students final project
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggmap)
install.packages("ggmap")
library(plyr)


##Reading-in data
a<-read.csv('EST-PR-PlanktonChemTax_2.csv')
a

# Data is in csv format and stored in data frame structure
##data frame

view(a)
a.df<-as.data.frame(a, dimnames=T)
a.df


##Example of indexing (indexing 3rd row 9th column)
names(a)

a[3,9]
# option 2
a[3,"SampleType" ]

##Subsetting "IBYC" from BottleName variable

unique(a$BottleName)

#Subsetting
sub<-subset(a,BottleName=="IBYC")
sub


##Ordering

a.order=a[order(a$Longitude),]
a.order
#option 2 of ordering/sorting
a.arrange <- arrange(a, Longitude, SampleName)
a.arrange

##Summarizing

# group by SampleName 
unique(a$SampleName)
c=tapply(X=a$Temp,INDEX=list(a$SampleName),FUN=fivenum)
c

##(2) Summarizing salinity of "IBYC" subset of BottleName
sum2=summary(a[a$BottleName=="IBYC",]$Longitude/a[a$BottleName=="IBYC",]$Salinity)
sum2

sum=summary(a[a$BottleName=="IBYC",]$Longitude/a[a$BottleName=="IBYC",]$Temp)
sum

#using piping
sum3=a %>% group_by(a$SampleName) %>% summarise(fivenum(Temp, na.rm = T))
sum3


# mean of Distance

dist.mean<-tapply(X=a$Distance, INDEX=list(a$SampleName), FUN=mean)
dist.mean
head(dist.mean)

library(reshape2)
#convert object to data frame
# creating a DF from mean of Distance
mean.df = data.frame(SampleNames = names(dist.mean), mean = as.numeric(dist.mean))
mean.df


# standard deviation of salinity
salinity.sd<-tapply(X=a$Salinity, INDEX=list(a$SampleName), FUN=sd)
salinity.sd
head(salinity.sd)

#creating DF from Standard deviation of Salinity
sd.df = data.frame(SampleNames = names(salinity.sd), sd = as.numeric(salinity.sd))
sd.df



# merging the two data frames of mean of distance and standard deviation of salinity

merge<-merge(x=mean.df,y=sd.df, by="SampleNames")
merge

#Join
Join=full_join(mean.df, sd.df, by = "SampleNames")
Join



#Custom function(s) (10 points)

unique(a$BottleName)
#levels;6      8      12     16     18     20     22     24     IBYC   Nelson Ocean  4      0      NELSON

myfunction<-function(x){
  if(x=="a")
    "Nelson"
  else if(x=="b")
    "12"
  else if(x=="c")
    "16"
  else if(x=="d")
    "18"
  else if(x=="e")
    "20" 
  else if(x=="f")
      "22"
  else if(x=="g")
    "24"
  else if(x=="h")
    "4"
  else if(x=="i")
    "6"
  else if(x=="j")
    "IBYC"
  else if(x=="k")
    "NELSON"
  else if(x=="l")
    "Ocean"
  else
    "0"
}

print(myfunction("a"))
print(myfunction("b"))


# Custom operator(s) (10 points)

'%Nonso%'<-function(x,y){3^x + 6*y}
2%Nonso%3



#Reshaping data with ‘melt’ and/or ‘dcast’ (5 points)

melt<-melt(data=a,id.vars=c("BottleName","SubsampleName"),
            measure.vars=c("Salinity","Temp"))
melt


cast<-dcast(data=melt,formula=BottleName~variable,
             fun.aggregate=mean)
cast

#ddply (10 pts)
unique(a$BottleName)

d=a[a$BottleName=="IBYC",]

nd<- ddply(.data=a, .variables="BottleName",function(x){
  z<-unique(x$BottleName)
  #ifelse(test=z=="Nelson",yes=30,no=15)
  BottleName_condition<-function(y){
    if(z=="a")
      "Nelson"
    else if(z=="b")
      "12"
    else if(z=="c")
      "16"
    else if(z=="d")
      "18"
    else if(z=="e")
      "20" 
    else if(z=="f")
      "22"
    else if(z=="g")
      "24"
    else if(z=="h")
      "4"
    else if(z=="i")
      "6"
    else if(z=="j")
      "IBYC"
    else if(z=="k")
      "NELSON"
    else if(z=="l")
      "Ocean"
    else
      "0"
  }
  
  x$BottleName_z<-BottleName_condition(y=z)
  return(x)
}, .inform=T,.progress = "text")




#‘for loop’ (10 points)
#creating for loop with r and c to iterate over the data frame(a)

for (r in 1:nrow(a))
  for (c in 1:ncol(a))
    print(paste("Row",r, "and column",c, "have values of", a[r,c] ))
print(paste("Row",r, "and column",c, "have values of", a[1,2] ))

##Histogram (5 points)
hist<-ggplot(data = a, aes(x = Salinity)) +
  geom_histogram(binwidth = 1, color="black", fill="green")+
  facet_wrap(.~BottleName)
hist


##Point, bar, or line plot (whichever makes the most sense) (5 points)
point<-ggplot(data = a, aes(x = Salinity, y=BottleName)) +
  geom_point(color="orange")
point

bargraph<-ggplot(data = a, aes(x=Salinity)) +
  geom_bar(color="Orange")+
  facet_grid(BottleName~.)
bargraph

##‘ggplot’ with at least 2 geoms (e.g. point, bar, tile), use one of the ‘scale_’ geoms,
#and adjusting the theme of the plot (10 points)
ggplot(data=a, aes(x=Salinity,y=BottleName)) +
  geom_point(color="blue") +geom_tile()


#A map of showing the geographic location where the data was collected (10 points)
install.packages("ggmap")
install.packages("osmdata")
library(ggmap)
library(osmdata)

# massachusetts map
getbb('massachusetts')
map.massachusetts=get_stamenmap(bbox=getbb('massachusetts'),
                         zoom=8,map='terrain')
ggmap(map.massachusetts)



#Exporting data set (2 points)
getwd()
dir.create("C:/Users/anons/OneDrive/Desktop/R programming/Grad_final_R_project")

#Exporting and saving figures from ggplot (2 points)
ggsave(".tiff")
