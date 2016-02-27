##Homework5###
#1
#a
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
d <- ggplot(diamonds,aes(x*y*z, y=price,na.rm=TRUE))
d+scale_x_log10(diamonds$x)+scale_y_log10(diamonds$y)+ geom_point(aes(colour = clarity, size = carat),alpha=0.5)
#b
ggplot(diamonds,aes(x=carat,y=..density..,colour=clarity,size=carat,na.rm=TRUE))+geom_histogram(binwidth =0.2)+facet_grid(cut~.)
#c
d1<-ggplot(diamonds,aes(x=cut,y=price,na.rm=TRUE,alpha=0.01))
d1+geom_jitter()+geom_violin()

#3
#a
library("foreign", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
df.dta<-read.dta(file="/Users/jingchiyan/Downloads/org_example-5.dta")
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
df.dta.2<-df.dta %>%
group_by(year,month)%>%
summarise(
    quarter1_d = quantile(rw, .25, na.rm = T,type=7),
    quarter3_d = quantile(rw, .75, na.rm = T,type=7),
    deciles1_d = quantile(rw, .1, na.rm = T,type=5),
    deciles3_d = quantile(rw, .9, na.rm = T,type=5),
    median_d = median(rw, na.rm = T)
) %>%
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
)  
d2<-ggplot(data=df.dta.2,aes(x=date))
d2+geom_line(aes(y=median_d))+ geom_ribbon(aes(ymin = quarter1_d, ymax = quarter3_d), fill = "grey50",
  alpha=0.5
)+geom_ribbon(
  aes(ymin = deciles1_d, ymax = deciles3_d), fill = "grey80",
  alpha=0.4)+ylim(0,50)
#b
df.dta.3<-df.dta %>%
group_by(year,month,educ) %>%
summarise(median_d = median(rw, na.rm = T)
) %>%
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
)
d3 <- ggplot(data=df.dta.3,aes(x=date,color=educ))
d3+geom_line(aes(y = median_d))+ylim(5,35)



