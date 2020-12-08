#####################################################
# SCRIPT TO ANALYZE DATA FROM A PUP RETRIEVAL TASK  #
# DURING OPTOGENETIC STIMULATION OF BRAIN CELLS     #
# BY SOFIA GONZALEZ-SALINAS, SHUMYATSKY LAB         #
# sophieglzs@gmail.com                              #
# RUTGERS UNIVERSITY, 2020                          #
#####################################################

# Get familiar with the syntax of the folder of you operative system
getwd()

#Set the directory where you stored the file to read
setwd("C:/Users/Shumyatsky-lab/Desktop/R_Ladies")
#List all the files to verify that you have the file to read
list.files()
#Read the csv file
opto<-read.csv("MatCare_Data.csv", header=T)
#We make a summary to verify the properties of the data we are reading
summary(opto)
#Convert to factors the independent variables
opto$MouseID<-as.factor(opto$MouseID)
opto$Virus<-as.factor(opto$Virus)
opto$Trial<-as.factor(opto$Trial)
opto$Laser<-as.factor(opto$Laser)

#We don't need to convert the column "Retrieved" as we will use the numbers 
#to calculate a % later

#We verify that the variables were updated
summary(opto)
#Make sure you have the following packages installed
#install.packages("plyr")
#install.packages("ggplot2")
#install.packages("ggpubr")
#install.packages("nlme") 
#install.packages("pwr") 

library("plyr") #To use ddply()
library("ggplot2") # To use ggpaired()
library("ggpubr") # To use ggpaired()
library("nlme") # To use lme()
library("car")# To use Anova()
library("emmeans")# To use emmeans()
library("pwr")# To use pwr.t.test()


#We create a summary table calculating the average latency across trials
ave_mouse<-ddply(opto, c("Virus", "Laser","MouseID"),summarize,
          Mean_Contact_s=mean(Latency_Contact_s),Mean_Ret_s=mean(Latency_Retrieve_s),
          Num_Trials=length(Retrieved), Sum_Ret=sum(Retrieved),
          Perc_ret=((sum(Retrieved)/length(Retrieved))*100))

#We visualize the summary table
ave_mouse


## We first plot the Latency to contact the pup
F1<-ggpaired(ave_mouse, x="Laser", y="Mean_Contact_s", color="Virus",
         palette=c("#009966","#FF0099"), line.color = "darkgray", id="MouseID",
         line.size = 0.4,ylab="Latency to \napproach pup (s)",xlab="Laser",
         facet.by="Virus", width=0.4, short.panel.labs = TRUE)+ 
  theme(legend.position="none",strip.background = element_rect(colour="black", fill="white"))+
  scale_y_continuous(limits=c(0, 120), breaks=seq(0, 120, 20))

#####We now plot the latency to retrieve pups to the nest
F2<-ggpaired(ave_mouse, x="Laser", y="Mean_Ret_s", color="Virus",
         palette=c("#009966","#FF0099"), line.color = "darkgray", id="MouseID",
         line.size = 0.4,ylab="Latency to \nretrieve pup (s)",xlab="Laser",
         facet.by="Virus", width=0.4,short.panel.labs = TRUE)+ 
  theme(legend.position="none",strip.background = element_rect(colour="black", fill="white"))+
  scale_y_continuous(limits=c(0, 120), breaks=seq(0, 120, 20))

#Plot the percentage of pups retrieved and save them in a variable named F3

F3<-ggpaired(ave_mouse, x="Laser", y="Perc_ret", color="Virus",
             palette=c("#009966","#FF0099"), line.color = "darkgray", id="MouseID",
             line.size = 0.4,ylab="% Trials retrieved",xlab="Laser",
             facet.by="Virus", width=0.4,short.panel.labs = TRUE)+ 
  theme(legend.position="none",strip.background = element_rect(colour="black", fill="white"))+ 
  scale_y_continuous(limits=c(0, 100),breaks=seq(0, 100, 20))


#We create a figure with three panels

Opto_Fig<-ggarrange(F1,F2, F3,nrow=1, ncol=3)

ggsave("Opto_MatCare_Boxplot.jpeg", Opto_Fig,width = 9, height = 3, dpi=300)
#The size is by default in inches, you can change the units by the 
# parameter units="cm"
#You can change the format of the picture just by changing the extension
#.jpeg by .tiff , . png, or .pdf


## OTHER TYPE OF PLOTS

A1<-ggline(ave_mouse, x="Laser", y="Mean_Contact_s", color="Virus",font.x=10,font.y=10,
           font.tickslab=9,
           add=c("mean_se","dotplot"), ylim=c(0,120), palette=c("#009966","#FF0099"),
           ylab="Latency to \napproach pup (s)")+ 
  theme(legend.position="none")+  scale_y_continuous(limits=c(0, 120), breaks=seq(0, 120, 20))


A2<-ggline(ave_mouse, x="Laser", y="Mean_Ret_s", color="Virus", font.x=10,font.y=10,
           font.tickslab=9,
           add=c("mean_se","dotplot"), ylim=c(0,120), palette=c("#009966","#FF0099"),
           ylab="Latency to \nretrieve pup (s)")+ 
  theme(legend.position="none")+scale_y_continuous(limits=c(0, 120), breaks=seq(0, 120, 20))


A3<-ggline(ave_mouse, x="Laser", y="Perc_ret", color="Virus", font.x=10,font.y=10,
           font.tickslab=9,font.legend=6,
           add=c("mean_se","dotplot"), ylim=c(0,100), palette=c("#009966","#FF0099"),
           ylab="% of Trials retrieved")+ 
  theme(legend.position="top",legend.title=element_blank(),legend.background=element_rect( size = 0.3, colour = 1))+
  scale_y_continuous(limits=c(0, 100),breaks=seq(0, 100, 20))



A<-ggarrange(A1,A2,A3, nrow=1, ncol=3)
ggsave("Opto_MatCare_Mean.jpeg", A,width = 6, height = 3, dpi=400)



##LET'S FOCUS ON THE STATISTICAL ANALYSIS
#########
#Latency to contact with the "conventional" ANOVA
summary(aov(Mean_Contact_s ~ (Virus*Laser) + Error(MouseID/Laser), data = ave_mouse))
#In order to study the paired comparison for the interaction of factors we need 
# to build a lineal model that is a generalization of the "ANOVA"
model_lme1 <- lme(Mean_Contact_s~Virus*Laser, random=~1|MouseID/(Laser), data=ave_mouse)

#We visualize the data in a "ANOVA" format
Anova(model_lme1, type=("III"))
#As the interaction of the factors was not significant it is not appropriate to compare all the pairs

#####
## Analyze the latency to retrieve the pup
summary(aov(Mean_Ret_s ~ (Virus*Laser) + Error(MouseID/Laser), data = ave_mouse))
model_lme2 <- lme(Mean_Ret_s~Virus*Laser, random=~1|MouseID/(Laser), data=ave_mouse)
Anova(model_lme2, type=("III"))
#As the interaction was significant we want to know the p-value for all the paired comparisons
# By deafault the comparisons are corrected by the Tukey method
emmeans(model_lme2, pairwise~Virus*Laser)
# We can change the correction method to Bonferroni
emmeans(model_lme2, pairwise~Virus*Laser, adjust = "bonf")

######
## Analyze the percentage of pups retrieved
summary(aov(Perc_ret ~ (Virus*Laser) + Error(MouseID/Laser), data = ave_mouse))
model_lme3 <- lme(Perc_ret~Virus*Laser, random=~1|MouseID/(Laser), data=ave_mouse)

Anova(model_lme3, type=("III"))

#As the interaction was significant we want to know the p-value for all the paired comparisons
# By deafault the comparisons are corrected by the Tukey method
emmeans(model_lme3, pairwise~Virus*Laser)


##We calculate the sample size from our data
# we will use the data when the laser was ON comparing the groups injected with
# the different viruses

mean1<-mean(ave_mouse$Mean_Ret_s[ave_mouse$Virus=="mCherry" & ave_mouse$Laser=="ON"])
mean2<-mean(ave_mouse$Mean_Ret_s[ave_mouse$Virus=="ChR2" & ave_mouse$Laser=="ON"])

mean1#mCherry ON
mean2#ChR2 ON
var1<-var(ave_mouse$Mean_Ret_s[ave_mouse$Virus=="mCherry" & ave_mouse$Laser=="ON"])
var2<-var(ave_mouse$Mean_Ret_s[ave_mouse$Virus=="ChR2" & ave_mouse$Laser=="ON"])

sd1_2<-sqrt((var1+var2)/2)

d_Cohen<-(mean1-mean2)/sd1_2

#For a t-test with unpaired samples
# In type you can change to "paired" if you have repeated measures in those two groups
pwr.t.test(d= d_Cohen, sig.level =0.05 , power = 0.8, type = c("two.sample"))

###########################################################
#EXERCISE: Compare Laser ON vs Laser OFF in the ChR2 group to get the required sample size
##########################################################
