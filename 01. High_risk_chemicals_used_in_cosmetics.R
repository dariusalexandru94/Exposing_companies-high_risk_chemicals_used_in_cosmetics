library('moments')
library('plotrix')
library('BSDA')
library('ggplot2')
library("tidyverse")
library ("dplyr")

mydata = read.csv("cscpopendata.csv", header = TRUE) #uploading csv file
view(mydata)
nrow(mydata) # number of records
unique_chemicals = unique(mydata$ChemicalName) #123 unique chemicals

#"Formaldehyde", "Lead" , "Arsenic" , "Benzene", "Isopropyl" , "Dibutyl", "Benzophenone", 
#"Mercury", "Polygeenan"  

### replace 
mydata[mydata == "Formaldehyde (gas)"] <- "Formaldehyde"
mydata[mydata == "Methyl chloride"] <- "Formaldehyde"
mydata[mydata == "Formaldehyde solution"] <- "Formaldehyde"
mydata[mydata == "Methylene glycol"] <- "Formaldehyde"
mydata[mydata == "Mercury and mercury compounds"] <- "Mercury"
mydata[mydata == "Di-n-butyl phthalate (DBP)"] <- "Dibutyl"
mydata[mydata == "Propylene oxide"] <- "Isopropyl"
mydata[mydata == "Isopropyl alcohol manufacture using strong acids"] <- "Isopropyl"
mydata[mydata == "Arsenic (inorganic arsenic compounds)"] <- "Arsenic"
mydata[mydata == "Arsenic (inorganic oxides)"] <- "Arsenic"
mydata[mydata == "Lead acetate"] <- "Lead"

### replace 2
mydata[mydata == "Lead"] <- "HRC"
mydata[mydata == "Formaldehyde"] <- "HRC"
mydata[mydata == "Benzene"] <- "HRC"
mydata[mydata == "Benzophenone"] <- "HRC"
mydata[mydata == "Polygeenan"] <- "HRC"
mydata[mydata == "Arsenic"] <- "HRC"
mydata[mydata == "Isopropyl"] <- "HRC"
mydata[mydata == "Dibutyl"] <- "HRC"
mydata[mydata == "Mercury"] <- "HRC"

####################################### HRC = HIGH RISK CHEMICAL
#######################################
#######################################

"HRC" %in% mydata$ChemicalName
nrow(filter(mydata,ChemicalName %in% c("HRC")))
### 534 products out of 114635 -> containing high risk chemicals for humans

chemicals = data.frame(sort(table(mydata$ChemicalName), decreasing = TRUE))
View(chemicals)

### replace 3
mdata = mydata
mdata['ChemicalName'][mdata['ChemicalName'] != "HRC"] <- "non - risk"
View(mdata)

dif = data.frame(table(mdata$ChemicalName))

pie <- ggplot(dif, aes(x = "", y=Freq, fill = factor(Var1))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="",                       
       x=NULL, 
       y=NULL, 
       title="0.46% of cosmetics contain HRC")
pie + coord_polar(theta = "y", start=0)

#########

categ = subset(mydata, ChemicalName == "HRC")
view(categ)
unique(categ$CompanyName) # atentie la asta
paste0(unique(categ$CompanyName), collapse=", ")
categ_df = data.frame(sort(table(categ$PrimaryCategory), decreasing = TRUE))
View(categ_df)
ggplot(head(categ_df), aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL,                                         
       y=NULL, 
       title="HRC - cosmetics category")

######### 

categ2 = subset(mydata, ChemicalName != "HRC")
categ2_df = data.frame(sort(table(categ2$PrimaryCategory), decreasing = TRUE))
View(categ2_df)
ggplot(head(categ2_df), aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL,                                         
       y=NULL, 
       title="non risk - cosmetics category")

########## 

comp = subset(mydata, ChemicalName == "HRC")
comp_df = data.frame(sort(table(categ$CompanyName), decreasing = TRUE))
View(comp_df)
ggplot(head(comp_df, 5), aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL,                                         
       y=NULL, 
       title="Top 5 Companies (| number of products) that use high risk chemicals in cosmetics")

nrow(mydata)
unique(mydata$CompanyName)
compp = subset(mydata, ChemicalName == "HRC")
nrow(compp)
unique(compp$CompanyName)
comp_df = data.frame(sort(table(compp$SubCategory), decreasing = TRUE))
View(comp_df)
ggplot(head(comp_df, 5), aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL,                                         
       y=NULL, 
       title="HRC")

##########################################################################################################
FORMALDEHYDE = subset(data_frame, ChemicalName == "Formaldehyde")
nrow(FORMALDEHYDE)

Fcat_df = data.frame(sort(table(FORMALDEHYDE$CompanyName), decreasing = TRUE))
view(Fcat_df)
pie <- ggplot(head(Fcat_df), aes(x = "", y=Freq, fill = factor(Var1))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Companies",                       
       x=NULL, 
       y=NULL, 
       title="Formaldehyde - companies") +
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0.5))
pie + coord_polar(theta = "y", start=0)

Fsub_df = data.frame(sort(table(FORMALDEHYDE$SubCategory), decreasing = TRUE))
View(Fsub_df)
ggplot(head(Fsub_df), aes(x = Var1, y = Freq, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL,                                         
       y=NULL, 
       title="Formaldehyde - product subcategories")

Fcom_df = data.frame(sort(table(FORMALDEHYDE$CompanyName), decreasing = TRUE))
View(Fcom_df)

ggplot(Fcom_df, aes(x = Var1, y = Freq, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL, 
       y=NULL,                                                      
       title="Formaldehyde - companies")






##########################################################################################################
MERCURY = subset(data_frame, ChemicalName == "Mercury")
nrow(MERCURY)

Mcat_df = data.frame(sort(table(MERCURY$PrimaryCategory), decreasing = TRUE))
view(Mcat_df)
pie <- ggplot(Mcat_df, aes(x = "", y=Freq, fill = factor(Var1))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Categories",                       
       x=NULL, 
       y=NULL, 
       title="Mercury - product category") + 
  geom_col(color = "black", size = 0.5) + 
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0.5))
pie + coord_polar(theta = "y", start=0)

Msub_df = data.frame(sort(table(MERCURY$SubCategory), decreasing = TRUE))
View(Msub_df)
ggplot(Msub_df, aes(x = Var1, y = Freq, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL,                                         
       y=NULL, 
       title="Mercury - product subcategories")

Mcom_df = data.frame(sort(table(MERCURY$CompanyName), decreasing = TRUE))
View(Mcom_df)

ggplot(Mcom_df, aes(x = Var1, y = Freq, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL, 
       y=NULL,                                                      
       title="MERCURY - companies")



########################################################################################
DIBUTYL = subset(data_frame, ChemicalName == "Dibutyl")
nrow(DIBUTYL)

Dcat_df = table(DIBUTYL$PrimaryCategory)
view(Dcat_df)
pie <- ggplot(data.frame(Dcat_df), aes(x = "", y = Freq, fill = factor(Var1))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Categories",                       
       x=NULL, 
       y=NULL, 
       title="Dibutyl - product category") + 
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0))
pie + coord_polar(theta = "x", start=0)

Dsub_df = data.frame(sort(table(DIBUTYL$SubCategory), decreasing = TRUE))
View(Dsub_df)
ggplot(Dsub_df, aes(x = Var1, y = Freq, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL,                                         
       y=NULL, 
       title="Dibutyl - product subcategories")

Dcom_df = data.frame(sort(table(DIBUTYL$CompanyName), decreasing = TRUE))
View(Dcom_df)

ggplot(Dcom_df, aes(x = Var1, y = Freq, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL, 
       y=NULL,                                                      
       title="Dibutyl - companies")


##########################################################################################################
ISOPROPYL = subset(data_frame, ChemicalName == "Isopropyl")
nrow(ISOPROPYL)

Icat_df = data.frame(sort(table(ISOPROPYL$CompanyName), decreasing = TRUE))
view(Icat_df)
pie <- ggplot(Icat_df, aes(x = "", y=Freq, fill = factor(Var1))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Companies",                       
       x=NULL, 
       y=NULL, 
       title="Isopropyl - companies") + 
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0.5))
pie + coord_polar(theta = "y", start=0)

Isub_df = data.frame(sort(table(ISOPROPYL$SubCategory), decreasing = TRUE))
View(Isub_df)
ggplot(head(Isub_df), aes(x = Var1, y = Freq, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL,                                         
       y=NULL, 
       title="Isopropyl - product subcategories")

Icom_df = data.frame(sort(table(ISOPROPYL$CompanyName), decreasing = TRUE))
View(Icom_df)

ggplot(Icom_df, aes(x = Var1, y = Freq, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL, 
       y=NULL,                                                      
       title="Isopropyl - companies")

##########################################################################################################
ARSENIC = subset(data_frame, ChemicalName == "Arsenic")
nrow(ARSENIC)

ARcat_df = data.frame(sort(table(ARSENIC$PrimaryCategory), decreasing = TRUE))
view(Icat_df)
pie <- ggplot(ARcat_df, aes(x = "", y=Freq, fill = factor(Var1))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Categories",                       
       x=NULL, 
       y=NULL, 
       title="Arsenic - product category") + 
  geom_col(color = "black", size = 0.5) + 
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0.5))
pie + coord_polar(theta = "y", start=0)

ARsub_df = data.frame(sort(table(ARSENIC$SubCategory), decreasing = TRUE))
View(ARsub_df)
ggplot(ARsub_df, aes(x = Var1, y = Freq, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL,                                         
       y=NULL, 
       title="Arsenic - product subcategories")

ARcom_df = data.frame(sort(table(ARSENIC$CompanyName), decreasing = TRUE))
View(ARcom_df)

ggplot(ARcom_df, aes(x = Var1, y = Freq, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL, 
       y=NULL,                                                      
       title="Arsenic - companies")
##########################################################################################################
LEAD = subset(data_frame, ChemicalName == "Lead")
nrow(LEAD)

Lcat_df = data.frame(sort(table(LEAD$PrimaryCategory), decreasing = TRUE))
view(Lcat_df)
pie <- ggplot(Lcat_df, aes(x = "", y=Freq, fill = factor(Var1))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Categories",                       
       x=NULL, 
       y=NULL, 
       title="Lead - product category") + 
  geom_col(color = "black", size = 0.5) + 
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0.5))
pie + coord_polar(theta = "y", start=0)

Lsub_df = data.frame(sort(table(LEAD$SubCategory), decreasing = TRUE))
View(Lsub_df)
ggplot(Lsub_df, aes(x = Var1, y = Freq, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL,                                         
       y=NULL, 
       title="Lead - product subcategories")

Lcom_df = data.frame(sort(table(LEAD$CompanyName), decreasing = TRUE))
View(Lcom_df)

ggplot(Lcom_df, aes(x = Var1, y = Freq, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL, 
       y=NULL,                                                      
       title="Lead - companies")

##########################################################################################################
BENZENE = subset(data_frame, ChemicalName == "Benzene")
nrow(BENZENE)

Bcat_df = table(BENZENE$PrimaryCategory)
view(Bcat_df)
pie <- ggplot(data.frame(Bcat_df), aes(x = "", y = Freq, fill = factor(Var1))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Categories",                       
       x=NULL, 
       y=NULL, 
       title="Benzene - product category") + 
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0))
pie + coord_polar(theta = "x", start=0)

Bsub_df = data.frame(sort(table(BENZENE$SubCategory), decreasing = TRUE))
View(Bsub_df)
ggplot(Bsub_df, aes(x = Var1, y = Freq, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL,                                         
       y=NULL, 
       title="Benzene - product subcategories")

Bcom_df = table(BENZENE$CompanyName)
View(Bcom_df)

ggplot(data.frame(Bcom_df), aes(x = Var1, y = Freq, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL, 
       y=NULL,                                                      
       title="Benzene - companies") ####################### atentie aici


unique(data_frame$ChemicalName)



##########################################################################################################
BENZOPHENONE = subset(data_frame, ChemicalName == "Benzophenone")
nrow(BENZOPHENONE)

BEcat_df = data.frame(sort(table(BENZOPHENONE$CompanyName), decreasing = TRUE))
view(BEcat_df)
pie <- ggplot(head(BEcat_df, 5), aes(x = "", y=Freq, fill = factor(Var1))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Companies",                       
       x=NULL, 
       y=NULL, 
       title="Benzophenone - companies") + 
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0.5))
pie + coord_polar(theta = "y", start=0)


BEsub_df = data.frame(sort(table(BENZOPHENONE$SubCategory), decreasing = TRUE))
View(Bsub_df)
ggplot(head(BEsub_df), aes(x = Var1, y = Freq, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL,                                         
       y=NULL, 
       title="Benzophenone - product subcategories")

BEcom_df = data.frame(sort(table(BENZOPHENONE$CompanyName), decreasing = TRUE))
View(BEcom_df)

ggplot(BEcom_df, aes(x = Var1, y = Freq, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL, 
       y=NULL,                                                      
       title="Benzophenon - companies")



unique(data_frame$ChemicalName)

#########################################################################################################
POLYGEENAN = subset(data_frame, ChemicalName == "Polygeenan")
nrow(POLYGEENAN)

Pcat_df = table(POLYGEENAN$PrimaryCategory)
view(BEcat_df)
pie <- ggplot(data.frame(Pcat_df), aes(x = "", y=Freq, fill = factor(Var1))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Categories",                       
       x=NULL, 
       y=NULL, 
       title="Polygeenan - product category") + 
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0))
pie + coord_polar(theta = "x", start=0)


Psub_df = data.frame(sort(table(POLYGEENAN$SubCategory), decreasing = TRUE))
View(Psub_df)
ggplot(Psub_df, aes(x = Var1, y = Freq, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL,                                         
       y=NULL, 
       title="Polygeenan - product subcategories")

Pcom_df = table(POLYGEENAN$CompanyName)
View(Pcom_df)

ggplot(data.frame(Pcom_df), aes(x = Var1, y = Freq, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL, 
       y=NULL,                                                      
       title="Polygeenan - companies")



c(unique(data_frame$ChemicalName))

########################################
###########################################
########################################
#####################################
pie <- ggplot(new_df, aes(x = "", y=Freq, fill = factor(Var1))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Chemical",                       
       x=NULL, 
       y=NULL, 
       title="Proportion of HRC in cosmetics")

pie + coord_polar(theta = "y", start=0)

################

g <- ggplot(new_df, aes(Var1, Freq))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Most harmful chemicals occurencies") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#################
table(data_frame$CompanyName)
companies = data.frame(table(data_frame$CompanyName))
View(companies)
sort(companies$Var1, decreasing = TRUE)
desc(companies)
companies
top_c = data.frame(sort(table(data_frame$CompanyName), decreasing = TRUE))
View(top_c)

View(data_frame)

##################################################################
########################################## 
##################################################################
########################################## LA PRAIRE LA PRAIRE LA PRAIRE
LaPrairie = subset(data_frame, CompanyName == 'La Prairie, Inc.')
View(LaPrairie) 
nrow(LaPrairie)
### atentie la benzophenon aici
L_categories = data.frame(sort(table(LaPrairie$PrimaryCategory), decreasing = TRUE))
L_categories


ggplot(LaPrairie) + geom_bar(aes(x = LaPrairie$PrimaryCategory)) + 
  labs(fill="",                                                                  
       x=NULL, 
       y=NULL, 
       title="LaPrairie - categories of human harmful products") + theme_minimal()
 

ggplot(L_categories, aes(x = Var1, y = Freq, fill = Var1)) +  
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL, 
       y=NULL,                                                              
       title="LaPrairie - categories")


LaPrairie
L_chemi = data.frame(table(LaPrairie$ChemicalName))

##########################

pie <- ggplot(L_chemi, aes(x = "", y=Freq, fill = factor(Var1))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Chemicals",                       
       y=NULL, 
       title="LaPrairie - harmful chemicals used by LaPrairie")
pie + coord_polar(theta = "y", start=0)


L_subcat = data.frame(sort(table(LaPrairie$SubCategory), decreasing = TRUE))
L_subcat


ggplot(L_subcat, aes(x = Var1, y = Freq, fill = Var1)) +  
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL,                                        
       y=NULL, 
       title="LaPrairie - subcategories")

########################
######################################
########################################## American International Industries

american = subset(data_frame, CompanyName == 'American International Industries')
View(american) 
nrow(american)

A_categories = data.frame(sort(table(american$PrimaryCategory), decreasing = TRUE))
A_categories

ggplot(A_categories, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL, 
       y=NULL,                                                                
       title="American International Industries - categories")

A_chemi = data.frame(table(american$ChemicalName))

##########################

pie <- ggplot(A_chemi, aes(x = "", y=Freq, fill = factor(Var1))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Chemicals",                      
       x=NULL, 
       y=NULL, 
       title="American International Industries - harmful chemicals used")
pie + coord_polar(theta = "y", start=0)

A_subcat = data.frame(sort(table(american$SubCategory), decreasing = TRUE))
View(A_subcat)
ggplot(A_subcat, aes(x = Var1, y = Freq, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  labs(fill="", 
       x=NULL,                                         #################################ADJUDECAT PENTRU MULTE
       y=NULL, 
       title="American International Industries - subcategories")
