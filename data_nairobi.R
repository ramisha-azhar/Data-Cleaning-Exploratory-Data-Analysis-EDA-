rm(list=ls())

### Import and check a dataset ----
?read.csv

### Every dataset must be checked and cleaned ----
# we use read.csv2(), which is designed for European‑style CSVs
# where we use (semicolon ; as separator and comma , as decimal)
'''
ID;Sesso;ETA;MEDICAL_CAMP;Malattie_sist_no_malaria;MALARIA;behaviour;NEEMA_ALREADY;SPAZZOLINO;N_SPAZZOLA_GG;FRUTTA;DOLCI;SUCCHI;DENTISTA_ANNO;Igiene orale;Indice parodontale;DD;MM;FF;d;m;f;n_denti;IPOPLASIA;FLUOROSI;MIH_DECIDUI;MIH;Traumi;Classe MOLARE;OVERBITE;OVERJET;TIPO_MORSO;CROSSBITE;RESPIRAZIONE;ABITUDINI_VIZ;FRENULI
1;F;8;Korogocho;;NO;4;SI;SI;1;NO;NO;NO;1;2;2;0;0;0;0;0;0;24;NO;4;0;0;solo smalto;inf avanzato;2;aumentato;aperto;nessuno;nasale;deglutizione atipica;normale
2;F;9;Korogocho;;SI;4;NO;SI;2;SI;NO;NO;0;2;2;2;0;0;5;1;0;24;NO;3;0;0;no;neutra;2;diminuito;aperto;nessuno;nasale;deglutizione atipica;normale
'''
data_nairobi_=read.csv2("data_teeth.csv",na.strings="") #convert empty string "" into NA
str(data_nairobi_)
head(data_nairobi_)# it is the first six rows of the data frame
dim(data_nairobi_)  #[1] 374  36
colnames((data_nairobi_))

#count missing values in the columns of the data frame
colSums(is.na(data_nairobi_))

#CSV uses comma for separation and dot for decimal
#it is a US/UK format,it is a Standard comma‑separated files
'''
ID,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
1,F,8,Korogocho,,NO,4,SI,SI,1,NO,NO,NO,1,2,2,0,0,0,0,0,0,24,NO,4,0,0,solo smalto,inf avanzato,2,aumentato ,aperto,nessuno,nasale,deglutizione atipica,normale
2,F,9,Korogocho,,SI,4,NO,SI,2,SI,NO,NO,0,2,2,2,0,0,5,1,0,24,NO,3,0,0,no,neutra,2,diminuito,aperto,nessuno,nasale,deglutizione atipica,normale
'''
data_nairobi=read.csv("data_teeth_dirty.csv",
                      na.strings="") 

# Apply the unique() function to every column in our data 
'''
here before adding elements in the argument na.strings we can see all the unique values e.g ??? 
and put it in to the na.strings vector to convert it into the NA values for our analysis
'''
unique_vals <- lapply(data_nairobi, unique)

# Print the results
print(unique_vals)

data_nairobi=read.csv("data_teeth_dirty.csv",
                      na.strings=c("","???"),
                      row.names=1) #use the first column as row names

#this is get the column names from the other data set and put it is this one
names_source <- colnames(data_nairobi_)
names_source <- names_source[-1]
colnames(data_nairobi) <- names_source

### Data cleaning and manipulation ----
# this is for the adjustment of the strings
# Convert variable names in lowercase
names(data_nairobi)
names(data_nairobi)=tolower(names(data_nairobi))
names(data_nairobi)

# Change some variable names
names(data_nairobi)
names(data_nairobi)[14]="oral_hygiene" #we are extracting the column name and reassign to our desire one 
names(data_nairobi)[15]="periodontal_index"

###we use grep function it has two arguments the first one is string " " 
#it gives us the position of an element 
#grep(pattern, x) Returns numeric positions of elements in x that match the pattern.
grep("_",names(data_nairobi)) #[1]  3  4  7  9 13 14 15 22 25 31 34 #the column name which contain an underscore

#Returns a logical vector (TRUE/FALSE)
grepl("_",names(data_nairobi))  

#it grab all the column names
grep(".",names(data_nairobi))#dot is a special symbol so avoid it as the column name 

#as we say dot is a special symbol so we need extra argument for meaningful interpretation
grep(".",names(data_nairobi),fixed=TRUE) #[1] 28
names(data_nairobi)[grep(".",names(data_nairobi),fixed=TRUE)] #we extracted the label with the help of indexing  "classe.molare"
names(data_nairobi)[grep(".",names(data_nairobi),fixed=TRUE)]="classe_molare"

dim(data_nairobi) #[1] 374  35
colnames(data_nairobi) #no column name ID anymore 
View(data_nairobi)
head(data_nairobi)


### First descriptive summaries(Descriptive summaries) ----
'''
Instead of staring at thousands of individual rows of numbers or text, 
a descriptive summary crunches all that data down into a few core numbers or categories
that tell us the "big picture" story of your dataset.
'''
# Useful command to have basic descriptive statistics and a general overview
library(Hmisc) #in the library we have the contributed packages 
describe(data_nairobi) #it gives us the count of the missing values in the variable(column name)
summary(data_nairobi) #show NA values just for numeric variable not character variable or else
str(data_nairobi)
attributes(data_nairobi)

### Clean eta (age) variable
head(data_nairobi$eta,10) #it is display as character vector because of dirty entries
str(data_nairobi$eta)

#unique show values in the random order so it is good idea to use sort function as well
sort(unique(data_nairobi$eta))
unique(data_nairobi$eta) #it shows NA values
sort(data_nairobi$eta) #it does not show NA value 
# Sort including NAs at the end with additional argument
sort(data_nairobi$eta,na.last = TRUE)

#Cleaning "8c" using gsub()
#This removes the "c" from "8c" → becomes "8"
gsub("c","",data_nairobi$eta) #we delete the c with empty string 
sort(unique(gsub("c","",data_nairobi$eta)),na.last = TRUE)
data_nairobi$eta=gsub("c","",data_nairobi$eta)

#Detecting the wrong value "133"
data_nairobi$eta==133
which(data_nairobi$eta==133) #return only the position ,which() ignore the NA entry [1] 84
data_nairobi$eta[which(data_nairobi$eta==133)]=13 #take the position with which()
sort(unique(data_nairobi$eta)) #it is still a character vector we need to change to numeric vector
data_nairobi$eta=as.numeric(data_nairobi$eta)
sort(unique(data_nairobi$eta),na.last = TRUE)
str(data_nairobi$eta)#now age is the numeric vector
summary(data_nairobi$eta) 
'''
 Min. 1st Qu.  Median    Mean 3rd Qu. 
  2.000   6.000   7.000   7.834   8.000 
   Max.    NAs 
17.000       1 
'''

### Clean n_spazzola_gg (number of brushing with toothbrush)

str(data_nairobi$n_spazzola_gg) 
unique(data_nairobi$n_spazzola_gg) #[1]  1  2  3 NA -2
sort(unique(data_nairobi$n_spazzola_gg),na.last = TRUE) #1] -2  1  2  3 NA
data_nairobi$n_spazzola_gg[which(data_nairobi$n_spazzola_gg==-2)]=2
sort(unique(data_nairobi$n_spazzola_gg),na.last = TRUE) #[1] 1 2 3
summary(data_nairobi$n_spazzola_gg)
'''
 Min. 1st Qu.  Median    Mean 3rd Qu. 
  1.000   1.000   2.000   1.846   2.000 
   Max.    NAs 
3.000      82
'''
### dentista_anno (number of visits to the dentist)

sort(unique(data_nairobi$dentista_anno),na.last = TRUE) #[1] 0 1

### Clean Overjet
#In dental data, Overjet refers to the horizontal overlap between the upper and lower front teeth (incisors).
#In the Nairobi children's oral health dataset, overjet is categorized into three levels:  
#Normal (normale): Standard upper-to-lower incisor relationship.
#Greater (aumentato): Increased/augmented overjet, where top front teeth stick out significantly ahead of bottom teeth.   
#Smaller (diminuito): Reduced overjet, where top front teeth have minimal or negative horizontal distance over bottom teeth
'''
there is the problem with the labels we have extra spaces
[1] "aumentato "  "aumentato  "
[3] "diminuito"   "normale"    
[5] "normale "
'''
unique(data_nairobi$overjet)
data_nairobi$overjet=gsub(" ","",data_nairobi$overjet)
unique(data_nairobi$overjet) #avoid duplicates
#[1] "aumentato" "diminuito" "normale" NA

### Clean Crossbite
#nessuno / NO: No crossbite present. 
#inverso / destro / sinistro / SI: Indicates the presence and direction of a crossbite
#Anterior / Reverse (inverso): Front top teeth fit behind front bottom teeth.
#Right (destro) or Left (sinistro): Unilateral posterior crossbite on the right or left side.
head(data_nairobi$crossbite,10)
str(data_nairobi$crossbite)
unique(data_nairobi$crossbite) #[1] "nessuno"  "inverso"  "destro"   "sinistro"
#replaces every comma with an underscore, making the variable clean and safe for analysis.
#In raw data entry, these are often recorded with commas separating the values, such as:"inverso, destro"
#Using gsub(",", "_", ...) transforms it into:,"inverso_destro"
data_nairobi$crossbite=gsub(",","_",data_nairobi$crossbite)

### Counting NAs ----
#this is the summary applied to the data frame the summary for each object is different
summary(data_nairobi)
summary(data_nairobi$eta) #for numeric variable we can see NA count with summary function
'''
 Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NAs 
2.000   6.000   7.000   7.834   8.000  17.000       1 
'''
#for character or categorical variable we can use table function to see count of NA values
table(data_nairobi$malaria)#the NA are overlooked only the absolute frequency of the unique values are shown
table(data_nairobi$malaria,useNA="always") #additional argument to see NA values
sort(colSums(is.na(data_nairobi)),decreasing=TRUE) #this give us the number of NA in each variable or column from variable with most NA's count to the lowest 

#first install the package
library(epiDisplay) 
#epiDisplay is an R package designed for epidemiology, public health, and medical statistics
#tab1() and tabpct() are the functions of epiDisplay
table(data_nairobi$sesso)
'''
  F   M 
209 165
'''

table(data_nairobi$sesso,useNA="always")
'''
  F    M <NA> 
 209  165    0 
'''


#tab1() is the more sophisticated version of the table() function,comes with the bar plot 

tab1(data_nairobi$sesso) #it gives bar plot too

#if we do not want graph 
tab1(data_nairobi$sesso,graph = FALSE) 


#because cum percent is only useful for the ordered categorical variable not nominal so we can remove it for nominal

tab1(data_nairobi$sesso, cum.percent = FALSE , bar.values = "percentage")

### Factor ---
str(data_nairobi$sesso ) #character vector
head(data_nairobi$sesso,25)
data_nairobi$sesso=factor(data_nairobi$sesso) #Converting to factor 
str(data_nairobi$sesso) #Factor w/ 2 levels "F","M": 1 1 1 2 2 1 2 2 1 2 ...
head(data_nairobi$sesso,25)
tab1(data_nairobi$sesso)
levels(data_nairobi$sesso)   #[1] "F" "M"
#changing the level names ,it is also apply to the labels, before my labels were F and M 
#but now along with the levels my label names also change into the Female and Male
#also level do not assign the new levels thay just change the name 
levels(data_nairobi$sesso)=c("Female","Male") #we change the labels of the level 
levels(data_nairobi$sesso)
head(data_nairobi$sesso,10)
str(data_nairobi$sesso) #Factor w/ 2 levels "Female","Male": 1 1 1 2 2 1 2 2 1 2 ...
nlevels(data_nairobi$sesso) #2
as.numeric(data_nairobi$sesso) #it gives us the integer values assign to each unique value of the variable

data_nairobi$sesso
data_nairobi$sesso=relevel(data_nairobi$sesso,ref="Male") #"Male" becomes the first level
str(data_nairobi$sesso) #Factor w/ 2 levels "Male","Female": 2 2 2 1 1 2 1 1 2 1 ..
data_nairobi$sesso
as.numeric(data_nairobi$sesso)

# Age (mostly 6-8 years old), only 1 missing value
summary(data_nairobi$eta)
'''
  Min. 1st Qu.  Median    Mean 3rd Qu. 
  2.000   6.000   7.000   7.834   8.000 
   Max.    NA 
   17.000  1 
'''

table(data_nairobi$eta)
barplot(table(data_nairobi$eta)) #simple bar plot for all the categories

prop.table(table(data_nairobi$eta))
plot(prop.table(table(data_nairobi$eta))) #This creates a bar plot of the relative frequencies called density plot

sum(prop.table(table(data_nairobi$eta))) #[1] 1
tab1(data_nairobi$eta)

#histogram is only for continuous variable like age 
hist(data_nairobi$eta)#it tells us absolute frequency for each class(class means the range of the rectangular)
hist(data_nairobi$eta,prob = TRUE)#it tells us relative frequency for each class
h_plot=hist(data_nairobi$eta)#the histogram is save as the object here
str(h_plot)
#without na.rm=TRUE
range(data_nairobi$eta)#[1] NA NA
#with na.rm=TRUE
range(data_nairobi$eta,na.rm=TRUE) #[1]  2 17
'''
the box plot is represented vertically
it is summary statistic applied to the numerical variable 
'''
boxplot(data_nairobi$eta)

# Age class
'''
Levels: (1.99,7] (7,12] (12,17]
these (] this is called class
'''
data_nairobi$eta_class1=cut(data_nairobi$eta, breaks=3)
data_nairobi$eta_class1
str(data_nairobi$eta_class1)
'''
 Factor w/ 3 levels "(1.99,7]","(7,12]",..: 2 2 2 2 2 2 2 2 2 2 ...
'''

data_nairobi$eta_class2=cut(data_nairobi$eta, breaks=c(2,5,11,17))# the classes depends on the vector length
data_nairobi$eta_class2
str(data_nairobi$eta_class2)
'''
Factor w/ 3 levels "(2,5]","(5,11]",..: 2 2 2 2 2 2 2 2 2 2 ...
'''

data_nairobi$eta_class3=cut(data_nairobi$eta, breaks=c(2,6,12,17),include.lowest=TRUE) #,include.lowest=TRUE the lower bound is included
data_nairobi$eta_class3
str(data_nairobi$eta_class3)
'''
 Factor w/ 3 levels "[2,6]","(6,12]",..: 2 2 2 2 2 2 2 2 2 2 ...
'''

data_nairobi$eta_class4=cut(data_nairobi$eta, breaks=c(2,6,12,17),include.lowest=TRUE,labels=c("0-6","<6-12","<12+"))
data_nairobi$eta_class4
str(data_nairobi$eta_class4)
'''
Factor w/ 3 levels "0-6","<6-12",..: 2 2 2 2 2 2 2 2 2 2 ...
'''

table(data_nairobi$eta_class3)
'''
 
  [2,6]  (6,12] (12,17] 
    122     222      29
these are the frequencies
we can see that two classificationes with the original level and change level are the same they have the same frequency
table(data_nairobi$eta_class2) = table(data_nairobi$eta_class3)
'''

table(data_nairobi$eta_class4)
'''
 0-6 <6-12  <12+ 
  122   222    29
'''

#it gives us the matrix it shows the cross tabulation
table(data_nairobi$eta_class3,data_nairobi$eta_class4)
'''
          0-6 <6-12 <12+
  [2,6]   122     0    0
  (6,12]    0   222    0
  (12,17]   0     0   29
we obtain the cross tabulation with the command table  
  
'''
###Abitudini Viziate
#abitudini_viz stands for "Abitudini Viziate" (Harmful Oral Habits or Vicious Oral Habits)
#"deglutizione atipica" (atypical swallowing / tongue thrust)
#"dito" (thumb sucking)
#"no" (no bad habits)
#"succhiotto_penne" (pacifier / pen biting)
#"succhiotto_penne_labbro" (pacifier / pen / lip biting)
str(data_nairobi$abitudini_viz ) #character vector
unique(data_nairobi$abitudini_viz)
'''
[1] "deglutizione atipica"   
[2] "no"                     
[3] "dito"                   
[4] NA                       
[5] "succhiotto_penne_labbro"
[6] "succhiotto_penne"
'''
data_nairobi$bad_oral_habits_bin=factor(data_nairobi$abitudini_viz)
unique(data_nairobi$bad_oral_habits_bin)
levels(data_nairobi$bad_oral_habits_bin)#they are alphabetic order
'''
[1] "deglutizione atipica"   
[2] "dito"                   
[3] "no"                     
[4] "succhiotto_penne"       
[5] "succhiotto_penne_labbro"
'''
table(data_nairobi$bad_oral_habits_bin,useNA="always")# this show the frequency table
#In clinical study reports (such as the study on Nairobi children), specific habits are often grouped into a binary indicator (Presence of harmful oral habits: No vs. Yes)
#Overwriting Levels with c(rep("Yes", 2), "No", rep("Yes", 2))
#The expression c(rep("Yes", 2), "No", rep("Yes", 2)) generates a vector of 5 labels: c("Yes", "Yes", "No", "Yes", "Yes").
levels(data_nairobi$bad_oral_habits_bin)=c(rep("Yes",2),"No",rep("Yes",2))
table(data_nairobi$bad_oral_habits_bin)
#No: 283 children have no harmful oral habits.   
#Yes: 90 children have at least one harmful oral habit
'''
Yes  No 
 90 283
'''

# equivalently data_nairobi$bad_oral_habits_bin=factor(data_nairobi$abitudini_viz,labels=c(rep("Yes",2),"No",rep("Yes",2)))
#This generates a cross-tabulation matrix (contingency table) showing exactly how each original category was mapped into the new binary categories
#the first variable is a row and second variable is column in the contigency table  of the table function
table(data_nairobi$abitudini_viz,data_nairobi$bad_oral_habits_bin)

# Let us change the order in the level codification
# a posterior(after creating the factor)
#data_nairobi$bad_oral_habits_bin2=relevel(data_nairobi$bad_oral_habits_bin,ref="No")
# a priori(during factor creation)
data_nairobi$bad_oral_habits_bin2=factor(data_nairobi$bad_oral_habits_bin,
                                         levels=c("No","Yes"))
#This confirms:
  
#The values are identical
# Only the order of levels changed
table(data_nairobi$bad_oral_habits_bin,data_nairobi$bad_oral_habits_bin2)

data_nairobi$bad_oral_habits_bin2 <- NULL

### Medical camp
str(data_nairobi$medical_camp)
unique(data_nairobi$medical_camp)#[1] "Korogocho"  "Dandora"    "Alice Home"
tab1(data_nairobi$medical_camp)
data_nairobi$medical_camp=factor(data_nairobi$medical_camp) #change character vector to the factors
str(data_nairobi$medical_camp) #Factor w/ 3 levels "Alice Home","Dandora",..: 3 3 3 3 3 3 3 3 3 3 ...
head(data_nairobi$medical_camp,10)

data_nairobi$medical_camp=factor(data_nairobi$medical_camp,
                                 levels=c("Korogocho","Dandora","Alice Home"))
str(data_nairobi$medical_camp)#Factor w/ 3 levels "Korogocho","Dandora",..: 1 1 1 1 1 1 1 1 1 1 ...



### Malattie_sist_no_malaria: deleted (too many NAs)

unique(data_nairobi$malattie_sist_no_malaria)
sum(is.na(data_nairobi$malattie_sist_no_malaria)) #371
data_nairobi$malattie_sist_no_malaria=NULL


### malaria: deleted (too many NAs)
unique(data_nairobi$malaria) #"NO" "SI" NA  
sum(is.na(data_nairobi$malaria)) #96
data_nairobi$malaria=NULL
dim(data_nairobi) #[1] 374  33

### behaviour: almost all are very collaborative (no variability)
#In clinical dental research, behaviour evaluates a child’s level of cooperation during dental screening or treatment 
head(data_nairobi$behaviour,10)
str(data_nairobi$behaviour)
unique(data_nairobi$behaviour)
tab1(data_nairobi$behaviour)


### neema_already:  deleted (too many NAs)
# it refers to whether a child was already enrolled in or previously visited the pediatric dental care program at Ruaraka Uhai Neema Hospital in Nairobi, Kenya
head(data_nairobi$neema_already,10)
unique(data_nairobi$neema_already) #"SI" "NO" NA 
sum(is.na(data_nairobi$neema_already)) #[1] 262
tab1(data_nairobi$neema_already)
data_nairobi$neema_already=NULL
dim(data_nairobi) #[1] 374  32

### Stratified statistics ----
# toothbrush: 3 out of 4 children use it, but percentage is different by camp
tab1(data_nairobi$spazzolino,
     bar.values = "percent",col=c("red","green"),
     main="Use of toothbrush")

tab1(data_nairobi$medical_camp)

table(data_nairobi$medical_camp, useNA = 'always')

table(data_nairobi$spazzolino, useNA = 'always')

table(data_nairobi$medical_camp,data_nairobi$spazzolino)

barplot(table(data_nairobi$medical_camp,data_nairobi$spazzolino),legend=levels(data_nairobi$medical_camp))

# Group instead of stacking
barplot( table(data_nairobi$medical_camp,data_nairobi$spazzolino),
         legend = levels(data_nairobi$medical_camp),           
         beside = TRUE)                           

prop.table(table(data_nairobi$spazzolino))
'''
       NO        SI 
0.2219251 0.7780749 
'''
# relative frequencies
table(data_nairobi$medical_camp,data_nairobi$spazzolino)/sum(table(data_nairobi$medical_camp,data_nairobi$spazzolino))
'''
                    NO         SI
  Korogocho  0.03475936 0.37165775
  Dandora    0.18716578 0.20855615
  Alice Home 0.00000000 0.19786096
  
'''
prop.table(table(data_nairobi$medical_camp,data_nairobi$spazzolino))

'''
                     NO         SI
  Korogocho  0.03475936 0.37165775
  Dandora    0.18716578 0.20855615
  Alice Home 0.00000000 0.19786096
  
'''
prop.table(table(data_nairobi$medical_camp,data_nairobi$spazzolino),margin=1) #margin 1 means by row
## conditional relative frequencies: by row
'''
                   NO         SI
  Korogocho  0.08552632 0.91447368
  Dandora    0.47297297 0.52702703
  Alice Home 0.00000000 1.00000000
'''
prop.table(table(data_nairobi$medical_camp,data_nairobi$spazzolino),margin=2)# margin 2 means by column
## conditional relative frequencies: by column
'''
                  NO        SI
  Korogocho  0.1566265 0.4776632
  Dandora    0.8433735 0.2680412
  Alice Home 0.0000000 0.2542955
'''

?tabpct

tabpct(data_nairobi$medical_camp,data_nairobi$spazzolino,
       xlab="Medical camp",ylab="Use of toothbrush",
       main="Use of toothbrushby\nmedical camp")

#functions to stratify the samples and compute summary statistics for each group for example to compute mean
# age distribution by use of toothbrush
#distribution of numeric variable
boxplot(data_nairobi$eta)

##~ the tilde sample
#we find it in the R it is used to introduce the formula, the formula notation to group the continuous numeric variable by categorical variable
#on the left side of tilde we have dependent variable and on the right side independent or grouped variable
#it is stratified continuous variable comparison
# our dependent variable is the numeric vector and independent or grouped variable is the character or categorical vector
# we grouped the numeric variable with the help of categorical variable and compute summary statistics for each group
str(data_nairobi$eta) #numeric vector
str(data_nairobi$spazzolino) #character vector
boxplot(data_nairobi$eta~data_nairobi$spazzolino)  
#here we use the graphical changes with the attribute col, main , xlab and y lab
boxplot(data_nairobi$eta~data_nairobi$spazzolino,col=c("red","green"),
        main="Age by use of toothbrush",
        xlab="Toothbrush",ylab="Age",varwidth=TRUE)
summary(data_nairobi$eta)
mean(data_nairobi$eta,na.rm=TRUE) #[1] 7.83378
'''
it is important to use na.rm=TRUE because there can be a missing value
if we do not use this argument the output of mean is NA 

'''
#it takes one numeric vector and split it into groups and then we can apply summary statics to each group
# we can apply tapply() on more than two groups of the categorical variable unlike independent t.test which takes only two group
tapply(data_nairobi$eta, INDEX = data_nairobi$spazzolino, FUN = mean, na.rm = TRUE)
unique(data_nairobi$spazzolino) #[1] "SI" "NO"
'''
      NO       SI 
6.890244 8.099656

'''
#tapply() can also take more than one categorical variable in the INDEX parameter using list() but the dependent or numeric vector is one
# the output is the matrix
#This table shows the mean age (eta) of children broken down by two factors: Toothbrush usage (spazzolino: NO vs. SI) across rows and medical camp / Location across columns
tapply(data_nairobi$eta, INDEX = list(data_nairobi$spazzolino,data_nairobi$medical_camp), FUN = mean, na.rm = TRUE)

#combine the labels of the groups of two categorical variable into one label using paste() function
tapply(data_nairobi$eta, INDEX = list(paste(data_nairobi$spazzolino,data_nairobi$medical_camp)), FUN = mean, na.rm = TRUE)

#in the aggregate we can use more than  one numeric vector in the first argument unlike tapply()
aggregate(data_nairobi$eta, by = list(data_nairobi$spazzolino), FUN = mean, na.rm = TRUE)

aggregate(data_nairobi$eta, list(data_nairobi$spazzolino,data_nairobi$medical_camp), mean, na.rm = TRUE)

#we will have a nicer output instead of  Group.1  and Group.2 we will have column names via indexing that is spazzolino and medical_camp
aggregate(data_nairobi$eta , by = data_nairobi[c(5, 3)], FUN = mean, na.rm = TRUE)

###independent t.test
# categorical variable must have only two groups
# df = n1+n2-2
# 0 should not be part of the confidence interval to reject null hypothesis
# the sample size of two group can be different
# the variance of both group should be same theoretically
# stratified study
t.test(eta ~ spazzolino, data = data_nairobi)
'''
 t.test(eta ~ spazzolino, data = data_nairobi)

	Welch Two Sample t-test

data:  eta by spazzolino
t = -4.6478, df = 197.84, p-value = 6.12e-06
alternative hypothesis: true difference in means between group NO and group SI is not equal to 0
95 percent confidence interval:
 -1.7225618 -0.6962631
sample estimates:
mean in group NO mean in group SI 
        6.890244         8.099656 
'''

### Contigency tables and association ----

# Sweets and juices
tabpct(data_nairobi$dolci,data_nairobi$succhi,
       main="Juice consumption by sweet consumption")
summary(table(data_nairobi$dolci,data_nairobi$succhi)) #summary applied to the different class the summary out put is completely different
'''
Number of cases in table: 369 
Number of factors: 2 
Test for independence of all factors:
	Chisq = 50.04, df = 1, p-value = 1.505e-12
'''
#chi square test 
table(data_nairobi$dolci, data_nairobi$succhi)
'''
     NO  SI
  NO 146  68
  SI  48 107
'''
chisq.test(table(data_nairobi$dolci, data_nairobi$succhi), correct = FALSE)

# 1. Run the Chi-Square test and save it into an object called 'chi_result'
chi_result <- chisq.test(table(data_nairobi$dolci, data_nairobi$succhi),correct = FALSE)

# 2. View the full test output (Chi-Square statistic, df, and p-value)
chi_result
'''
Pearsons Chi-squared test

data:  table(data_nairobi$dolci, data_nairobi$succhi)
X-squared = 50.042, df = 1, p-value = 1.505e-12
'''

chi_result$observed
'''
    NO  SI
  NO 146  68
  SI  48 107
'''

# 3. View the expected frequency table that R generated behind the scenes
chi_result$expected
'''
         NO        SI
  NO 112.50949 101.49051
  SI  81.49051  73.50949
'''
prop.table(chi_result$expected)
'''
        NO        SI
  NO 0.3049038 0.2750420
  SI 0.2208415 0.1992127
'''

prop.table(chi_result$expected,margin = 1) #both column are completely identicle
'''
          NO        SI
  NO 0.5257453 0.4742547
  SI 0.5257453 0.4742547
'''
prop.table(chi_result$expected,margin = 2) #both row are completely identicle
'''
           NO        SI
  NO 0.5799458 0.5799458
  SI 0.4200542 0.4200542
'''
### non-parametric test ----
#Non‑parametric tests make no distributional assumptions.
#Non‑parametric tests do not assume normality.
#They use ranks, medians, or signs instead of means.
#the input is numeric vector
'''
before running the t test and f test we need to see the normality
of the two populations
there are several statistical test to check the normality
1st one shapiro wilk test 
'''
?shapiro.test
'''
shapiro.test(x)
there is one single argument only
The Shapiro–Wilk test checks whether our data comes from a normal distribution
'''
shapiro.test(data_nairobi$eta)
#The W statistic measures how close the sample distribution is to a normal distribution (where W = 1 indicates perfect normality). 
#A value of 0.87363 indicates a noticeable departure from a symmetric bell curve.
#Because the p-value is far below the standard significance threshold (alpha = 0.05), we reject the null hypothesis (H_0) of normality.
# The age variable (eta) is not normally distributed.
'''
Shapiro-Wilk normality test

data:  data_nairobi$eta
W = 0.87363, p-value < 2.2e-16
'''
###wilcox.test
# If normality does not hold, let us opt for a non-parametric test..
# Here is the Mann-Whitney test for comparing distribution/ranks of two unpaired group
# Mann-Whitney test/(Wilcoxon Rank-Sum Test is equivalent to the two sample independent t.test
?wilcox.test
#Use case: Comparing 2 independent / unrelated group
wilcox.test(
  eta ~ spazzolino,
  data = data_nairobi,
  exact = FALSE,       # Recommended if your data has duplicate/tied values
)
'''
	Wilcoxon rank sum test with continuity
	correction

data:  eta by spazzolino
W = 8868, p-value = 0.0003119
alternative hypothesis: true location shift is not equal to 0
'''
#Conclusion: There is a statistically significant difference in age distribution between children who use a toothbrush (SI) and those who do not (NO).

#Because the Mann–Whitney U test compares distributions/ranks rather than means, always report the Median and Interquartile Range (IQR) alongside the test results:
#Accompanying descriptive stats (Median and IQR)
tapply(data_nairobi$eta, data_nairobi$spazzolino, median, na.rm = TRUE)
'''
NO SI 
 7  7
'''
tapply(data_nairobi$eta, data_nairobi$spazzolino, IQR, na.rm = TRUE)
'''
NO SI 
 2  3 
'''
###Kruskal–Wallis Test (For => 3 groups)
#Comparing 3 or more independent / unrelated groups.
#Nairobi dataset across 3 medical camps:
kruskal.test(eta ~ medical_camp, data = data_nairobi)
'''
	Kruskal-Wallis rank sum test

data:  eta by medical_camp
Kruskal-Wallis chi-squared = 59.844, df =
2, p-value = 1.012e-13
'''
#We reject the null hypothesis (H_0). This means there is a highly significant difference in the age distribution (eta) across at least one of the medical camps.
# Accompanying descriptive stats across 3+ groups
tapply(data_nairobi$eta, data_nairobi$medical_camp, median, na.rm = TRUE)
'''
 Korogocho    Dandora Alice Home 
         7          7         12 
'''
###Post-Hoc Testing
# Install and load FSA library if needed
install.packages("FSA")
library(FSA)

# Run Dunn's test with Bonferroni correction
#Because our Kruskal-Wallis test is statistically significant,we know that the groups differ, but it doesn't tell you which exact pairs differ So,to find out, run Dunn’s Post-Hoc Test:
dunnTest(eta ~ medical_camp, data = data_nairobi, method = "bonferroni")
#This will give us pairwise p-values comparing Korogocho vs. Dandora, Korogocho vs. Alice Home, and Dandora vs. Alice Home
#Age Homogeneity: Korogocho and Dandora are statistically similar in terms of participant age.
#Distinct Group: Alice Home represents a significantly older cohort of children in our survey compared to the other two camps.


###Runs Tests
# Install the package 
install.packages("randtests")

# Load the package
library(randtests)
#The runs test is a non-parametric test used to determine whether a sequence of data values has been produced in a random manner or if there is a systematic pattern, trend, or dependence in the order of the observations.
runs.test(data_nairobi$eta)
'''
	Runs Test

data:  data_nairobi$eta
statistic = -9.5689, runs = 61, n1 = 161,
n2 = 122, n = 283, p-value < 2.2e-16
alternative hypothesis: nonrandomness
'''
#Because p < 0.05 (p < 2.2e-16), we reject the null hypothesis (H_0) that is, The data sequence was produced in a random manner (observations are independent).
#We accept H_1,the data sequence is non-random (there is clustering or a pattern in how the rows are ordered).
#This is very common in clinical field surveys—children examined on the same day or at the same station tend to share similar characteristics (like being the exact same age), 
#which creates clustering rather than a completely randomized sequence.So,No it does not ruin your analysis.
#Strict randomness of row order only matters for a standard public health survey
#our data is completely fine to use. The Runs Test simply confirmed a structural reality of your dataset: our rows are organized by groups (like medical camps or schools) rather than shuffled completely at random.


## Export a dataset ----
#how to save the work space is by going to the session the extension is the .RData
##read means to import and write means to export
#convert the data frame to the csv

#.csv2 produce file with deparater ; and decimal ,
write.csv2(data_nairobi,file="DatiPulitiNairobi.csv")
write.csv2(data_nairobi,file="DatiPulitiNairobi.csv",
           quote=FALSE) #it can be used to delete "" around the character vector, it re run and we get the file modify not the new file

##produce mile with separater , and decimal.
write.csv(data_nairobi,file="Dati_Puliti_Nairobi.csv",
          quote=FALSE) 

save.image(file="LectureNairobi.RData")#the work space is created in the same directory


