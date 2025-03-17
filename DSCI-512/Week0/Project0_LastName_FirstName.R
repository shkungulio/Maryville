#############################################
#                                           #
# Author:     FirstName LastName            #
# Date:       10/25/2018                    #
# Subject:    Project 0                     #
# Class:      BDAT 640                      #
# Section:    01W                           #         
# Instructor: Chris Shannon                 #
# File Name:  Project0_LastName_FirstName.R #
#                                           #
#############################################

# 1.1 Read the dataset in titanic2.csv into R.
#     Call the loaded data titanic.
titanic <- read.csv("titanic2.csv", header=T)

# 1.2 Make sure that you have the directory set 
#     to the correct location for the data.
#setwd("R:/bdat640_19_SP01")

# 2.1 How many rows are in the data frame?
#     Answer: 14
nrow(titanic)

# 2.2 How many columns? 
#     Answer: 5
ncol(titanic)

       
# 2.3 What do the rows and columns represent?
#     Answer: Each row represents class of passenger or crew,
#             Each column represents a characteristic
#             for each passenger, which include the
#             characteristics Clas, Sex, Age, Survived, Died.

# NOTE: MAKE SURE YOU WRAP LONG LINES SO THAT THEY ARE
#       EASILY READ IN A TEXT EDITOR, AS IN THE ABOVE ANSWER
#       THE NEXT LINE IS AN EXAMPLE OF WHAT NOT TO DO:
#     Answer: Each row represents class of passenger or crew, Each column represents a characteristic For each passenger, which include the characteristics Clas, Sex, Age, Survived, Died.
#
#     YOU WILL LOSE POINTS IF COMMENT LINES ARE TOO LONG!

# 3.1 Select the 1st, 5th, and 10th rows with
#     columns Class and Age.
#     Answer:
#    Class   Age    Sex Survived Died
# 1    1st Child Female        1    0
# 5    2nd Adult Female       11    0
# 10   3rd Child   Male       76   89
# THIS IS TABULAR DATA, SO YOU MAY TAKE A SCREEN SHOT 
# OF THIS FOR INCLUDING IN YOUR MICROSOFT WORD DOCUMENT.
titanic[c(1,5,10),c("Class", "Age")]

# 4.1 Regress Survived and Died on the predictors Class, Sex and Age
#     using the two-column form for the dependent variable.
fit <- glm(cbind(Survived, Died) ~ Class + Sex + Age,
           family=binomial, 
           data=titanic)

# THIS IS TABULAR DATA, SO YOU MAY TAKE A SCREEN SHOT 
# OF THIS FOR INCLUDING IN YOUR MICROSOFT WORD DOCUMENT.
summary(fit) 

# 4.2 Are any of the predictors associated with survival?
# Answer: AgeChild is positively associated with survival.
#         Class2nd, Class3rd, ClassCrew and SexMale are
#         all negatively associated with survival.

# 4.3 If so, explain the relationship based on the t-statistics.
# Answer: The t-statistics for AgeChild, Class2nd, Class3rd, ClassCrew
#         and SexMale are all well below the significance threshold
#         of 0.05.

# 4.4 Explain the chances of survival in terms of odds, giving the 
#     precise numbers and giving in terms a non-expert can understand.
# Answer: 
#  Class2nd   Class3rd  ClassCrew    SexMale   AgeChild 
# 0.3612825  0.1690159  0.4241466  0.3459219 11.2465380
#
# Second Class and Male each have an approximate survival
# rate of 1 in 3. If you are in Third Class, the odds of your
# survival is approximately 1 to 6, and if you are in the crew,
# your odds of survival are approximately 4 to 10. But if you
# are a child, the odds of your survival are better than
# 11 to 1.
odds <- exp(fit$coefficients)[-1]
odds

# 5.1 What are the odds of a male 1st-class and a male 3rd-class
#     child surviving the Titanic? Show the precise odds and
#     explain in a way a non-expert can understand.
# Answer: First-class Male Child: 35.0000
#         Third-class Male Child:  0.8539
# 
# A #male child who is a first-class passenger has 35 to 1 odds
# of surviging the Titanic. Your survival is almost guaranteed.
# But a third-class male child, however, has 5 to 6 odds of
# surviving.
# 
# display data and calculate with numbers from the list.
titanic
p1 <- 140/144 
p3 <- 76/(76+89)

p1.odds <- p1/(1 - p1)
p3.odds <- p3/(1 - p3)

results <- c(`First-class Male Child`=p1.odds,
             `3rd-class Male Child`=p3.odds)
round(results, 2)

# 6.1 Show a histogram of the Chi-Square distribution with 8 (upper) 
#     and 13 (lower) degrees of freedom.
cdist <- qchisq((1:999)/1000, 8,13)
hist(cdist, breaks=20, col="lavender", probability = T,
     xlab="Chi-square, with df=(8,13)",
     main="Histogram of Chi-Square Distribution\nWith 8, 13 degrees freedom")
points(density(cdist), col="blue", type="l", lwd=3)

# 6.2 On the same graphic, draw a vertical dotted line to
#     show the critical test value (0.05 significance level).
#     Make sure your graphic is properly titled and labeled.

critical.value <- qchisq(0.95, 8,13)
abline(v=critical.value, type="l",lty=2,col="green",lwd=3)
text(critical.value -4, 0.045, "0.95\nsignificance\nlevel")

# End assignment