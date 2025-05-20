# Differential influence of habitual behavior components on compulsive and
# problematic reward-seeking behavior

# Load database if Experiment 1
Q2 <- read.csv(file.path(data_path,'Experiment_2.csv'), check.names = F)

Q2$sub = Q2$id + 1000

# Load databases ---------------------------------------------------------------

# Get databases from B1 classes
B1 <- read.csv(file.path(data_path, 'Experiment_1_B1.csv'), check.names = F)

# Get databases from B2 classes
B2 <- read.csv(file.path(data_path, 'Experiment_1_B2.csv'), check.names = F)

# Get databases from B2 classes second round
B3 <- read.csv(file.path(data_path, 'Experiment_1_B3.csv'), check.names = F)

# Merge two datasets

# Create a dataset id
B2$dataset = 'B2'
B1$dataset = 'B1'
B3$dataset = 'B3'

# Create unique id for each dataset
B2$sub = B2$sub + 200
B3$sub = B3$sub + 300

# Combine scales of short and long UPPS (for Experiment 1 only) ----------------

# Keep only negative urgency of short version for consistency with long version
B1$UPPS_urgency <- B1$UPPS_negative_urgency

# Scale then merge
# Set version on the same scale
B1$UPPS_urgency <- scale(B1$UPPS_urgency)
B2$UPPS_urgency <- scale(B2$UPPS_urgency)
B3$UPPS_urgency <- scale(B3$UPPS_urgency)

B1$UPPS_premeditation <- scale(B1$UPPS_premeditation)
B2$UPPS_premeditation <- scale(B2$UPPS_premeditation)
B3$UPPS_premeditation <- scale(B3$UPPS_premeditation)

B1$UPPS_perseverance <- scale(B1$UPPS_perseverance )
B2$UPPS_perseverance  <- scale(B2$UPPS_perseverance )
B3$UPPS_perseverance  <- scale(B3$UPPS_perseverance )

B1$UPPS_sensation     <- scale(B1$UPPS_sensation )
B2$UPPS_sensation     <- scale(B2$UPPS_sensation )
B3$UPPS_sensation     <- scale(B3$UPPS_sensation )

# Compute total
col <- paste("UPPS[", 1:20, "]", sep = "")
B1$UPPS_total_raw <- apply(B1[col], 1, sum)

col <- paste("UPPS[", 1:45, "]", sep = "")
B2$UPPS_total_raw <- apply(B2[col], 1, sum)

col <- paste("UPPS[", 1:45, "]", sep = "")
B3$UPPS_total_raw <- apply(B3[col], 1, sum)

B1$UPPS_total   <- scale(B1$UPPS_total_raw)
B2$UPPS_total   <- scale(B2$UPPS_total_raw)
B3$UPPS_total   <- scale(B3$UPPS_total_raw)

# Merge
TMP   <- join (B1, B2, type = 'full')
Q1 <- join (TMP, B3, type = 'full')

# Remove participants ----------------------------------------------------------

# Remove for missing data
Q1 <-  subset(Q1, sub != 'NA') # Remove missing lines
Q1 <- subset(Q1, sub != '83') # Participant only used extreme values on each scale and the time to complete the questionnaire is too short to have read the questions
Q1 <- subset(Q1, sub != '497') # participant only used the extreme values on each scale and the time to complete the questionnaire is too short to have read the questions
Q1 <- subset(Q1, sub != '499') # participant only used the extreme values on each scale and the time to complete the questionnaire is too short to have read the questions

# Experiment 2: impute missing data for Yale Food Addiction questionnaire ------

# Create an alternative database for coding
col <- paste("mYFAS2[", 1:13, "]", sep = "")
codeyfas <- Q2[col] # Creates another data.base containing only YFAS data to code them while keeping the real item scores available in the original data.base
codeyfas[ , c(3, 7, 12, 13)] <- lapply(codeyfas[ , c(3, 7, 12, 13)], function(x) ifelse(x >= 2, 1, 0)) # Codes the scores of the 3rd, 7th, 12th, 13th items 0 or 1
codeyfas[ , c(1, 4, 8, 10)] <- lapply(codeyfas[ , c(1, 4, 8, 10)], function(x) ifelse(x >= 4, 1, 0)) # Same for items 1, 4, 8 and 10
codeyfas[ , c(2, 5, 6, 9, 11)] <- lapply(codeyfas[ , c(2, 5, 6, 9, 11)], function(x) ifelse(x >= 5, 1, 0)) # Same for items 2, 5, 6, 9 and 11

# Fitting a logistic regression model of mYFAS[3] on non-missing data
pred1 <- Q2$'mYFAS2[1]'[!is.na(Q2$'mYFAS2[3]')]
pred2 <- Q2$'mYFAS2[2]'[!is.na(Q2$'mYFAS2[3]')]
pred4 <- Q2$'mYFAS2[4]'[!is.na(Q2$'mYFAS2[3]')]
pred7 <- Q2$'mYFAS2[7]'[!is.na(Q2$'mYFAS2[3]')]
pred8 <- Q2$'mYFAS2[8]'[!is.na(Q2$'mYFAS2[3]')]
pred9 <- Q2$'mYFAS2[9]'[!is.na(Q2$'mYFAS2[3]')]
pred10 <- Q2$'mYFAS2[10]'[!is.na(Q2$'mYFAS2[3]')]
pred11 <- Q2$'mYFAS2[11]'[!is.na(Q2$'mYFAS2[3]')]
pred12 <- Q2$'mYFAS2[12]'[!is.na(Q2$'mYFAS2[3]')]
pred13 <- Q2$'mYFAS2[13]'[!is.na(Q2$'mYFAS2[3]')]

imput.model <- glm(codeyfas[!is.na(codeyfas$'mYFAS2[3]'), "mYFAS2[3]"] ~ pred1 + pred2 + pred4 + pred7 + pred8 + pred9 + pred10 + pred11 + pred12 + pred13, family = "binomial")

# Applying logistic regression threshold rule to missing data
pred1 <- Q2$'mYFAS2[1]'[is.na(Q2$'mYFAS2[3]')]
pred2 <- Q2$'mYFAS2[2]'[is.na(Q2$'mYFAS2[3]')]
pred4 <- Q2$'mYFAS2[4]'[is.na(Q2$'mYFAS2[3]')]
pred7 <- Q2$'mYFAS2[7]'[is.na(Q2$'mYFAS2[3]')]
pred8 <- Q2$'mYFAS2[8]'[is.na(Q2$'mYFAS2[3]')]
pred9 <- Q2$'mYFAS2[9]'[is.na(Q2$'mYFAS2[3]')]
pred10 <- Q2$'mYFAS2[10]'[is.na(Q2$'mYFAS2[3]')]
pred11 <- Q2$'mYFAS2[11]'[is.na(Q2$'mYFAS2[3]')]
pred12 <- Q2$'mYFAS2[12]'[is.na(Q2$'mYFAS2[3]')]
pred13 <- Q2$'mYFAS2[13]'[is.na(Q2$'mYFAS2[3]')]

yfas.imput <- ifelse((1/(1+exp(-(imput.model$coefficients[1] + pred1*imput.model$coefficients[2] + pred2*imput.model$coefficients[3] + pred4*imput.model$coefficients[4] + pred7*imput.model$coefficients[5] + pred8*imput.model$coefficients[6] + pred9*imput.model$coefficients[7] + pred10*imput.model$coefficients[8] + pred11*imput.model$coefficients[9] + pred12*imput.model$coefficients[10] + pred13*imput.model$coefficients[11])))) > 0.175, 1, 0)
codeyfas$'mYFAS2[3]'[is.na(codeyfas$'mYFAS2[3]')] <- yfas.imput

yfas.score.list <- paste("mYFAS2[", c(1:4, 7:13) ,"]", sep = "")
Q2$YFAS_score <- apply(codeyfas[yfas.score.list], 1, sum)

# Compute BAS total ------------------------------------------------------------

Q2$BISBAS_BAS = Q2$BISBAS_BAS_Drive + Q2$BISBAS_BAS_Fun + Q2$BISBAS_BAS_Reward

# Compute UPPS total -----------------------------------------------------------

col <- paste("UPPS[", 1:20, "]", sep = "")

Q2$UPPS_total <- apply(Q2[col], 1, sum)

# Compute COHS total -----------------------------------------------------------
col <- paste("COHS[",1:27,"]", sep="")

# Total score variable
Q1$COHS_total  <- apply(Q1[col],1,sum)

# Compute subscales for CES-D in Experiment 1 ----------------------------------

col <- paste("CESD[", 1:20, "]", sep = "")

# Compute subscales
# Depressed affect
list_DA <- col[c(3, 6, 9, 10, 14, 17, 18)]
Q1$CESD_depressed <- apply(Q1[list_DA], 1, sum)

# Positive affect
list_PA <- col[c(4, 8, 12, 16)]
Q1$CESD_positive <- apply(Q1[list_PA], 1, sum)

# Somatic complains
list_SC <- col[c(1, 2, 5, 7, 11, 13, 20)]
Q1$CESD_somatic <- apply(Q1[list_SC], 1, sum)

# Disturbed interpersonal relationships
list_DIR <- col[c(15, 19)]
Q1$CESD_relationship <- apply(Q1[list_DIR], 1, sum)

# Compute subscales for IAT for Experimenet 2 ----------------------------------

# Salience
list_salience = c(10, 12, 13, 15, 19)
col = paste("IAT[",list_salience ,"]", sep="")
Q2$IAT_salience <- apply(Q2[col], 1, sum)

# Excessive use
list_excessive_use = c(1, 2, 14, 18, 20)
col = paste("IAT[",list_excessive_use ,"]", sep="")
Q2$IAT_excessive_use <- apply(Q2[col], 1, sum)

# Neglect work 
list_neglect_work = c(6, 8, 9)
col = paste("IAT[",list_neglect_work ,"]", sep="")
Q2$IAT_neglect_work <- apply(Q2[col], 1, sum)

# Anticipation
list_anticipation = c(7, 11)
col = paste("IAT[",list_anticipation ,"]", sep="")
Q2$IAT_anticipation <- apply(Q2[col], 1, sum)

# Lack of control
list_lack_control = c(5, 16, 17)
col = paste("IAT[",list_lack_control ,"]", sep="")
Q2$IAT_lack_control <- apply(Q2[col], 1, sum)

# Neglect social life 
list_neglect_social_life = c(3, 4)
col = paste("IAT[",list_neglect_social_life ,"]", sep="")
Q2$IAT_neglect_social_life <- apply(Q2[col], 1, sum)

# COHS validation --------------------------------------------------------------

# Experiment 1
col = paste("COHS[",1:27,"]", sep="")
db1.COHS <- Q1[col]

# Demographics for COHS validation db
demo_col = c("sub", "Age", "Genre", col)
demo1 <- Q1[demo_col]

db1.COHS <- na.omit(db1.COHS) # Remove NAN lines
demo1<- na.omit(demo1) # Remove NAN lines

# Experiment 2
db2.COHS <- Q2[col]
names(Q2)[9] = "Age" # Rename to have the same labels as in Experiment 1
names(Q2)[10] = "Genre"
demo2 <- Q2[demo_col]

db2.COHS <- na.omit(db2.COHS) # Remove NAN lines
demo2<- na.omit(demo2) # Remove NAN lines

# Set blank cell of genre in demo2 as NA
demo2[demo2 == ""] <- "N/A" # Replace blank by NA

# Join
db.COHS  = join (db2.COHS, db1.COHS, type = "full")
db.COHS.demo = join(demo1, demo2, type = "full")

# Name the items in a way that lavaan will also recognize
db.COHS.cfa <- db.COHS
col_old = paste("COHS[",1:27,"]", sep="")
col_new = paste("item",1:27,"", sep="")
colnames(db.COHS.cfa)[colnames(db.COHS.cfa) == col_old] <- col_new

# Compute the db.cohs scales
# Routine
list_routine <- c(15, 27, 13, 6, 24, 2, 12, 17, 7, 10, 20, 1, 22, 18, 4, 14)
col_rout = paste("COHS[",list_routine,"]", sep="")
db.COHS$COHS_routine <- apply(db.COHS[col_rout],1,sum)

# Automaticity
list_automaticity <- c(11, 25, 3, 23, 19, 26, 8, 9, 16, 5, 21)
col_auto = paste("COHS[",list_automaticity,"]", sep="")
db.COHS$COHS_automaticity <- apply(db.COHS[col_auto],1,sum)

# Plots aesthetics -------------------------------------------------------------

# Setup palette
pal = viridis::inferno(n=10)
pal_sy = viridis::viridis(n=10)

# Dataset for alphas -----------------------------------------------------------

# COHS
col_COHS = paste("COHS[",1:27,"]", sep="")

# CES-d
col_CESD = paste("CESD[",1:20,"]", sep="")

# EAT
col_EAT26 = paste("EAT26[",1:26,"]", sep="")

# IAT
col_IAT = paste("IAT[",1:20,"]", sep="")

# PCLS
col_PCLS_total = paste("PCLS[",1:17,"]", sep="")

# PMPUBQSV
col_PMPUBQSV = paste("PMPUQSV[",1:15,"]", sep="")

# PSS
col_PSS = paste("PSS[",1:10,"]", sep="")

# QABB
col_QABB = paste("QABB[",1:19,"]", sep="")

# OCI-R
col_OCIR = paste("OCIR[",1:18,"]", sep="")

#STAIS
col_STAIS = paste("STAIS[",1:20,"]", sep="")

# STAIT
col_STAIT = paste("STAIT[",1:20,"]", sep="")

# STICSA-S
col_STICSAE = paste("STICSAE[",1:21,"]", sep="")

# STICSA-T
col_STICSAT = paste("STICSAT[",1:21,"]", sep="")

# GAS
col_GAS = paste("GAS[",1:7,"]", sep="")

# SASSV
col_SASSV = paste("SASSV[",1:10,"]", sep="")

# EDSR
col_EDSR = paste("EDSR[",1:21,"]", sep="")

#YFAS
col_YFAS  <- paste("mYFAS2[", c(1:4, 7:13) ,"]", sep = "")

# UPPS
col_UPPS = paste("UPPS[",1:20,"]", sep="")

# LSAS
#col_LSAS = paste("LSAS[",1:20,"]", sep="")
col_e <- paste("LSAS[", 1:24, "][1]", sep = "") #
col_a <- paste("LSAS[", 1:24, "][2]", sep = "") #
col_LSAS = c(col_e,col_a)

# Create a list with the 2 subscales to see the alpha and correlations
Q2.alphas <- list(CEDS = c(col_CESD),
                  COHS = c(col_COHS),
                  EAT26 = c(col_EAT26),
                  IAT = c(col_IAT),
                  PCLS = c(col_PCLS_total),
                  SASSV= c(col_SASSV),
                  PSS = c(col_PSS),
                  QABB = c (col_QABB),
                  OCIR = c(col_OCIR),
                  STICSAT = c(col_STICSAT),
                  STICSAE = c(col_STICSAE),
                  GAS = c(col_GAS),
                  EDSR = c(col_EDSR),
                  UPPS = c(col_UPPS),
                  YFAS = c(col_YFAS),
                  LSAS = c(col_LSAS)
)

# Create a list with the 2 subscales to see the alpha and correlations
Q1.alphas <- list(CEDS = c(col_CESD),
            COHS = c(col_COHS),
            EAT26 = c(col_EAT26),
            IAT = c(col_IAT),
            PMPUBQSV = c(col_PMPUBQSV),
            PSS = c(col_PSS),
            OCIR = c(col_OCIR),
            STAIT = c(col_STAIT)
)
