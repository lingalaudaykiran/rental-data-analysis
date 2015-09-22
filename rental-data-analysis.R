library("parallel")
library("foreach")
library("doParallel")
require(plyr)
library(NbClust)
set.seed(1234)


###################################################
clean the data

n_col <- max(count.fields("testData.txt", sep = ","))

n_col
clean.data <- read.table(file="testData.txt", sep = ",",
	fill=TRUE, col.names=1:n_col)

misplaced <-which(clean.data$X1=="")

 
misplaced.data <- clean.data[misplaced,]

cleaned.data.partial1 <- clean.data[-misplaced,-8]

head(cleaned.data.partial1)
head(misplaced.data)
cleaned.data.partial2 <- misplaced.data[,-1]

colnames<- as.character(unlist(cleaned.data.partial1[1,]))
colnames(cleaned.data.partial1) <- colnames

colnames(cleaned.data.partial2) <- colnames
cleaned.data.partial1 <- cleaned.data.partial1[-1,]
head(cleaned.data.partial1)
head(cleaned.data.partial2)

length(cleaned.data.partial1$name)#56897
length(cleaned.data.partial2$name)#2916

cleaned.data<-merge(cleaned.data.partial1, cleaned.data.partial2, all=TRUE)
head(cleaned.data)



cleaned.data$dob <- as.character(cleaned.data$dob)
cleaned.data$paymentDate <- as.character(cleaned.data$paymentDate)


###########make data DOB and paymentDate


dob.wrong.format <- cleaned.data$dob[which(nchar(
                   cleaned.data$dob)==8)]
dob.date.format <- strptime(dob.wrong.format, "%Y%m%d" )
dob.right.format <- format(dob.date.format, "%m/%d/%Y")
dob <- cleaned.data$dob

dob[which(nchar(cleaned.data$dob)==8)] <-dob.right.format




pDate.wrong.format <- cleaned.data$paymentDate[which(nchar(
                   cleaned.data$paymentDate)==8)]
pDate.date.format <- strptime(pDate.wrong.format, "%Y%m%d" )
pDate.right.format <- format(pDate.date.format, "%m/%d/%Y")

pDate <- cleaned.data$paymentDate
pDate[which(nchar(cleaned.data$paymentDate)==8)] <- pDate.right.format




cleaned.data$dob.format <-dob
cleaned.data$paymentDate.format <- pDate
cleaned.data$dob.format <-as.character(cleaned.data$dob.format)
cleaned.data$paymentDate.format <-as.character(cleaned.data$paymentDate.format)
head(cleaned.data)



cleaned.data$paymentAmount <-as.numeric(as.character(cleaned.data$paymentAmount))

cleaned.data$paymentAmount[is.na(cleaned.data$paymentAmount)] <-0

cleaned.data$rentAmount <-as.numeric(as.character(cleaned.data$rentAmount))


############################

###create new date set with removing worng formats feild.

###age at the time of rent



cleaned.data.formated <- cleaned.data[, c(-2,-5)]

head(cleaned.data.formated )

age <- round(difftime(strptime(cleaned.data.formated$paymentDate.format, format = "%m/%d/%Y"),
strptime(cleaned.data.formated$dob.format, format = "%m/%d/%Y"),units="weeks")/52.14)

cleaned.data.formated$age <-age

head(cleaned.data.formated )


##### difference in payment vs rent.
cleaned.data.formated$rent.diff <- cleaned.data.formated$paymentAmount - 
                                    cleaned.data.formated$rentAmount


##order the data based on the name and payment date

cleaned.data.formated<-cleaned.data.formated[order(cleaned.data.formated$name,
	strptime(cleaned.data.formated$paymentDate.format, format = "%m/%d/%Y" )),]
head(cleaned.data.formated )
#### get the month and day of the payment date

date.format <-strptime(cleaned.data.formated$paymentDate.format, format= "%m/%d/%Y" )

 cleaned.data.formated$paymentDay <- as.numeric(format( date.format, format="%d"))

cleaned.data.formated$paymentmonth <- as.numeric(format( date.format, format="%m"))



head(cleaned.data.formated )




##################################ddply for renters to know how many
### time they skipped the rent payment and how many times they paid the rents.


rental.payment.data <-ddply( cleaned.data.formated, c("name"), summarise, 
				number.of.payments = length(paymentAmount),
				number.of.miss.payments = length(which(paymentAmount<=0)),
				 age.years = as.numeric(mean(age)), 
				# diff.houses = length(unique(houseID)),
				# diff.places = length(unique(houseZip)),
				rent.diff.mean = mean(rent.diff), rent.diff.var = var(rent.diff),
                         mean.paymentDay = mean(paymentDay)	)

head(rental.payment.data)


rental.dates.data <- ddply( cleaned.data.formated, c("name"),
			summarise, paymentDate = sort(as.Date(paymentDate.format, format="%m/%d/%Y")))


###find the frequency of payment in ,months.

head(rental.dates.data)

rental.dates.diff <- ddply(rental.dates.data, c("name"),
			summarise, months = diff(paymentDate)/30 )

head(rental.total.months)
head(rental.dates.diff)
rental.dates.diff.mean <- ddply(rental.dates.diff, c("name"),
			summarise, months.mean = mean(months), 
			total.months = sum(months), months.var = var(months))
head(rental.dates.diff.mean)

single.rent.name <-rental.dates.data[
                 ! rental.dates.data$name %in% rental.dates.diff.mean$name, 1 ]

single.rent.names.months <- data.frame(single.rent.name)
colnames(single.rent.names.months)[1] <- "name"
single.rent.names.months$months.mean<- rep(1, length(single.rent.name))
single.rent.names.months$total.months<- rep(1, length(single.rent.name))

rental.dates.diff.mean <- merge(rental.dates.diff.mean, single.rent.names.months, all=TRUE)





rental.payment.data$months.paymentDate <- rental.dates.diff.mean$months.mean
rental.payment.data$paymentDate.var <- rental.dates.diff.mean$months.var



rental.payment.data$months.paymentDate <- as.numeric(rental.payment.data$months.paymentDate)




head(rental.payment.data)










####analysis



###analysis


analysis.data <- rental.payment.data[rental.payment.data$age.years<100 &
          rental.payment.data$age.years>0 ,-1]

head(analysis.data)



plot(analysis.data$age.years, analysis.data$number.of.miss.payments)

plot(analysis.data$rent.diff.var, analysis.data$paymentDate.var)


plot(analysis.data$age.years, analysis.data$rent.diff.sum)

plot(analysis.data$age.years, analysis.data$median.paymentDay)

plot(analysis.data$age.years, analysis.data$mean.paymentDay)

plot(analysis.data$rent.diff.sum, analysis.data$mean.paymentDay)## very important

plot(analysis.data$rent.diff.mean, analysis.data$paymentDate.var)## very important

plot(analysis.data$rent.diff.sum, analysis.data$number.of.miss.payments)


plot(analysis.data$number.of.miss.payments, analysis.data$months.paymentDate)





plot(analysis.data)


plot(analysis.data$age, analysis.data$months.paymentDate)

cov(analysis.data$rent.diff.sum, analysis.data$mean.paymentDay) # 5432

var(analysis.data$rent.diff.sum, analysis.data$mean.paymentDay)

cov(analysis.data)




###### ages corrected analysis
#rental.payment.data.corrected.age <- rental.payment.data[which(rental.payment.data$age.years<=100),]

#plot(rental.payment.data.corrected.age$age.years, rental.payment.data.corrected.age$number.of.miss.payments)
#plot(rental.payment.data.corrected.age$age.years, rental.payment.data.corrected.age$rent.diff.sum)

#plot(rental.payment.data.corrected.age$age.years, rental.payment.data.corrected.age$median.paymentDay)

#plot(rental.payment.data.corrected.age$age.years, rental.payment.data.corrected.age$mean.paymentDay)









#### k mean clustering


analysis.data <- rental.payment.data[rental.payment.data$age.years<100 &
          rental.payment.data$age.years>0 ,]
analysis.data.no.age <- rental.payment.data[rental.payment.data$age.years>100 |
          rental.payment.data$age.years<0 ,]

##analysis.data <- rental.payment.data
head(analysis.data)
analysis.data <- na.omit(analysis.data)

#analysis.data <- scale(analysis.data)


mydata <- analysis.data[,-1]

head(mydata)

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
  	centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
  ylab="Within groups sum of squares")


nc <- NbClust(mydata, min.nc=2, max.nc=15, method="kmeans")## suggested 3 clusters




fit.km <- kmeans(mydata, 3, nstart=25)
fit.km$size###609  61 299



fit.km$centers ###

 ## number.of.payments number.of.miss.payments age.years rent.diff.sum rent.diff.mean
##1           29.82266               0.1182266  33.58851      484.1699       16.49124
##2           28.45902               2.0491803  31.27006    -2204.3639      -82.09303
##3           31.43813               1.2307692  32.53702    -1344.3744      -42.99242
##  rent.diff.var mean.paymentDay median.paymentDay months.paymentDate paymentDate.var
##1      6752.333        2.918807          2.617406           1.014417      0.03279973
##2    271694.246        4.874214          4.303279           1.019909      0.08134514
##3    107867.805        2.848742          2.555184           1.013295      0.03062510


analysis.data$class <- fit.km$cluster

rf1 = randomForest(class~., 
       data=analysis.data, mtry=5, ntree=400)



