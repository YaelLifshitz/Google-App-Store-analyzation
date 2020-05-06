## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----Libraries, message=FALSE, warning = FALSE, cache=TRUE---------------
library(knitr)
library(kableExtra)
library(MASS)
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)


## ----Data Import, cache=TRUE---------------------------------------------
apps_data <- read.csv("googleplaystore.csv", header = T)
# Removal of duplicate entries
apps_data<- distinct(apps_data)


## ---- eval=FALSE, echo = FALSE-------------------------------------------
## colnames(apps_data)


## ---- cache=TRUE, echo = FALSE-------------------------------------------
kable(head(apps_data), format = "html")  %>%
  kable_styling( bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>% 
  column_spec(column = 1, width_min = "10em", border_right = T, bold = T) %>%
  scroll_box(fixed_thead = T, width = "800px", height = "300px")


## ----Categories, cache=TRUE, echo=FALSE----------------------------------
comp1<- apps_data %>% group_by(Category, Type) %>% summarise(num=n()) %>% filter(Type == "Free" || Type == "Paid")
comp1$Type2<- as.character(comp1$Type)
comp1<-comp1 %>% filter(Type2 != "NaN")
levels(comp1$Category)


## ----PaidvsFree, cache=TRUE, echo=FALSE----------------------------------

ggplot(comp1, aes(y=num, x=Type, color=Type, fill=Category)) + 
    geom_bar( stat="identity", show.legend = F) +    
    facet_wrap(~Category) +
    labs(title="Number of Paid Apps vs. Free Apps based on Category", y="Number of Apps", x="Type")


## ----PaidAll, cache=TRUE, echo=FALSE-------------------------------------
comp2<- apps_data%>%group_by(Category, Type)%>%summarise(num=n())%>%filter(Type == "Paid")
comp2$Type2<- as.character(comp2$Type)
comp2<-comp2%>%filter(Type2 != "NaN")
ggplot(comp2, aes(y=num, x=Type, color=Type, fill=Category)) + 
    geom_bar( stat="identity", show.legend = F) +    
    facet_wrap(~Category) +
    labs(title="Number of Paid Apps based on Category", y="Number of Apps", x="Type")


## ----Cleaning Data, message=FALSE, warning = FALSE, cache= TRUE----------
paidApps<-apps_data%>%filter(Type == "Paid")

paidApps$Price.num <- as.numeric(gsub("[\\$,]", "", paidApps$Price))
paidApps$Installs_notlog <-as.numeric(gsub("[\\+,]", "", paidApps$Installs))
paidApps$Installs.num <- log10(as.numeric(gsub("[\\+,]", "", paidApps$Installs)))
paidApps$Installs.num[paidApps$Installs=='0+']<-0
paidApps$Rating.num <- as.numeric(paidApps$Rating)
paidApps$Reviews.num <- as.numeric(paidApps$Reviews)
paidApps$Size.num<- as.numeric(gsub("[\\M,]", "", paidApps$Size))
paidApps<-paidApps[complete.cases(paidApps),]


## ----Useful Apps, cache = TRUE-------------------------------------------
usefulPApps<-paidApps%>%filter(Category=="FAMILY" | Category== "GAME" | Category== "MEDICAL" | Category ==  "PERSONALIZATION" | Category ==  "TOOLS")
usefulPApps <- data.frame(App = usefulPApps$App, 
                          Category = usefulPApps$Category,
                          Rating = usefulPApps$Rating.num, 
                          Reviews = usefulPApps$Reviews.num, 
                          Size = usefulPApps$Size.num, 
                          Installs = usefulPApps$Installs_notlog, 
                          Installs.log = usefulPApps$Installs.num, 
                          Price = usefulPApps$Price.num )
kable(usefulPApps[1:15,], format = "html")  %>%
  kable_styling( bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>% 
  column_spec(column = 1, width_min = "10em", border_right = T, bold = T) %>%
  scroll_box(fixed_thead = T, width = "800px", height = "300px")
usefulPApps$Category <- factor(usefulPApps$Category)
levels(usefulPApps$Category)
nrow(usefulPApps)


## ----Data Variables, cache=TRUE------------------------------------------
par(mfrow=c(2,3))
hist(usefulPApps$Price, main = "Price of App", xlab = "Price")
hist(usefulPApps$Rating, main = "Ratings", xlab = "Rating")
hist(usefulPApps$Installs.log, main = "Number of Downloads", xlab = "Installs (log)")
hist(usefulPApps$Reviews, main = "Number of Reviews", xlab = "Reviews")
hist(usefulPApps$Size, main = "Size of App", xlab = "Size (Mb)")
bp<-barplot(table(usefulPApps$Category), main = "Categories", las = 2, col = c(1:5), xaxt = "n")
text(bp, par("usr")[3], labels = as.character(levels(usefulPApps$Category)), srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.8)


## ---- cache = TRUE-------------------------------------------------------
plot(usefulPApps[c(7,8,3,4,5)],col = usefulPApps$Category)


## ---- cache=TRUE---------------------------------------------------------
kable(usefulPApps%>%filter(Price>50), format = "html")  %>%
  kable_styling( bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>% 
  column_spec(column = 1, width_min = "10em", border_right = T, bold = T) %>%
  scroll_box(fixed_thead = T, width = "800px")


## ---- cache = TRUE-------------------------------------------------------
usefulPApps2<-usefulPApps%>%filter(Price<50)
plot(usefulPApps2[c(7,8,3,4,5)],col = usefulPApps2$Category)



## ---- cache = TRUE-------------------------------------------------------
nrow(usefulPApps2)


## ----Linear Models, cache=TRUE-------------------------------------------
price.lm <- lm(formula = Installs.log~Price, data = usefulPApps2)
summary(price.lm)

reviews.lm <- lm(formula = Installs.log~Reviews, data = usefulPApps2)
summary(reviews.lm)

rating.lm <- lm(formula = Installs.log~Rating, data = usefulPApps2)
summary(rating.lm)

size.lm <- lm(formula = Installs.log~Size, data = usefulPApps2)
summary(size.lm)

par(mfrow = c(2, 2))
plot(Installs.log~Price, data=usefulPApps2, 
     ylab = "Installs (log)", xlab = "Price", col = Category)
abline(price.lm, lwd = 2, col = "pink")
plot(Installs.log~Reviews, data=usefulPApps2, 
     ylab = "Installs (log)", xlab = "Reviews", col = Category)
abline(reviews.lm, lwd = 2, col = "pink")
plot(Installs.log~Rating, data=usefulPApps2, 
     ylab = "Installs (log)", xlab = "Rating", col = Category)
abline(rating.lm, lwd = 2, col = "pink")
plot(Installs.log~Size, data=usefulPApps2, 
     ylab = "Installs (log)", xlab = "Size", col = Category)
abline(size.lm, lwd = 2, col = "pink")


## ----Linear Correlation, cache=TRUE--------------------------------------
Corr_vals <-c(cor(x = usefulPApps2$Price, y = usefulPApps2$Installs.log), 
              cor(x = usefulPApps2$Reviews, y = usefulPApps2$Installs.log), 
              cor(x = usefulPApps2$Rating, y = usefulPApps2$Installs.log),
              cor(x = usefulPApps2$Size, y = usefulPApps2$Installs.log))
R2_vals <- Corr_vals^2
val <- data.frame(Price = c(Corr_vals[1], R2_vals[1]), 
                  Reviews = c(Corr_vals[2], R2_vals[2]), 
                  Rating = c(Corr_vals[3], R2_vals[3]), 
                  Size = c(Corr_vals[4], R2_vals[4]))
row.names(val)<- c("Correlation", "R ^2 Values")
kable(val, format = "html")  %>%
  kable_styling( bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>% 
  column_spec(column = 1, width_min = "10em", border_right = T, bold = T) %>%
  scroll_box(fixed_thead = T, width = "800px")


## ----Transformations, cache=TRUE-----------------------------------------
apps_model_a = lm(Installs ~ Price + Rating + Reviews + Size + Category,
                data = usefulPApps2)
apps_model_b = lm(Installs ~ (Price + Rating + Reviews + Size) * Category,
                data = usefulPApps2)
summary(apps_model_a)
summary(apps_model_b)
boxcox(apps_model_a, lambda = seq(-0.25, 0.25, by = 0.01), plotit = TRUE)



## ----Success Column, cache = TRUE----------------------------------------
usefulPApps2$Success[usefulPApps2$Installs>=5e4]<-1
usefulPApps2$Success[usefulPApps2$Installs<5e4]<-0

sum(usefulPApps2[,'Success']==1)
sum(usefulPApps2[,'Success']==0)


## ---- cache = TRUE-------------------------------------------------------
p.x <- mean(usefulPApps2$Success)
p.x


## ----Price GLM, cache=TRUE-----------------------------------------------
price.glm<-glm(Success ~ Price, data = usefulPApps2, family = binomial(link = "logit"))
price.glm2<-glm(Success ~ poly(Price, degree = 2), 
                data = usefulPApps2, family = binomial(link = "logit"))
price.glm3<-glm(Success ~ poly(Price, degree = 3), 
                data = usefulPApps2, family = binomial(link = "logit"))
price.lm2<-lm(Success ~ Price, data = usefulPApps2)
summary(price.lm2)
summary(price.glm)
summary(price.glm2)
summary(price.glm3)
plot(jitter(Success, factor = 0.1) ~ Price, data = usefulPApps2, 
     pch = 20, ylab = "Successful Apps", 
     main = "Success vs. Price")
abline(price.lm2, col = "darkorange")
abline(h = 0.5, col = "pink")
curve(predict(price.glm, newdata = data.frame(Price=x), type='response'), 
      add = TRUE, col = "dodgerblue", lty = 2)
curve(predict(price.glm2, newdata = data.frame(Price=x), type='response'),
      add = TRUE, col = "darkred", lty = 2)
curve(predict(price.glm3, newdata = data.frame(Price=x), type='response'),
      add = TRUE, col = "darkgreen", lty = 2)
legend("topright", c("Linear", "Logistic","Logistic (x^2)","Logistic (x^3)", "Data"), 
       lty = c(1, 2, 3, 4, 0), 
       pch = c(NA, NA, NA, NA, 20), lwd = 2, 
       col = c("darkorange", "dodgerblue","darkred","darkgreen","black"),
       bty = "n")


## ----Rating GLM, cache=TRUE----------------------------------------------
Rating.glm<-glm(Success ~ Rating, data = usefulPApps2, family = binomial(link = "logit"))
Rating.glm2<-glm(Success ~ poly(Rating, degree = 2), 
                 data = usefulPApps2, family = binomial(link = "logit"))
Rating.glm3<-glm(Success ~ poly(Rating, degree = 3), 
                 data = usefulPApps2, family = binomial(link = "logit"))
Rating.lm2<-lm(Success ~ Rating, data = usefulPApps2)
summary(Rating.lm2)
summary(Rating.glm)
summary(Rating.glm2)
summary(Rating.glm3)
plot(jitter(Success, factor = 0.1) ~ Rating, data = usefulPApps2, 
     pch = 20, ylab = "Successful Apps", 
     main = "Success vs. Rating")
abline(Rating.lm2, col = "darkorange")
abline(h = 0.5, col = "pink")
curve(predict(Rating.glm, newdata = data.frame(Rating=x), type='response'), 
      add = TRUE, col = "dodgerblue", lty = 2)
curve(predict(Rating.glm2, newdata = data.frame(Rating=x), type='response'), 
      add = TRUE, col = "darkred", lty = 2)
curve(predict(Rating.glm3, newdata = data.frame(Rating=x), type='response'), 
      add = TRUE, col = "darkgreen", lty = 2)
legend("topleft", c("Linear", "Logistic","Logistic (x^2)","Logistic (x^3)", "Data"), 
       lty = c(1, 2, 3, 4, 0), 
       pch = c(NA, NA, NA, NA, 20), lwd = 2, 
       col = c("darkorange", "dodgerblue","darkred","darkgreen","black"), 
       bty = "n")


## ----Reviews GLM, cache=TRUE---------------------------------------------
Reviews.glm<-glm(Success ~ Reviews, data = usefulPApps2, family = binomial(link = "logit"))
Reviews.glm2<-glm(Success ~ poly(Reviews, degree = 2), 
                  data = usefulPApps2, family = binomial(link = "logit"))
Reviews.glm3<-glm(Success ~ poly(Reviews, degree = 3), 
                  data = usefulPApps2, family = binomial(link = "logit"))
Reviews.lm2<-lm(Success ~ Reviews, data = usefulPApps2)
summary(Reviews.glm)
summary(Reviews.glm2)
summary(Reviews.glm3)
plot(jitter(Success, factor = 0.1)~ Reviews, data = usefulPApps2, 
     pch = 20, ylab = "Successful Apps", 
     main = "Success vs. Reviews")
abline(Reviews.lm2, col = "darkorange")
abline(h = 0.5, col = "pink")
curve(predict(Reviews.glm, newdata = data.frame(Reviews=x), type='response'), 
      add = TRUE, col = "dodgerblue", lty = 2)
curve(predict(Reviews.glm2, newdata = data.frame(Reviews=x), type='response'), 
      add = TRUE, col = "darkred", lty = 2)
curve(predict(Reviews.glm3, newdata = data.frame(Reviews=x), type='response'), 
      add = TRUE, col = "darkgreen", lty = 2)
legend("topleft", c("Linear", "Logistic","Logistic (x^2)","Logistic (x^3)", "Data"), 
       lty = c(1, 2, 3, 4, 0), 
       pch = c(NA, NA, NA, NA, 20), lwd = 2, 
       col = c("darkorange", "dodgerblue","darkred","darkgreen","black"),
       bty ="n")


## ----Size GLM, cache=TRUE------------------------------------------------
Size.glm<-glm(Success ~ Size, data = usefulPApps2, family = binomial(link = "logit"))
Size.glm2<-glm(Success ~ poly(Size, degree = 2), 
                  data = usefulPApps2, family = binomial(link = "logit"))
Size.glm3<-glm(Success ~ poly(Size, degree = 3), 
                  data = usefulPApps2, family = binomial(link = "logit"))
Size.lm2<-lm(Success ~ Size, data = usefulPApps2)
summary(Size.glm)
summary(Size.glm2)
summary(Size.glm3)
plot(jitter(Success, factor = 0.1)~ Size, data = usefulPApps2, 
     pch = 20, ylab = "Successful Apps", 
     main = "Success vs. Size")
abline(Size.lm2, col = "darkorange")
abline(h = 0.5, col = "pink")
curve(predict(Size.glm, newdata = data.frame(Size=x), type='response'), 
      add = TRUE, col = "dodgerblue", lty = 2)
curve(predict(Size.glm2, newdata = data.frame(Size=x), type='response'), 
      add = TRUE, col = "darkred", lty = 2)
curve(predict(Size.glm3, newdata = data.frame(Size=x), type='response'), 
      add = TRUE, col = "darkgreen", lty = 2)
legend("left", c("Linear", "Logistic","Logistic (x^2)","Logistic (x^3)", "Data"), 
       lty = c(1, 2, 3, 4, 0), 
       pch = c(NA, NA, NA, NA, 20), lwd = 2, 
       col = c("darkorange", "dodgerblue","darkred","darkgreen","black"),
       bty ="n")


## ----AIC of GLM, cache = TRUE--------------------------------------------
AIC_data <- data.frame(GLM = c("x", "x+x^2", "x+x^2+x^3"),Price = c(price.glm$aic, price.glm2$aic, price.glm3$aic), 
                       Reviews = c(Reviews.glm$aic, Reviews.glm2$aic, Reviews.glm3$aic),
                       Rating = c(Rating.glm$aic, Rating.glm2$aic, Rating.glm3$aic),
                       Size = c(Size.glm$aic, Size.glm2$aic, Size.glm3$aic))

kable(AIC_data, format = "html")  %>%
  kable_styling( bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>% 
  column_spec(column = 1, width_min = "10em", border_right = T, bold = T) %>%
  scroll_box(fixed_thead = T, width = "800px")


## ---- cache=TRUE---------------------------------------------------------
PRRS.glm<-glm(Success ~ Category + Price + Rating + Reviews + Size, 
            data = usefulPApps2)
summary(PRRS.glm)$aic
PRRS.glm2<-glm(Success ~ Category * (Price + Rating + Reviews + Size), 
              data = usefulPApps2, 
              family = binomial(link = "logit"))
summary(PRRS.glm2)$aic


## ----Seperating Train and Test, cache = TRUE-----------------------------
set.seed(6)
train.apps <- usefulPApps2[sample(nrow(usefulPApps2), nrow(usefulPApps2)*0.7), ]
test.apps  <- usefulPApps2[!(usefulPApps2$App%in%train.apps$App),]


## ---- cache = TRUE-------------------------------------------------------
kable(t(summary(train.apps)), format = "html")  %>%
  kable_styling( bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>% 
  column_spec(column = 1, width_min = "10em", border_right = T, bold = T) %>%
  scroll_box(fixed_thead = T, width = "800px", height = "300px")


## ---- cache = TRUE-------------------------------------------------------
kable(t(summary(test.apps)), format = "html")  %>%
  kable_styling( bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>% 
  column_spec(column = 1, width_min = "10em", border_right = T, bold = T) %>%
  scroll_box(fixed_thead = T, width = "800px", height = "300px")


## ----Random Forest 1, cache = TRUE---------------------------------------
set.seed(6)
# Train the model with a grid search of 10 folders
trControl <- trainControl(method = "cv",
    number = 10,
    search = "grid")
RFmodel1 <- train(as.factor(Success)~Category+Rating+Size+Reviews+Price, 
                  data = train.apps,
                  method='rf', 
                  metric='Accuracy')
RFmodel1


## ----Optimizing mtry, cache = TRUE---------------------------------------
set.seed(6)
tuneGrid <- expand.grid(.mtry = c(1:8))
RFmodel2 <- train(as.factor(Success)~as.factor(Category)+Rating+Size+Reviews+Price, 
                  data = train.apps,
                  method='rf', 
                  metric='Accuracy',
                  tuneGrid = tuneGrid,
                  trControl = trControl,
                  importance = TRUE,
                  nodesize = 14,
                  ntree = 300)
RFmodel2
bestmtry <- RFmodel2$bestTune$mtry
bestmtry


## ----Optimizing Max Nodes, cache = TRUE----------------------------------
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = bestmtry)
for (maxnodes in c(10:30)){
  set.seed(6)
  RFmodel2_maxn <- train(as.factor(Success)~as.factor(Category)+Rating+Size+Reviews+Price, 
                  data = train.apps,
                  method='rf', 
                  metric='Accuracy',
                  tuneGrid = tuneGrid,
                  trControl = trControl,
                  importance = TRUE,
                  nodesize = 14,
                  maxnodes = maxnodes,
                  ntree = 300)
  current_it<- toString(maxnodes)
  store_maxnode[[current_it]]<- RFmodel2_maxn
}
resultsmaxnode <- resamples(store_maxnode)
summary(resultsmaxnode)


## ----Optimizing ntrees, cache = TRUE-------------------------------------
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)){
  set.seed(8)
  RFmodel2_maxtree <- train(as.factor(Success)~as.factor(Category)+Rating+Size+Reviews+Price, 
                  data = train.apps,
                  method='rf', 
                  metric='Accuracy',
                  tuneGrid = tuneGrid,
                  trControl = trControl,
                  importance = TRUE,
                  nodesize = 14,
                  maxnodes = 20,
                  ntree = ntree)
  key<- toString(ntree)
  store_maxtrees[[key]]<- RFmodel2_maxtree
}
resultstree <- resamples(store_maxtrees)
summary(resultstree)
max(summary(resultstree)$values)


## ----Random Forest 2, cache = TRUE---------------------------------------
set.seed(10)
RFmodel_opt <- train(as.factor(Success)~as.factor(Category)+Rating+Size+Reviews+Price, 
                  data = train.apps,
                  method='rf', 
                  metric='Accuracy',
                  tuneGrid = tuneGrid,
                  trControl = trControl,
                  importance = TRUE,
                  nodesize = 14,
                  maxnodes = 20,
                  ntree = 600)
RFmodel_opt


## ---- cache = TRUE-------------------------------------------------------
set.seed(16)
# Predictions on the Training Data
train.apps$Prediction1 <-predict(RFmodel_opt, train.apps)
kable(train.apps[sample(nrow(train.apps), size = 10),
                 c('App','Success','Prediction1')], format = "html")  %>%
  kable_styling( bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>% 
  column_spec(column = 1, width_min = "10em", border_right = T, bold = T) %>%
  scroll_box(fixed_thead = T, width = "800px", height = "300px")
train_tab = table(predicted = train.apps$Prediction1, actual = as.factor(train.apps$Success))
confusionMatrix(train_tab, positive = "1")


## ---- cache=TRUE---------------------------------------------------------
#Predictions on the Test Data
test.apps$Prediction1 <-predict(RFmodel_opt, test.apps)
kable(test.apps[sample(nrow(test.apps), size = 10),
                 c('App','Success','Prediction1')], format = "html")  %>%
  kable_styling( bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>% 
  column_spec(column = 1, width_min = "10em", border_right = T, bold = T) %>%
  scroll_box(fixed_thead = T, width = "800px", height = "300px")
test_tab = table(predicted = test.apps$Prediction1, actual = as.factor(test.apps$Success))
confusionMatrix(test_tab, positive = "1")

