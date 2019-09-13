rm(list=ls())

require(dplyr)
require(foreign)
require(ggplot2)
library(tidyr)
library(tree)

miller_type <-read.csv('typecsv.csv')
miller_type <- miller_type %>%
  transmute(
    desc,
    type
  )
miller_type <-  unique(miller_type)
rawdata <- read.csv('beer.csv') # all kinds of beer in each store by week data
upc <- read.csv('beer_upc.csv') # types of beer
store <- read.dta("demo.dta") # store info
ccount <- read.dta("ccount.dta")
ccount <- drop_na(ccount)
ccount<-select(ccount,c('store','date','beer'))
ccount <- ccount %>% group_by(store) %>% 
  transmute(
    mean_beer = mean(beer)
  ) 
ccount = unique(ccount)

colnames(upc) <- c("code", "upc", "desc", "size", "case", "nitem")

colnames(rawdata) <- c("store", "upc", "week", "move",
                       "qty", "price", "prom", "profit",
                       "valid", "priceHex", "profitHex")
movement <- rawdata %>% 
  filter(valid == 1) %>% 
  select(-c("valid", "priceHex","profitHex","profit")) %>% 
  mutate(
    sales = price * move / qty,  # or by move / qty
    upc_firm  = substr(upc, start = 1, stop = 3)
  )
movement$prom <- as.character(movement$prom)
movement$prom[movement$prom == ''] <- 'N'
movement$prom <- as.factor(movement$prom)
store <- store %>% select(c("name", "age9",'zone', "store", "age60", "ethnic", "educ", "nocar", "income"))
store<- store %>%
  filter(
    name != ""
  )


# add upc code by firm 
upc <- upc%>% 
  select(-c('code','nitem'))


miller <- movement[(movement$upc_firm =='341')|
                     (movement$upc %in% c('731','732','735')),]
miller <- miller %>%
  filter(
    price != 0
  )
# miller merge with upc
miller <- merge( miller, upc ,by = 'upc', all.x = TRUE)

# miller merge with total sales in beeer
miller <- merge( miller, ccount ,by = 'store', all.x = TRUE)

# miller merge with store nearby info
miller <- merge( miller, store ,by = 'store', all.x = TRUE)



# convert size 
miller$size <- as.character(miller$size)

miller <-  miller %>%
  mutate(
    size_class = case_when(
      "/" %in% strsplit(size,'')[[1]] ~ substr(
        size, start = 1, stop = regexpr("/",size)-1)
    )
  )
miller <- miller %>%
  mutate(
    holiday = case_when(
      week %in% c(112,116,119,120,
                  164,168,172,173,216,220,224,225
                  ,268,272,276,277
                  ,320,324,328,329,372,377,
                  380,381,389,393) ~ 1,
      !week %in% c(112,116,119,120,
                   164,168,172,173,216,220,224,225
                   ,268,272,276,277
                   ,320,324,328,329,372,377,
                   380,381,389,393) ~ 0 )
  )



miller$size_class = as.integer(miller$size_class)
miller$size_class[is.na(miller$size_class) ] = 1

miller$zone = as.factor(miller$zone)

miller <-  miller %>%
  mutate(
    total_num = move*size_class
  )

miller$size_class = as.factor(miller$size_class)

miller$prom = as.factor(miller$prom)

#add type
miller = merge(miller, miller_type, by= 'desc', all.x = TRUE )
miller$type = as.factor(miller$type)



# add bud
bud <- movement[movement$upc_firm == '182',]
bud <- bud %>% select(-c(upc_firm))
bud <- bud %>% select(c("store", "week", "move")) %>% group_by(store, week) %>% summarise(rival = sum(move))
miller <- merge(miller,bud,by = c('store', 'week'), all.x =TRUE)


miller <- drop_na(miller)


# test tree
test <- tree(total_num ~prom+ type + holiday+ zone+
               income + nocar +educ+ size_class,miller  
)
summary(test)
plot(test)
text(test,pretty = 0, cex=0.8)

#Create the random forest
library(randomForest)
set.seed(101)


miller$prom  = as.factor(miller$prom)
miller$zone=as.factor(miller$zone)
miller$size_class=as.factor(miller$size_class)
miller$holiday=as.factor(miller$holiday)


best5 = data.frame('desc' = c('MILLER LITE BEER','MILLER GENUINE DRAFT','MILLER GENUINE DRFT',
                              'MILLER LITE BEER N.R','MILLER GEN DRFT LNNR'),
                   'best_type' = 1:5)

miller_5 = miller[miller$desc == 'MILLER LITE BEER'|
                    miller$desc == 'MILLER GENUINE DRAFT'|
                    miller$desc =='MILLER GENUINE DRFT'|
                    miller$desc =='MILLER LITE BEER N.R'|
                    miller$desc =='MILLER GEN DRFT LNNR',]
miller_5 = merge(miller_5, best5, by= 'desc', all.x = TRUE)

## best 5 with prom B ##
miller_5B = miller_5[miller_5$prom == 'B',]

train_5B = sample(1:nrow(miller_5B),0.7*nrow(miller_5B))

rf5B = randomForest(total_num ~size_class+best_type+holiday+zone+ educ+age9+income ,
                    miller_5B, mtry =4, subset = train_5B,ntree = 100)
rf5B
importance(rf5B)
plot(rf5B)

pred5b<-predict(rf5B,miller_5B[-train_5B,]) 
test_5b= with(miller_5B[-train_5B,], mean( (total_num - pred5b)^2)) #MSE

## best 5 with prom S  ##
miller_5S = miller_5[miller_5$prom == 'S',]

train_5S = sample(1:nrow(miller_5S),0.7*nrow(miller_5S))


rf5S = randomForest(total_num ~size_class+best_type+holiday+zone+ educ+age9+income ,
                    miller_5S, mtry =3, subset = train_5S,ntree = 500)

rf5S
importance(rf5S)
plot(rf5S)

pred5s<-predict(rf5S,miller_5S[-train_5S,]) 
test_5s= with(miller_5S[-train_5S,], mean( (total_num - pred5s)^2)) #Mean Squared Test Error
test_5s

## predict 5 samples with different packages (size_class)
sample = read.csv('test.csv')

sample$holiday = as.factor(sample$holiday)
sample$zone = as.factor(sample$zone)
sample$best_type = as.factor(sample$best_type)
sample$size_class = as.factor(sample$size_class)

levels(sample$zone) <- levels(miller_5S$zone) 
levels(sample$best_type) <- levels(miller_5S$best_type)
levels(sample$size_class) <- levels(miller_5S$size_class)

preds<-predict(rf5S,sample) 
preds

pred5b<-predict(rf5B,sample) 
pred5b - preds

### Random forest for non-promotion type

miller<-left_join(miller,best5,by='desc')
miller$best_type[is.na(miller$best_type)]<-6
miller$best_type<-factor(miller$best_type,levels = c(1,2,3,4,5,6))

N<-miller%>%filter(prom=='N')
N<-drop_na(N)
train<-sample(1:nrow(N),0.2*nrow(N))

#Find out the best mtry for random forest
oob.err = double(7)
test.err = double(7)
for(mtry in 2:7){
  rf_N = randomForest(total_num~zone+best_type+income+price+age9+educ+holiday+size_class, 
                      data = N, subset=train, mtry=mtry, 
                      ntree = 100)
  oob.err[mtry] = rf_N$mse[7]
  pred = predict(rf_N, N[-train,])
  test.err[mtry] = with(N[-train,], mean( (total_num-pred)^2 ))
}
matplot(1:mtry, cbind(test.err, oob.err), pch = 23, col = c("red", "blue"), type = "b", ylab="Mean Squared Error")
legend("topright", legend = c("OOB", "Test"), pch = 23, col = c("red", "blue"))

#Train the model
rf_N =randomForest(total_num~zone+rank+income+age9+educ+holiday+size_class, 
                   data = N, subset=train, mtry=4, 
                   ntree = 100 )
rf_N

save(rf_N,file='rf_N.RData')

#predict 5 samples for non-promotion
sample$size_class<-as.factor(sample$size_class)
sample$holiday<-as.factor(sample$holiday)
sample$zone<-as.factor(sample$zone)
sample$best_type<-as.factor(sample$best_type)
levels(sample$holiday)=levels(N$holiday)
levels(sample$zone)=levels(N$zone)
levels(sample$size_class)=levels(N$size_class)
levels(sample$best_type)=levels(N$best_type)

N_pred<-predict(rf_N,sample)
sample<-cbind(sample,N_pred)