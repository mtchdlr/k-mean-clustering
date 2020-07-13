library(dplyr)
library(ggplot2)
library(gridExtra)
library(data.table)
library(plotly)
library(tm) # package for text mining
library(wordcloud) # for word visualization
library(ape) # dendrogram plotting
library(ggdendro) # 
orders=read.csv('orders_rfm.csv', header = T)
reporting_date=as.Date('2017-03-10',format='%Y-%m-%d')
reporting_date
str(orders)
orders$order_date=as.Date(orders$order_date, format='%Y-%m-%d')
orders_filter=filter(orders, order_date==reporting_date)
length(unique(orders_filter$client_id))
table(orders$product)

frm_tbl_initial=orders%>%
  group_by(client_id)%>%
  summarise(order_frequency=n(), order_recency=min(reporting_date-order_date), order_monetary=sum(money_spent))

head(frm_tbl_initial)
class(frm_tbl_initial$order_recency)
frm_tbl_initial$order_recency=as.numeric(frm_tbl_initial$order_recency)

ggplot(frm_tbl_initial, aes(x=order_recency))+
  geom_histogram(fill='#8b3840', color='grey60', binwidth = 1)+
  theme_bw()

ggplot(frm_tbl_initial, aes(x=order_frequency))+
  geom_histogram(fill='#8b3840', color='grey60', binwidth = 1)+
  theme_bw()

ggplot(frm_tbl_initial, aes(x=order_monetary))+
  geom_histogram(fill='#8b3840', color='grey60', binwidth =50)+
  theme_bw()
summary(frm_tbl_initial)

fr_tbl=mutate(frm_tbl_initial, frequency_bins=cut(order_frequency, breaks = c(0,5,8,10,13,32)))
table(fr_tbl$frequency_bins)

fr_tbl=mutate(fr_tbl,recency_bins=cut(order_recency, breaks = c(-33,-20, -4, 3, 21,67)))
table(fr_tbl$recency_bins)

fr_tbl=mutate(fr_tbl, monetary_bins=cut(order_monetary, breaks = c(4,467,826,943, 1269,3493)))
table(fr_tbl$monetary_bins)

table(frequency=fr_tbl$frequency_bins, recency=fr_tbl$recency_bins)
table(frequency=fr_tbl$frequency_bins, recency=fr_tbl$recency_bins, monetary=fr_tbl$monetary_bins)

fr_tbl$frequency_bins=factor(fr_tbl$frequency_bins, levels = rev(levels(fr_tbl$frequency_bins)))
fr_tbl$monetary_bins=factor(fr_tbl$monetary_bins, levels = rev(levels(fr_tbl$monetary_bins)))

table(frequency=fr_tbl$frequency_bins, recency=fr_tbl$recency_bins)
table(frequency=fr_tbl$frequency_bins, recency=fr_tbl$recency_bins, monetary=fr_tbl$monetary_bins)

fr_tbl_count=fr_tbl%>%
  group_by(frequency_bins, recency_bins)%>%
  summarise(count=n())

p_basic=ggplot(fr_tbl_count, aes(x=recency_bins, y=frequency_bins))+
  geom_tile(aes(fill=count))+
  geom_text(aes(label=count))+
  scale_fill_gradient(low = '#f0f0f0',high = '#636363')+
  theme_bw(base_size = 20)
ggplotly(p_basic)

p_quadrants=p_basic+
  ggplot2::annotate("rect", xmin = 0, xmax=3.47, ymin = 3.47, ymax = 6,color='green',alpha=0.1, fill='green')+
  ggplot2::annotate("rect", xmin = 0, xmax=3.47, ymin = 2.5, ymax = 3.47,color='yellow',alpha=0.1, fill='yellow')+
  ggplot2::annotate("rect", xmin = 0, xmax=3.47, ymin = 0, ymax = 2.5,color='blue',alpha=0.1, fill='blue')+
  ggplot2::annotate("rect", xmin = 3.5, xmax=6, ymin = 3.47, ymax = 6,color='red',alpha=0.1, fill='red')+
  ggplot2::annotate("rect", xmin = 3.5, xmax=6, ymin = 0, ymax = 3.47,color='black',alpha=0.1, fill='black')
  
  
p_quadrants

p_quadrants + 
  ggplot2::annotate("text", x=1.8, y=5.8, label='New') +
  ggplot2::annotate("text", x=4.8, y=5.8, label='Lost') +
  ggplot2::annotate("text", x=1.4, y=2.7, label='Promising') +
  ggplot2::annotate("text", x=1.8, y=0.2, label='Loyal customers') +
  ggplot2::annotate("text", x=4.8, y=0.2, label='Hibernating loyal customers')


##numeric approach for RFM

fr_tbl_score=fr_tbl%>%
  mutate(f_score=as.numeric(frequency_bins), r_score=as.numeric(recency_bins), m_score=as.numeric(monetary_bins))%>%
  mutate(RFM_score=paste(f_score, r_score, m_score, sep = ''))

fr_tbl_score%>%
  arrange(RFM_score)%>%
  View()


##k-means clustering

data_clustering=frm_tbl_initial%>%
  mutate(order_frequency=scale(order_frequency), order_recency=scale(order_recency), order_monetary=scale(order_monetary))
head(data_clustering)

?kmeans
clusters=kmeans(data_clustering[,-1], centers = 4, nstart = 20)
clusters

data_clustering$cluster=as.factor(clusters$cluster)
ggplot(data_clustering, aes(x=order_frequency, y=order_recency, color=cluster)) + geom_point(size=2) + theme_bw()

p1 <- ggplot(data_clustering, aes(x=order_frequency, y=order_recency, color=cluster)) + geom_point(size=2) + theme_bw()
p2 <- ggplot(data_clustering, aes(x=order_frequency, y=order_monetary, color=cluster)) + geom_point(size=2) + theme_bw()
p3 <- ggplot(data_clustering, aes(x=order_monetary, y=order_recency, color=cluster)) + geom_point(size=2) + theme_bw()
grid.arrange(p1,p2,p3, nrow=3)



elbow_method <- function(data, max_k=15){
  require(ggplot2)
  wss <- 0
  for (i in 1:max_k){
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }
  p <- data.frame(number_of_clusters=c(1:max_k), wss=wss) %>%
    ggplot(aes(x=number_of_clusters, y=wss)) + geom_point() + 
    geom_line() + theme_bw() + ylab("Within groups sum of squares")
  return(print(p))
}

elbow_method(data_clustering[,1], max_k=15)


clusters_8=kmeans(data_clustering[,-1], centers = 8, nstart = 20)
clusters

data_clustering$cluster=as.factor(clusters_8$cluster)
ggplot(data_clustering, aes(x=order_frequency, y=order_recency, color=cluster)) + geom_point(size=2) + theme_bw()



##real life data

dt_orders=fread('orders.csv')
dt_products=fread('order_products__prior.csv')
product_names=fread('products.csv')


dt_products=left_join(dt_products, product_names[,c(1, 2)], by='product_id')
dt_full=left_join(dt_products, dt_orders, by='order_id')


##
##how many times each product was bought
##how many unique users bought it
##in how many different orders this product appears
##most frequent day of week
##most frequent hour of the day
##what is the average number of day since the last order
##most frequent order of adding it to the cart
##how frequently it was bought before


##the most frequent function
freq_value <- function(x) {
  ux <- unique(x)
  ux[which.max(
    tabulate(match(x, ux)) # number of times each item occurs
  )]
}

features=dt_full%>%
  group_by(product_id)%>%
  summarise(count=n(),unique_users=length(unique(user_id)), unique_orders=length(unique(order_id)), freq_ord_dow=freq_value(order_dow), freq_hour=freq_value(order_hour_of_day), avg_daus_prior=mean(days_since_prior_order, na.rm=T), freq_add_to_cart=freq_value(add_to_cart_order), reordered_n=sum(reordered))
View(features)

##missing data problem

filter(features, is.na(product_id)==F)
##no NAs

scaled_features=mutate_at(features, vars(-product_id), scale)
View(scaled_features)


elbow_method <- function(data, max_k=15){
  require(ggplot2)
  wss <- 0
  for (i in 1:max_k){
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }
  p <- data.frame(number_of_clusters=c(1:max_k), wss=wss) %>%
    ggplot(aes(x=number_of_clusters, y=wss)) + geom_point() + 
    geom_line() + theme_bw() + ylab("Within groups sum of squares")
  return(print(p))
}

elbow_method(scaled_features, max_k = 30)

filter(features, is.na(avg_days_prior)==F)
scaled_features=na.omit(scaled_features)
features_with_cluster=na.omit(features)
features_with_cluster$cluster=as.factor(clusters$cluster)
features_with_cluster <- left_join(features_with_cluster, product_names[,c(1,2)], by="product_id") # to see what are the product names


ggplot(features_with_cluster, aes(x=count, y=unique_users, color=cluster)) + geom_point() + theme_bw() + scale_x_log10() + scale_y_log10()

ggplot(features_with_cluster, aes(x=freq_ord_dow)) + geom_density() + facet_grid(~cluster)

corpus=Corpus(VectorSource(filter(features_with_cluster, cluster==2)$product_name))
tdm=TermDocumentMatrix(corpus)
m=as.matrix(tdm)
v=sort(rowSums(m), decreasing = T)
d=data.frame(word=names(v), freq=v)
wordcloud(d$word, d$freq, min.freq = 2, max.words = 1000)
