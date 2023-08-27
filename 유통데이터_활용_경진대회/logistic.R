library(ggplot2)
library(tidyverse)
library(wordcloud2)
library(RColorBrewer) 
library(extrafont) 
library(dplyr) 
library(tm) 
library(devtools)  
library(showtext)
library(dplyr)
library(TSstudio)
library(rJava)
library(KoNLP)


font_import()
theme_set(theme_grey(base_family = 'AppleGothic'))

font_add("nanum","NanumGothic.ttf")
showtext_auto()


purchase <- read.csv("purchase_transaction_data.csv")
View(purchase)


str(purchase)

head(purchase,4)

tail(purchase,4)


purchase$PurchaseDate <- as.Date(purchase$PurchaseDate)
str(purchase)

table(purchase$Agegroup)

purchase$Agedivide <- case_when(purchase$Agegroup >= 20 & purchase$Agegroup <= 29 ~ "20~29",
                                purchase$Agegroup >= 30 & purchase$Agegroup <= 39 ~ "30~39",
                                purchase$Agegroup >= 40 & purchase$Agegroup <= 49 ~ "40~49",
                                purchase$Agegroup >= 50 & purchase$Agegroup <= 59 ~ "50~59",
                                purchase$Agegroup >= 60 ~ "60~69" )

View(purchase$Agegroup)




value_counts <-table(purchase$Agedivide, purchase$Gender)
value_counts_df <- as.data.frame(value_counts)

colnames(value_counts_df) <- c("AgeDivided", "Gender", "Count")


ggplot(value_counts_df, aes(x = AgeDivided, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "상품구매 연령그룹 및 성별", x = "연령별", y = " 성별 값") +
  theme_minimal() + theme_light(base_family = "nanum")+
  theme_classic()+
  scale_fill_manual(values = c("F" = "pink","M"="skyblue"))+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 17))



# pie chart


gender_counts <-table(purchase$Gender)
gender_counts_df <- data.frame(Category = names(gender_counts),Count = gender_counts)


gender_total <-sum(gender_counts_df$Count.Freq)
gender_counts_df$Percentage <- (gender_counts_df$Count.Freq /gender_total) * 100


View(gender_counts_df)




ggplot(gender_counts_df, aes(x = "", y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 1)+ 
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),colour = "white",
            position = position_stack(vjust = 0.5),size = 6)+
  scale_fill_discrete(guide = guide_legend(title = "성별"))+
  scale_fill_manual(values = c("orange", "darkgreen"))+
  ggtitle("성별 상품구매 비율")+
  theme(text = element_text(family = "AppleGothic"), 
        title = element_text(size = 16, face = 2, hjust = 0.5))
  
  


retail_counts <-table(purchase$Retailer)
retail_counts_df <- data.frame(Category = names(retail_counts ),
                               Count = retail_counts )


retail_total <-sum(retail_counts_df$Count.Freq)
retail_counts_df$Percentage <- 
  (retail_counts_df$Count.Freq /retail_total) * 100


ggplot(retail_counts_df, aes(x = "", y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 1)+ 
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),colour = "white",
            position = position_stack(vjust = 0.5),size = 6)+
  scale_fill_discrete(guide = guide_legend(title = "성별"))+
  scale_fill_manual(values = c("orange", "darkblue"))+
  ggtitle("리테일러 비율")+
  theme(text = element_text(family = "AppleGothic"), 
        title = element_text(size = 16, face = 2, hjust = 0.5))


write.csv(purchase, file = "fixed_purchase.csv")
)





