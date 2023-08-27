library(ggplot2) # 데이터 시각화 
library(wordcloud2)# 워드클라우드 
library(RColorBrewer) # 워드클라우드 색조 
library(extrafont) # 폰트 생성 
library(dplyr) # 데이터 전처리 
library(tm) # 텍스트 마이닝 
library(devtools) # github 연동하기 


# wordcloud2 생성하기 위해 devtools를 활용하여 github 연동하기 
devtools::install_github("lchiffon/wordcloud2",force = TRUE)



# AppleGothic으로 폰트 생성 
font_import()
theme_set(theme_grey(base_family = 'AppleGothic'))


# 디즈니리랜드 리뷰에 대한 데이터셋 불러오기 
disney <- read.csv("disneylandreviews.csv")


# 데이터 속성 보기 
str(disney)

#결측치 확인 
sum(is.na(disney))


# 전체 데이터셋 보기 
View(disney)


# 각 Branch에 속하는 데이터 나누기 
hongkong <- disney[disney$Branch == "Disneyland_HongKong",]
paris <- disney[disney$Branch == "Disneyland_Paris",]
california <- disney[disney$Branch == "Disneyland_California",]


# 히스토그램 
# 히스토그램 생성 
b1 <- ggplot(hongkong, aes(x=hongkong$Rating)) + 
  geom_histogram(binwidth = 1, color="skyblue4", fill="lightblue")+
  labs(title="디즈니랜드 평점(홍콩)",x="평점", y = "리뷰 수")+
  theme(plot.title = element_text(hjust = 0.5))

b2 <-ggplot(paris, aes(x=paris$Rating)) + 
  geom_histogram(binwidth = 1, color="black", fill="orange")+
  labs(title="디즈니랜드 평점(파리)",x="평점", y = "리뷰수 ")+
  theme(plot.title = element_text(hjust = 0.5))

b3 <-ggplot(california, aes(x=california$Rating)) + 
  geom_histogram(binwidth = 1, color="darkgreen", fill="lightgreen")+
  labs(title="디즈니랜드 평점(캘리포니아)",x="평점", y = "리뷰 수")+
  theme(plot.title = element_text(hjust = 0.5))


b1 + theme_classic(base_family = "AppleGothic")+ theme(plot.title = element_text(hjust = 0.5))
b2 + theme_classic(base_family = "AppleGothic")+ theme(plot.title = element_text(hjust = 0.5))
b3 + theme_classic(base_family = "AppleGothic")+ theme(plot.title = element_text(hjust = 0.5))





 
# 막대그래프
# n으로 정렬하기 
hong2 <-hongkong%>% count(Reviewer_Location)%>%arrange(desc(n))
paris2 <-paris%>% count(Reviewer_Location)%>%arrange(desc(n))
cali2 <-california%>% count(Reviewer_Location)%>%arrange(desc(n))


# 1부터 5까지 데이터를 가져오기 
final_hong <-hong2[c(1:5),]
final_paris <-paris2[c(1:5),]
final_cali <- cali2[c(1:5),]


# factor를 생성하고 n으로 정렬하기
final_hong$Reviewer_Location <- factor(final_hong$Reviewer_Location,
                                       levels = final_hong$Reviewer_Location
                                       [order(final_hong$n,decreasing = TRUE)])


final_paris$Reviewer_Location <- factor(final_paris$Reviewer_Location,
                                       levels = final_paris$Reviewer_Location
                                       [order(final_paris$n,decreasing = TRUE)])


final_cali$Reviewer_Location <- factor(final_cali$Reviewer_Location,
                                       levels = final_cali$Reviewer_Location
                                       [order(final_cali$n,decreasing = TRUE)])



# 막대그래프 생성  
p1 <-ggplot(data = final_hong, aes(x = Reviewer_Location, y = n))+
  geom_bar(stat = "identity", fill = "orange")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5)+ 
  labs(title="홍콩 디즈니랜드 외국인 방문자 수(Top5)",x="나라", y = "방문자 수")


p2 <-ggplot(data = final_paris, aes(x = Reviewer_Location, y = n))+
  geom_bar(stat = "identity", fill = "skyblue")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5)+ 
  labs(title="파리 디즈니랜드 외국인 방문자 수(Top5)",x="나라", y = "방문자 수")
 

p3 <-ggplot(data = final_cali, aes(x = Reviewer_Location, y = n))+
  geom_bar(stat = "identity", fill = "darkgreen")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5)+ 
  labs(title="캘리포니아 디즈니랜드 외국인 방문자 수(Top5)",x="나라", y = "방문자 수")


p1 + theme_classic(base_family = "AppleGothic")+ theme(plot.title = element_text(hjust = 0.5))
p2 + theme_classic(base_family = "AppleGothic")+ theme(plot.title = element_text(hjust = 0.5))
p3 + theme_classic(base_family = "AppleGothic")+ theme(plot.title = element_text(hjust = 0.5))





# 텍스트 전처리
## hongkong 
hongkong_corpus <- Corpus(VectorSource(hongkong$Review_Text))
##paris 
paris_corpus<- Corpus(VectorSource(paris$Review_Text))

##california
california_corpus<- Corpus(VectorSource(california$Review_Text))


# 텍스트 정제 (소문자로 변환, 기호/숫자/불용어 제거)
hongkong_corpus.Clean<-tm_map(hongkong_corpus.Corpus, PlainTextDocument)
hongkong_corpus.Clean<-tm_map(hongkong_corpus.Corpus,tolower)
hongkong_corpus.Clean<-tm_map(hongkong_corpus.Clean,removeNumbers)
hongkong_corpus.Clean<-tm_map(hongkong_corpus.Clean,removeWords,stopwords("english"))# 필요에 따라 불용어 목록을 바꿀 수 있음 
hongkong_corpus.Clean<-tm_map(hongkong_corpus.Clean,removePunctuation)
hongkong_corpus.Clean<-tm_map(hongkong_corpus.Clean,stripWhitespace)
hongkong_corpus.Clean<-tm_map(hongkong_corpus.Clean,stemDocument)


# 정제된 텍스트 데이터를 합쳐서 하나의 문자열로 만들기 
text1<- unlist(sapply(hongkong_corpus.Clean, as.character))
text1 <- paste(text1, collapse = " ")


# 단어 빈도 테이블 생성
word_freq1 <- table(strsplit(text1, "\\s+"))
View(word_freq1)

# wordcloud2 생성
wordcloud2(data = word_freq1, size = 1.5, color = "random-dark",
           figPath ='mickey.png', background = "white")


# 텍스트 정제 (소문자로 변환, 기호/숫자/불용어 제거)
paris_corpus.Clean<-tm_map(paris_corpus.Corpus, PlainTextDocument)
paris_corpus.Clean<-tm_map(paris_corpus.Corpus,tolower)
paris_corpus.Clean<-tm_map(paris_corpus.Clean,removeNumbers)
paris_corpus.Clean<-tm_map(paris_corpus.Clean,removeWords,stopwords("english"))
paris_corpus.Clean<-tm_map(paris_corpus.Clean,removePunctuation)
paris_corpus.Clean<-tm_map(paris_corpus.Clean,stripWhitespace)
paris_corpus.Clean<-tm_map(paris_corpus.Clean,stemDocument)


# 정제된 텍스트 데이터를 합쳐서 하나의 문자열로 만들기 
text2<- unlist(sapply(paris_corpus.Clean, as.character))
text2 <- paste(text2, collapse = " ")


# 단어 빈도 테이블 생성
word_freq2 <- table(strsplit(text2, "\\s+"))
View(word_freq2)

# wordcloud2 생성
wordcloud2(data = word_freq2, size = 1.5, color = "random-dark",
           figPath ='mickey.png', background = "white")


# 텍스트 정제 (소문자로 변환, 기호/숫자/불용어 제거)
california_corpus.Clean<-tm_map(california_corpus.Corpus, PlainTextDocument)
california_corpus.Clean<-tm_map(california_corpus.Corpus,tolower)
california_corpus.Clean<-tm_map(california_corpus.Clean,removeNumbers)
california_corpus.Clean<-tm_map(california_corpus.Clean,removeWords,stopwords("english"))
california_corpus.Clean<-tm_map(california_corpus.Clean,removePunctuation)
california_corpus.Clean<-tm_map(california_corpus.Clean,stripWhitespace)
california_corpus.Clean<-tm_map(california_corpus.Clean,stemDocument)



# 정제된 텍스트 데이터를 합쳐서 하나의 문자열로 만들기 
text3<- unlist(sapply(california_corpus.Clean, as.character))
text3 <- paste(text3, collapse = " ")


# 단어 빈도 테이블 생성
word_freq3 <- table(strsplit(text3, "\\s+"))

# wordcloud2 생성
wordcloud2(data = word_freq3, size = 1.5, color = "random-light",
           figPath ='mickey.png', background = "black")
