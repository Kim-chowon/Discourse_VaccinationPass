library(KoNLP)
library(tidyverse)
library(tidytext) 
library(tm)
library(topicmodels)
library(furrr)
library(writexl)

useNIADic()


# 데이터 로딩 및 전처리 ------------------------------------------------------------

user_dic <- data.frame(
  term = c("사회적 거리두기", "방역패스", "60대 이상", "단계적 일상회복", "위드코로나", "위중증", "계도기간", "다중이용시설", "인원제한"), tag = "ncn")

buildDictionary(ext_dic = 'NIAdic', user_dic = user_dic)

rawdata <- read_csv('news_final_변환_3.csv',                       # csv 데이터 읽어오기
                    col_names = TRUE,                       
                    locale=locale('ko', encoding='UTF-8'), # 한글 인코딩처리
                    na=".")
  
data <- rawdata %>% 
  mutate(word = gsub("[[:cntrl:]]", " ", word)) %>%     
  mutate(word = gsub("사회적 거리두기|사회적거리두기|거리두기", "거리두기", word)) %>%
  mutate(word = gsub("방역패스|백신패스|방역 패스|백신 패스", "방역패스", word)) %>% 
  mutate(word = gsub("단계적 일상 회복|단계적 일상회복", "단계적 일상회복", word)) %>% 
  mutate(word = gsub("사적 모임|사적모임", "사적모임", word)) %>% 
  mutate(word = gsub("다중 이용시설|다중 이용 시설|다중이용시설", "다중이용시설", word)) %>% 
  mutate(word = gsub("인원제한|인원 제한", "인원제한", word)) %>% 
  mutate(word = gsub("위드 코로나|위드코로나", "위드코로나", word)) %>% 
  mutate(word = gsub("고령층|노령층|노년층", "고령층", word)) %>% 
  mutate(word = gsub("계도기간|계도 기간", "계도기간", word))
mutate(word = gsub("들", "", word))


# 시기별 데이터셋 형성 -------------------------------------------------------------

data_1 <- data %>% 
  subset(phase == 1)

data_2 <- data %>% 
  subset(phase == 2)

data_3 <- data %>% 
  subset(phase == 3)


# 형태소분석 -------------------------------------------------------------------
data_morp <- data %>%
  unnest_tokens(input = word,
                output = morp,
                token = SimplePos09) %>%
  filter(str_detect(morp, "/n")) %>%
  mutate(word = str_remove(morp, "/.*$")) %>%
  filter(str_length(word) >= 2)

write_csv(data_morp, "인용문형태소_7.csv")
data_morp <- read_csv("인용문형태소_7.csv")

data_morp <- data_morp %>% 
  filter(!(word == "때문")) %>% 
  filter(!(word == "경우")) %>% 
  filter(!(word == "이후")) %>% 
  filter(!(word == "정도")) %>% 
  filter(!(word == "사실상")) %>% 
  filter(!(word == "만큼")) %>% 
  filter(!(word == "이번")) %>% 
  filter(!(word == "었다")) %>% 
  filter(!(word == "그동안")) %>% 
  filter(!(word == "70")) %>% 
  filter(!(word == "일일")) %>% 
  filter(!(word == "우리")) %>% 
  filter(!(word == "국민")) %>% 
  filter(!(word == "동안"))

# 빈도분석 --------------------------------------------------------------------

# 행위자별 데이터셋
data_morp_1 <- data_morp %>% 
  subset(actor == '정부(방역당국)')

data_morp_2 <- data_morp %>% 
  subset(actor == '자영업자(단체)')

data_morp_3 <- data_morp %>% 
  subset(actor == '시민(단체)')


data_morp_1 %>% 
  count(word, sort = T) %>% 
  print(n = 50) %>% 
  write_xlsx("정부top50.xlsx")

data_morp_2 %>% 
  count(word, sort = T) %>% 
  print(n = 50) %>% 
  write_xlsx("자영업자top50.xlsx")

data_morp_3 %>% 
  count(word, sort = T) %>% 
  print(n = 50) %>% 
  write_xlsx("시민top50.xlsx")


# 행위자-시기별 데이터셋 ----------------------------------------------------------------

data_morp_11 <- data_morp_1 %>% 
  subset(phase == 1)

data_morp_12 <- data_morp_1 %>% 
  subset(phase == 2)

data_morp_13 <- data_morp_1 %>% 
  subset(phase == 3)


data_morp_21 <- data_morp_2 %>% 
  subset(phase == 1)

data_morp_22 <- data_morp_2 %>% 
  subset(phase == 2)

data_morp_23 <- data_morp_2 %>% 
  subset(phase == 3)


data_morp_31 <- data_morp_3 %>% 
  subset(phase == 1)

data_morp_32 <- data_morp_3 %>% 
  subset(phase == 2)

data_morp_33 <- data_morp_3 %>% 
  subset(phase == 3)

# 토픽모델링 -----------------------------------------------------------

# 빈도 높은 단어 제거

data_morp_11 %>% 
  count(word, sort = T)

morp_count_11 <- data_morp_11 %>%  
  add_count(word) %>% 
  filter(n < 200) %>% 
  select(-n)

morp_count_11 %>% 
  count(word, sort = T)

# 불용어 확인하기 

morp_count_11 %>% 
  count(word, sort = T) %>% 
  print(n = 100)


# 문서별 단어 빈도 구하기
word_doc_count_11 <- morp_count_11 %>%
  count(no, word, sort = T)

dtm_11 <- word_doc_count_11 %>% 
  cast_dtm(document = no, 
           term = word, 
           value = n)


# 최적 토픽수 찾기

topics <- c(2:10)

lda_11 <- topics %>%
  future_map(LDA,
             x = dtm_11,
             control = list(seed = 12))


devtools::install_github("nikita-moor/ldatuning")
result_11 <- ldatuning::FindTopicsNumber(
  dtm_11,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1234),
  mc.cores = 2L,
  verbose = TRUE)

ldatuning::FindTopicsNumber_plot(result_11)

lda_11 <- LDA(dtm_11,
              k = 3,
              control = list(seed = 1234))


# 베타 추출하기 

term_topic_11 <- tidy(lda_11, matrix = "beta")

top_term_topic_11 <- term_topic_11 %>%
  group_by(topic) %>%
  slice_max(beta, n = 12)


# 감마 추출하기 

doc_topic_11 <- tidy(lda_11, matrix = "gamma")


# 문서별로 확률이 가장 높은 토픽 추출

doc_class_11 <- doc_topic_11 %>%
  group_by(document) %>%
  slice_max(gamma, n = 1)

# 변수 타입 통일

doc_class_11$document <- as.integer(doc_class_11$document)

# 문서에 확률이 가장 높은 토픽 번호 부여

data_topic_11 <- data %>%
  left_join(doc_class_11, by = c("no" = "document"))

# topic이 NA인 문서 제거

data_topic_11 <- data_topic_11 %>%
  na.omit()

# 토픽별 주요 단어 목록

top_terms_11 <- term_topic_11 %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = F) %>%
  summarise(term = paste(term, collapse = ", "))

# 토픽별 문서 빈도

count_topic_11 <- data_topic_11 %>%
  count(topic)

# 문서빈도에 주요 단어 결합

count_topic_word_11 <- count_topic_11 %>%
  left_join(top_terms_11, by = "topic") %>%
  mutate(topic_name = paste("Topic", topic))

# 토픽별 무서 수와 주요 단어 막대 그래프

ggplot(count_topic_word_11,
       aes(x = reorder(topic_name, n),
           y = n,
           fill = topic_name)) +
  geom_col(show.legend = F,
           color = "black",
           fill = "grey") +
  coord_flip() +
  geom_text(aes(label = n) ,                # 문서 빈도 표시
            hjust = -0.2) +                 # 막대 밖에 표시
  geom_text(aes(label = term),              # 주요 단어 표시
            hjust = 1.03,                   # 막대 안에 표시
            col = "black",                  # 색깔             # 두껍게
            family = "sans",
            size = 7) +       # 폰트180
  scale_y_continuous(expand = c(0, 0),      # y축-막대 간격 줄이기
                     limits = c(0, 60)) +  # y축 범위
  labs(x = NULL)  +
  theme(axis.text.y=element_text(colour="black",size=15))


# gamma 높은 문서 추출 

data_topic_11 %>%
  arrange(-gamma) %>%
  select(no, date, journal, source, word, topic, gamma) %>%
  write_xlsx("topic11.xlsx")
