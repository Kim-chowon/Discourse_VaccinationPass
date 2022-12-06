library(KoNLP)
library(tidyverse)
library(tidytext) 
library(tm)
library(topicmodels)
library(furrr)
library(writexl)

useNIADic()


# ������ �ε� �� ��ó�� ------------------------------------------------------------

user_dic <- data.frame(
  term = c("��ȸ�� �Ÿ��α�", "�濪�н�", "60�� �̻�", "�ܰ��� �ϻ�ȸ��", "�����ڷγ�", "������", "�赵�Ⱓ", "�����̿�ü�", "�ο�����"), tag = "ncn")

buildDictionary(ext_dic = 'NIAdic', user_dic = user_dic)

rawdata <- read_csv('news_final_��ȯ_3.csv',                       # csv ������ �о����
                    col_names = TRUE,                       
                    locale=locale('ko', encoding='UTF-8'), # �ѱ� ���ڵ�ó��
                    na=".")
  
data <- rawdata %>% 
  mutate(word = gsub("[[:cntrl:]]", " ", word)) %>%     
  mutate(word = gsub("��ȸ�� �Ÿ��α�|��ȸ���Ÿ��α�|�Ÿ��α�", "�Ÿ��α�", word)) %>%
  mutate(word = gsub("�濪�н�|����н�|�濪 �н�|��� �н�", "�濪�н�", word)) %>% 
  mutate(word = gsub("�ܰ��� �ϻ� ȸ��|�ܰ��� �ϻ�ȸ��", "�ܰ��� �ϻ�ȸ��", word)) %>% 
  mutate(word = gsub("���� ����|��������", "��������", word)) %>% 
  mutate(word = gsub("���� �̿�ü�|���� �̿� �ü�|�����̿�ü�", "�����̿�ü�", word)) %>% 
  mutate(word = gsub("�ο�����|�ο� ����", "�ο�����", word)) %>% 
  mutate(word = gsub("���� �ڷγ�|�����ڷγ�", "�����ڷγ�", word)) %>% 
  mutate(word = gsub("������|�����|�����", "������", word)) %>% 
  mutate(word = gsub("�赵�Ⱓ|�赵 �Ⱓ", "�赵�Ⱓ", word))
mutate(word = gsub("��", "", word))


# �ñ⺰ �����ͼ� ���� -------------------------------------------------------------

data_1 <- data %>% 
  subset(phase == 1)

data_2 <- data %>% 
  subset(phase == 2)

data_3 <- data %>% 
  subset(phase == 3)


# ���¼Һм� -------------------------------------------------------------------
data_morp <- data %>%
  unnest_tokens(input = word,
                output = morp,
                token = SimplePos09) %>%
  filter(str_detect(morp, "/n")) %>%
  mutate(word = str_remove(morp, "/.*$")) %>%
  filter(str_length(word) >= 2)

write_csv(data_morp, "�ο빮���¼�_7.csv")
data_morp <- read_csv("�ο빮���¼�_7.csv")

data_morp <- data_morp %>% 
  filter(!(word == "����")) %>% 
  filter(!(word == "���")) %>% 
  filter(!(word == "����")) %>% 
  filter(!(word == "����")) %>% 
  filter(!(word == "��ǻ�")) %>% 
  filter(!(word == "��ŭ")) %>% 
  filter(!(word == "�̹�")) %>% 
  filter(!(word == "����")) %>% 
  filter(!(word == "�׵���")) %>% 
  filter(!(word == "70")) %>% 
  filter(!(word == "����")) %>% 
  filter(!(word == "�츮")) %>% 
  filter(!(word == "����")) %>% 
  filter(!(word == "����"))

# �󵵺м� --------------------------------------------------------------------

# �����ں� �����ͼ�
data_morp_1 <- data_morp %>% 
  subset(actor == '����(�濪�籹)')

data_morp_2 <- data_morp %>% 
  subset(actor == '�ڿ�����(��ü)')

data_morp_3 <- data_morp %>% 
  subset(actor == '�ù�(��ü)')


data_morp_1 %>% 
  count(word, sort = T) %>% 
  print(n = 50) %>% 
  write_xlsx("����top50.xlsx")

data_morp_2 %>% 
  count(word, sort = T) %>% 
  print(n = 50) %>% 
  write_xlsx("�ڿ�����top50.xlsx")

data_morp_3 %>% 
  count(word, sort = T) %>% 
  print(n = 50) %>% 
  write_xlsx("�ù�top50.xlsx")


# ������-�ñ⺰ �����ͼ� ----------------------------------------------------------------

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

# ���ȸ𵨸� -----------------------------------------------------------

# �� ���� �ܾ� ����

data_morp_11 %>% 
  count(word, sort = T)

morp_count_11 <- data_morp_11 %>%  
  add_count(word) %>% 
  filter(n < 200) %>% 
  select(-n)

morp_count_11 %>% 
  count(word, sort = T)

# �ҿ�� Ȯ���ϱ� 

morp_count_11 %>% 
  count(word, sort = T) %>% 
  print(n = 100)


# ������ �ܾ� �� ���ϱ�
word_doc_count_11 <- morp_count_11 %>%
  count(no, word, sort = T)

dtm_11 <- word_doc_count_11 %>% 
  cast_dtm(document = no, 
           term = word, 
           value = n)


# ���� ���ȼ� ã��

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


# ��Ÿ �����ϱ� 

term_topic_11 <- tidy(lda_11, matrix = "beta")

top_term_topic_11 <- term_topic_11 %>%
  group_by(topic) %>%
  slice_max(beta, n = 12)


# ���� �����ϱ� 

doc_topic_11 <- tidy(lda_11, matrix = "gamma")


# �������� Ȯ���� ���� ���� ���� ����

doc_class_11 <- doc_topic_11 %>%
  group_by(document) %>%
  slice_max(gamma, n = 1)

# ���� Ÿ�� ����

doc_class_11$document <- as.integer(doc_class_11$document)

# ������ Ȯ���� ���� ���� ���� ��ȣ �ο�

data_topic_11 <- data %>%
  left_join(doc_class_11, by = c("no" = "document"))

# topic�� NA�� ���� ����

data_topic_11 <- data_topic_11 %>%
  na.omit()

# ���Ⱥ� �ֿ� �ܾ� ���

top_terms_11 <- term_topic_11 %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = F) %>%
  summarise(term = paste(term, collapse = ", "))

# ���Ⱥ� ���� ��

count_topic_11 <- data_topic_11 %>%
  count(topic)

# �����󵵿� �ֿ� �ܾ� ����

count_topic_word_11 <- count_topic_11 %>%
  left_join(top_terms_11, by = "topic") %>%
  mutate(topic_name = paste("Topic", topic))

# ���Ⱥ� ���� ���� �ֿ� �ܾ� ���� �׷���

ggplot(count_topic_word_11,
       aes(x = reorder(topic_name, n),
           y = n,
           fill = topic_name)) +
  geom_col(show.legend = F,
           color = "black",
           fill = "grey") +
  coord_flip() +
  geom_text(aes(label = n) ,                # ���� �� ǥ��
            hjust = -0.2) +                 # ���� �ۿ� ǥ��
  geom_text(aes(label = term),              # �ֿ� �ܾ� ǥ��
            hjust = 1.03,                   # ���� �ȿ� ǥ��
            col = "black",                  # ����             # �β���
            family = "sans",
            size = 7) +       # ��Ʈ180
  scale_y_continuous(expand = c(0, 0),      # y��-���� ���� ���̱�
                     limits = c(0, 60)) +  # y�� ����
  labs(x = NULL)  +
  theme(axis.text.y=element_text(colour="black",size=15))


# gamma ���� ���� ���� 

data_topic_11 %>%
  arrange(-gamma) %>%
  select(no, date, journal, source, word, topic, gamma) %>%
  write_xlsx("topic11.xlsx")