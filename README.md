# TF-IDF, LDA, Doc2Vec을 이용하여 document representation을 위한 Multi-co-training

### 1. Dictionary_'카테고리 이름'.csv : 네이버의 두산백과의 해당 카테고리들을 크롤링한 파일

### 2. Digital_Daily.csv : 네이버의 디지털 데일리 라는 기사들을 크롤링한 파일

### 3. Stopword.csv : 불용어 파일

### 4. mydicitionary.txt : 사전

### 5. Crawling.R : 데이터를 크롤링 하기위해 코드를 짠 파일

### 6. Morpheme_Analysis.R : corpus를 만들기 위해 연습한 파일

### 7. Realization_TFIDF_LDA.R : doc2vec은 사용하지 못했으며, TF-IDF와 LDA 두 방법만을 가지고 논문에 있는 실험들을 구현한 파일

# 하지 못했거나 아쉬운 점

1. Doc2Vec은 사용하는 방법을 몰라 하지 못했으며, Word2Vec을 사용해보려 했으나 본 목적인 문서 분류에 맞지 않아 예시 데이터를 가지고 사용만 해봤다. 결국, TF-IDF와 LDA만 이용해서 만들어 봤다.

2. 여러 조건들을 고려하여 만들어야 하지만 이를 다 무시 했다. (LDA의 경우 Collapsed Gibbs Sampling, Variational Expectaion-Maxmization)

3. 커스텀 사전과 불용어 (Stopwords) 사전이 굉장히 허술하여 파싱 및 형태소 분석이 제대로 이루어지지 않은 것 같고, 데이터의 갯수도 7천 or 1만2천개 정도 밖에 안된다.

4. 학습 전략 (Learning Strategy) 중 SL, SSL은 사용하지 못했다. Doc2Vec을 사용하지 못해 MCT 또한 구현 하지 못했고 결국 Co-training 까지만 만들었다.

5. 레이블링 데이터 비율 (2%, 5%, ...)도 다양하게 맞춰 해야했지만 고정된 하나의 비율로만 하였다.
