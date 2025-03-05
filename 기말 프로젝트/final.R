
# 필요한 라이브러리 로드
library(forecast)
library(ggplot2)

# 데이터 로드 및 전처리


# 시계열 데이터 생성 (시작 연도: 2010, 빈도: 12개월)
ts_data <- ts(data, start=c(2010, 1), frequency=12)

# 데이터 시각화
autoplot(ts_data) + ggtitle("월별 데이터") + ylab("인원(명)")

# 이상치 탐지 및 대체
ts_data_clean <- tsclean(ts_data)
autoplot(ts_data_clean) + ggtitle("이상치 제거 후 데이터") + ylab("인원(명)")

decomposed_data <- stl(ts_data_clean, s.window="periodic")
autoplot(decomposed_data)


# SARIMA 모델 적합 (계절성을 반영한 ARIMA)
fit <- auto.arima(ts_data_clean, seasonal=TRUE)

# 예측 (2025년까지)
forecast_data <- forecast(fit, h=20) # 2024년 5월부터 2025년 12월까지 예측
autoplot(forecast_data) + ggtitle("2024-2025년 예측") + ylab("인원(명)")

# 예측값 출력
print(forecast_data)

# 필요한 패키지 로드
library(ggplot2)

# 주어진 데이터
dates <- c("2010년01월", "2010년02월", "2010년03월", "2010년04월", "2010년05월", "2010년06월", "2010년07월", "2010년08월",
           "2010년09월", "2010년10월", "2010년11월", "2010년12월", "2011년01월", "2011년02월", "2011년03월", "2011년04월",
           "2011년05월", "2011년06월", "2011년07월", "2011년08월", "2011년09월", "2011년10월", "2011년11월", "2011년12월",
           "2012년01월", "2012년02월", "2012년03월", "2012년04월", "2012년05월", "2012년06월", "2012년07월", "2012년08월",
           "2012년09월", "2012년10월", "2012년11월", "2012년12월", "2013년01월", "2013년02월", "2013년03월", "2013년04월",
           "2013년05월", "2013년06월", "2013년07월", "2013년08월", "2013년09월", "2013년10월", "2013년11월", "2013년12월",
           "2014년01월", "2014년02월", "2014년03월", "2014년04월", "2014년05월", "2014년06월", "2014년07월", "2014년08월",
           "2014년09월", "2014년10월", "2014년11월", "2014년12월", "2015년01월", "2015년02월", "2015년03월", "2015년04월",
           "2015년05월", "2015년06월", "2015년07월", "2015년08월", "2015년09월", "2015년10월", "2015년11월", "2015년12월",
           "2016년01월", "2016년02월", "2016년03월", "2016년04월", "2016년05월", "2016년06월", "2016년07월", "2016년08월",
           "2016년09월", "2016년10월", "2016년11월", "2016년12월", "2017년01월", "2017년02월", "2017년03월", "2017년04월",
           "2017년05월", "2017년06월", "2017년07월", "2017년08월", "2017년09월", "2017년10월", "2017년11월", "2017년12월",
           "2018년01월", "2018년02월", "2018년03월", "2018년04월", "2018년05월", "2018년06월", "2018년07월", "2018년08월",
           "2018년09월", "2018년10월", "2018년11월", "2018년12월", "2019년01월", "2019년02월", "2019년03월", "2019년04월",
           "2019년05월", "2019년06월", "2019년07월", "2019년08월", "2019년09월", "2019년10월", "2019년11월", "2019년12월",
           "2020년01월", "2020년02월", "2020년03월", "2020년04월", "2020년05월", "2020년06월", "2020년07월", "2020년08월",
           "2020년09월", "2020년10월", "2020년11월", "2020년12월", "2021년01월", "2021년02월", "2021년03월", "2021년04월",
           "2021년05월", "2021년06월", "2021년07월", "2021년08월", "2021년09월", "2021년10월", "2021년11월", "2021년12월",
           "2022년01월", "2022년02월", "2022년03월", "2022년04월", "2022년05월", "2022년06월", "2022년07월", "2022년08월",
           "2022년09월", "2022년10월", "2022년11월", "2022년12월", "2023년01월", "2023년02월", "2023년03월", "2023년04월",
           "2023년05월", "2023년06월", "2023년07월", "2023년08월", "2023년09월", "2023년10월", "2023년11월", "2023년12월",
           "2024년01월", "2024년02월", "2024년03월", "2024년04월")

total_visitors <- c(400846, 446579, 545442, 543297, 525340, 520474, 541085, 580207,
                    546739, 658244, 558324, 500389, 399998, 430922, 539278, 543974,
                    535772, 586384, 667906, 718143, 674970, 768196, 677254, 660296,
                    557008, 574347, 711326, 773569, 732888, 755401, 811913, 854750,
                    766355, 793089, 663682, 662490, 570758, 608384, 756798, 727217,
                    658412, 760357, 872436, 986570, 853274, 837826, 710478, 733178,
                    640335, 643532, 865927, 981715, 951282, 946177, 1029905, 1084204,
                    939728, 1056804, 900170, 887701, 723255, 815138, 958493, 1112904,
                    1060730, 517926, 419430, 730808, 907874, 1076153, 911647, 901131,
                    865064, 849995, 1123465, 1214201, 1202692, 1277116, 1405496, 1312514,
                    1199450, 1303692, 1076471, 1102769, 978223, 952948, 970389, 854669,
                    738590, 775181, 782389, 817226, 839415, 899881, 880745, 925938,
                    742732, 790477, 1085542, 1091353, 991449, 1055855, 1030612, 1100711,
                    1013326, 1269201, 1130709, 1112381, 884293, 907928, 1251744, 1382540,
                    1233562, 1237840, 1218767, 1251764, 1178421, 1403576, 1234503, 1247337,
                    1030497, 503976, 31497, 5135, 6111, 9744, 12732, 8634, 10619, 11119,
                    11514, 11893, 9035, 8500, 11169, 11492, 15381, 17847, 20297, 18452,
                    19611, 25698, 28734, 25630, 16961, 16512, 22492, 57448, 89179, 133883,
                    153171, 159128, 213310, 360862, 341686, 434305, 312847, 319098, 627681,
                    736870, 691789, 789269, 853340, 839458, 901891, 1009849, 933033, 865774,
                    696840, 773941, 1223899, 1233368)

japan_visitors <- c(202825, 224475, 299820, 233697, 233311, 226786, 221590, 254178,
                    259846, 295488, 268947, 224684, 189601, 213970, 262003, 213645,
                    229017, 249397, 269455, 315693, 312686, 332700, 325057, 293435,
                    235585, 287950, 353455, 291877, 306488, 296274, 291787, 334318,
                    301543, 259529, 243495, 220917, 198320, 209621, 280541, 194611,
                    205412, 191995, 200452, 258870, 234591, 235183, 215737, 208626,
                    164038, 181307, 239575, 169382, 184996, 164438, 164741, 198068,
                    186840, 182575, 172285, 164921, 131909, 134366, 211846, 147108,
                    176044, 93726, 74890, 137125, 150080, 172411, 158383, 154643,
                    129290, 136324, 217623, 169211, 171675, 173883, 179569, 215859,
                    200876, 220409, 206968, 191412, 146111, 177671, 267416, 159263,
                    151502, 161395, 163712, 216683, 213446, 171182, 207088, 187745,
                    159174, 159831, 287578, 207727, 220375, 228667, 224273, 306108,
                    240457, 283494, 293965, 252461, 198805, 204697, 367157, 283101,
                    279174, 275286, 267816, 318985, 242475, 241484, 251663, 248793,
                    194840, 205007, 6478, 58, 45, 73, 44, 25, 29, 37, 32, 33, 16, 31, 31,
                    40, 36, 44, 54, 97, 72, 176, 273, 177, 103, 91, 112, 535, 668, 2042,
                    7737, 20324, 23413, 63905, 59089, 81012, 61766, 89217, 187833,
                    124741, 178728, 192316, 206914, 256082, 245965, 250235, 272686,
                    191892, 137713, 177633, 334131, 225182)

# 필요한 패키지 설치 및 로드

library(ggplot2)
library(tidyr)

# 주어진 데이터
dates <- c("2010-01", "2010-02", "2010-03", "2010-04", "2010-05", "2010-06", "2010-07", "2010-08",
           "2010-09", "2010-10", "2010-11", "2010-12", "2011-01", "2011-02", "2011-03", "2011-04",
           "2011-05", "2011-06", "2011-07", "2011-08", "2011-09", "2011-10", "2011-11", "2011-12",
           "2012-01", "2012-02", "2012-03", "2012-04", "2012-05", "2012-06", "2012-07", "2012-08",
           "2012-09", "2012-10", "2012-11", "2012-12", "2013-01", "2013-02", "2013-03", "2013-04",
           "2013-05", "2013-06", "2013-07", "2013-08", "2013-09", "2013-10", "2013-11", "2013-12",
           "2014-01", "2014-02", "2014-03", "2014-04", "2014-05", "2014-06", "2014-07", "2014-08",
           "2014-09", "2014-10", "2014-11", "2014-12", "2015-01", "2015-02", "2015-03", "2015-04",
           "2015-05", "2015-06", "2015-07", "2015-08", "2015-09", "2015-10", "2015-11", "2015-12",
           "2016-01", "2016-02", "2016-03", "2016-04", "2016-05", "2016-06", "2016-07", "2016-08",
           "2016-09", "2016-10", "2016-11", "2016-12", "2017-01", "2017-02", "2017-03", "2017-04",
           "2017-05", "2017-06", "2017-07", "2017-08", "2017-09", "2017-10", "2017-11", "2017-12",
           "2018-01", "2018-02", "2018-03", "2018-04", "2018-05", "2018-06", "2018-07", "2018-08",
           "2018-09", "2018-10", "2018-11", "2018-12", "2019-01", "2019-02", "2019-03", "2019-04",
           "2019-05", "2019-06", "2019-07", "2019-08", "2019-09", "2019-10", "2019-11", "2019-12",
           "2020-01", "2020-02", "2020-03", "2020-04", "2020-05", "2020-06", "2020-07", "2020-08",
           "2020-09", "2020-10", "2020-11", "2020-12", "2021-01", "2021-02", "2021-03", "2021-04",
           "2021-05", "2021-06", "2021-07", "2021-08", "2021-09", "2021-10", "2021-11", "2021-12",
           "2022-01", "2022-02", "2022-03", "2022-04", "2022-05", "2022-06", "2022-07", "2022-08",
           "2022-09", "2022-10", "2022-11", "2022-12", "2023-01", "2023-02", "2023-03", "2023-04",
           "2023-05", "2023-06", "2023-07", "2023-08", "2023-09", "2023-10", "2023-11", "2023-12",
           "2024-01", "2024-02", "2024-03", "2024-04" )

kpop <- c(5, 5, 5, 6, 8, 8, 7, 27, 23, 19, 17, 14, 17, 18, 14, 13, 16, 19, 19, 20, 23, 20, 17, 17, 17, 17, 17, 18, 17, 16, 16, 20, 18, 14, 14, 12, 14, 15, 16, 15, 16, 16, 17, 18, 18, 18, 18, 18, 20, 18, 17, 17, 17, 19, 19, 19, 36, 21, 21, 18, 25, 21, 19, 19, 22, 20, 23, 24, 24, 23, 24, 27, 31, 31, 29, 28, 34, 28, 31, 27, 29, 29, 30, 30, 32, 36, 35, 32, 37, 38, 42, 39, 35, 39, 38, 61, 51, 52, 49, 45, 45, 46, 46, 47, 48, 46, 49, 53, 48, 49, 52, 45, 49, 41, 50, 51, 45, 49, 62, 48, 48, 48, 51, 49, 72, 65, 58, 71, 66, 62, 65, 68, 69, 70, 68, 60, 65, 63, 66, 72, 68, 66, 72, 73, 79, 83, 85, 82, 90, 87, 87, 97, 90, 84, 85, 91, 92, 99, 92, 100, 95, 92, 93, 91, 93, 86, 87, 96, 90, 79, 82, 79)
seoul <- c(30, 28, 31, 25, 23, 22, 23, 25, 28, 27, 24, 18, 24, 32, 28, 28, 30, 34, 33, 35, 91, 100, 58, 39, 39, 36, 37, 33, 34, 34, 31, 33, 38, 44, 45, 38, 34, 35, 50, 36, 33, 29, 28, 28, 30, 28, 27, 27, 30, 33, 90, 72, 60, 43, 37, 37, 37, 34, 28, 26, 26, 31, 30, 32, 27, 24, 24, 24, 27, 27, 25, 23, 26, 30, 55, 86, 53, 37, 42, 36, 31, 35, 33, 28, 29, 28, 30, 35, 30, 25, 25, 23, 24, 24, 23, 22, 25, 22, 23, 22, 30, 37, 28, 26, 23, 24, 24, 24, 23, 21, 23, 21, 21, 21, 23, 22, 21, 20, 21, 20, 23, 21, 16, 16, 18, 17, 17, 17, 18, 14, 17, 17, 17, 16, 16, 14, 14, 13, 14, 15, 14, 14, 14, 15, 17, 20, 18, 14, 15, 15, 12, 17, 16, 19, 15, 14, 16, 17, 16, 15, 16, 17, 17, 16, 17, 16, 17, 16, 16, 18, 18, 16)
kdrama <- c(41, 44, 47, 53, 55, 48, 46, 48, 50, 44, 43, 43, 49, 61, 49, 49, 62, 59, 60, 62, 73, 57, 54, 43, 45, 52, 52, 50, 52, 50, 45, 45, 49, 44, 48, 45, 47, 49, 48, 47, 52, 50, 48, 48, 50, 52, 49, 46, 48, 54, 57, 57, 57, 56, 55, 58, 57, 53, 52, 49, 58, 60, 57, 60, 58, 57, 57, 60, 68, 65, 61, 48, 56, 61, 65, 64, 60, 57, 56, 53, 63, 59, 64, 60, 58, 60, 59, 63, 63, 62, 58, 60, 69, 61, 67, 62, 62, 63, 64, 68, 65, 59, 56, 59, 61, 65, 66, 57, 63, 78, 68, 61, 72, 64, 74, 70, 74, 66, 70, 63, 65, 74, 75, 81, 100, 88, 82, 82, 82, 76, 75, 73, 87, 89, 85, 81, 90, 87, 83, 87, 94, 82, 85, 73, 75, 79, 78, 78, 76, 73, 67, 69, 74, 68, 63, 56, 71, 76, 70, 69, 64, 61, 55, 58, 64, 64, 63, 57, 54, 57, 52, 46)
samgyeopsal <- c(12, 13, 12, 11, 15, 12, 13, 15, 16, 17, 17, 13, 20, 28, 20, 24, 27, 32, 43, 43, 37, 42, 45, 49, 43, 51, 56, 49, 52, 56, 50, 50, 52, 48, 47, 39, 43, 50, 55, 47, 48, 52, 51, 52, 40, 41, 43, 47, 43, 42, 47, 47, 64, 49, 49, 44, 41, 41, 42, 41, 40, 46, 49, 40, 44, 46, 44, 38, 41, 44, 46, 46, 45, 50, 50, 47, 57, 53, 53, 46, 45, 47, 54, 55, 58, 62, 62, 55, 59, 66, 64, 52, 50, 52, 52, 53, 53, 62, 64, 59, 58, 59, 54, 57, 54, 53, 55, 60, 60, 62, 63, 63, 64, 67, 63, 61, 74, 59, 69, 65, 66, 75, 61, 48, 71, 70, 78, 74, 86, 92, 80, 65, 80, 78, 79, 72, 77, 82, 73, 66, 68, 69, 79, 69, 73, 81, 84, 88, 100, 97, 91, 92, 92, 83, 79, 75, 79, 91, 88, 95, 95, 91, 92, 87, 89, 79, 79, 82, 76, 88, 88, 86)
dakgalbi <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 2, 2, 5, 6, 10, 18, 21, 21, 24, 27, 28, 33, 42, 52, 60, 93, 88, 100, 77, 59, 48, 42, 33, 38, 35, 29, 33, 40, 28, 28, 26, 25, 24, 19, 20, 18, 21, 18, 17, 16, 16, 19, 17, 21, 21, 18, 17, 15, 15, 16, 18, 15, 19, 19, 16, 15, 17, 14, 13, 14, 15, 14, 14, 13, 14, 17, 14, 12, 13, 13, 11, 11, 11, 11, 12, 11, 11, 12, 12, 10, 11, 10, 8, 9, 9, 9, 11, 9, 10, 9, 9, 8)
seongsu <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 19, 0, 15, 12, 0, 12, 27, 16, 23, 14, 13, 13, 20, 28, 21, 24, 13, 19, 10, 13, 13, 19, 15, 14, 17, 16, 9, 16, 10, 19, 23, 16, 24, 15, 27, 20, 21, 19, 18, 26, 39, 22, 23, 18, 21, 22, 18, 18, 26, 25, 21, 25, 21, 23, 22, 25, 15, 17, 22, 22, 22, 23, 18, 20, 19, 17, 20, 21, 15, 16, 14, 20, 14, 16, 24, 22, 17, 20, 18, 20, 18, 17, 25, 30, 32, 20, 21, 19, 16, 18, 18, 17, 16, 14, 14, 20, 22, 15, 18, 15, 17, 15, 13, 15, 12, 13, 14, 17, 21, 16, 16, 20, 15, 11, 12, 14, 15, 13, 12, 13, 14, 30, 15, 15, 15, 15, 15, 13, 10, 9, 11, 12, 15, 14, 15, 14, 13, 14, 14, 13, 15, 15, 15, 19, 20, 24, 17, 29, 25, 21, 26, 30, 35, 37, 50, 48, 54, 38, 43, 58, 75, 74)
myeongdong <- c(28, 25, 22, 23, 25, 21, 25, 27, 32, 34, 27, 23, 31, 36, 28, 31, 33, 34, 36, 41, 46, 43, 45, 38, 49, 53, 45, 39, 43, 45, 40, 47, 42, 41, 42, 35, 48, 50, 44, 32, 31, 35, 38, 39, 45, 41, 34, 35, 38, 43, 39, 28, 27, 31, 28, 28, 33, 28, 25, 23, 33, 35, 36, 29, 30, 16, 14, 23, 31, 32, 32, 31, 42, 48, 49, 37, 41, 44, 40, 47, 50, 55, 48, 45, 64, 70, 73, 41, 37, 46, 49, 59, 59, 56, 58, 51, 67, 74, 86, 64, 70, 74, 74, 83, 87, 88, 83, 74, 87, 93, 100, 79, 87, 88, 82, 71, 71, 73, 76, 72, 89, 66, 13, 9, 11, 11, 11, 10, 12, 13, 11, 10, 10, 12, 10, 9, 10, 10, 10, 10, 10, 10, 11, 11, 10, 11, 10, 10, 10, 12, 12, 14, 22, 32, 29, 31, 44, 61, 66, 57, 71, 89, 81, 87, 96, 87, 83, 60, 70, 94, 98, 89)
hangang <- c(18, 16, 14, 13, 0, 19, 23, 18, 25, 19, 16, 15, 13, 18, 17, 20, 23, 23, 25, 25, 30, 20, 15, 16, 17, 15, 19, 19, 15, 18, 19, 20, 19, 14, 11, 14, 11, 22, 17, 13, 16, 15, 18, 16, 19, 13, 15, 12, 8, 13, 10, 19, 14, 12, 9, 13, 11, 8, 6, 10, 9, 8, 11, 8, 9, 12, 8, 10, 7, 9, 8, 9, 6, 8, 7, 6, 6, 12, 11, 10, 10, 11, 9, 10, 10, 13, 11, 12, 9, 8, 15, 13, 10, 8, 11, 9, 10, 13, 11, 10, 11, 10, 10, 10, 12, 11, 14, 10, 11, 10, 10, 14, 14, 14, 15, 26, 19, 14, 11, 11, 15, 23, 14, 15, 12, 12, 13, 14, 10,10,	16,	11,	13,	10,	10,	9,	13,	11,	11,	9,	10,	12,	11,	11,	8,	9,	9,	10,	11,	11,	11,	13,	12,	15,	11,	10,	13,	17,	21,	17,	17,	21,	22,	19,	21,	21,	18,	14,	13,	20,	23,	73)

# 데이터 프레임이 이미 존재한다고 가정
data <- data.frame(
  Date = as.Date(paste0(dates, "-01")),
  Kpop = kpop,
  Seoul = seoul,
  Kdrama = kdrama,
  Samgyeopsal = samgyeopsal,
  Dakgalbi = dakgalbi,
  Seongsu = seongsu,
  Myeongdong = myeongdong,
  Hangang = hangang
)

# 필요한 패키지 로드
install.packages("ggplot2")
library(ggplot2)
library(tidyr)

# 데이터 프레임이 이미 존재한다고 가정
data <- data.frame(
  Date = as.Date(paste0(dates, "-01")),
  Kpop = kpop,
  Seoul = seoul,
  Kdrama = kdrama,
  Samgyeopsal = samgyeopsal,
  Dakgalbi = dakgalbi,
  Seongsu = seongsu,
  Myeongdong = myeongdong,
  Hangang = hangang
)

# 데이터 프레임을 긴 형식으로 변환
data_long <- data %>%
  gather(key = "Keyword", value = "SearchVolume", -Date)

# 패싯을 사용하여 꺾은선 그래프 그리기
ggplot(data_long, aes(x = Date, y = SearchVolume, color = Keyword)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.5) +
  labs(title = "Search Volume Trends Over Time",
       x = "Date",
       y = "Search Volume",
       color = "Keyword") +
  scale_color_manual(values = c("Kpop" = "red", "Seoul" = "blue", "Kdrama" = "green",
                                "Samgyeopsal" = "purple", "Dakgalbi" = "orange",
                                "Seongsu" = "brown", "Myeongdong" = "pink", "Hangang" = "black")) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  facet_wrap(~ Keyword, scales = "free_y", ncol = 2)  # 패싯으로 그래프 분할


#####
# 데이터 프레임이 이미 존재한다고 가정
library(dplyr)
library(knitr)
library(tidyr)
data <- data.frame(
  Date = as.Date(paste0(dates, "-01")),
  Japan_Visitors = japan_visitors,
  Kpop = kpop,
  Seoul = seoul,
  Kdrama = kdrama,
  Samgyeopsal = samgyeopsal,
  Dakgalbi = dakgalbi,
  Seongsu = seongsu,
  Myeongdong = myeongdong,
  Hangang = hangang
)

# 통계 요약표 생성
summary_table <- data %>%
  summarize(
    Mean_Japan_Visitors = mean(Japan_Visitors, na.rm = TRUE),
    Median_Japan_Visitors = median(Japan_Visitors, na.rm = TRUE),
    Max_Japan_Visitors = max(Japan_Visitors, na.rm = TRUE),
    Min_Japan_Visitors = min(Japan_Visitors, na.rm = TRUE),
    SD_Japan_Visitors = sd(Japan_Visitors, na.rm = TRUE),
    
    Mean_Kpop = mean(Kpop, na.rm = TRUE),
    Median_Kpop = median(Kpop, na.rm = TRUE),
    Max_Kpop = max(Kpop, na.rm = TRUE),
    Min_Kpop = min(Kpop, na.rm = TRUE),
    SD_Kpop = sd(Kpop, na.rm = TRUE),
    
    Mean_Seoul = mean(Seoul, na.rm = TRUE),
    Median_Seoul = median(Seoul, na.rm = TRUE),
    Max_Seoul = max(Seoul, na.rm = TRUE),
    Min_Seoul = min(Seoul, na.rm = TRUE),
    SD_Seoul = sd(Seoul, na.rm = TRUE),
    
    Mean_Kdrama = mean(Kdrama, na.rm = TRUE),
    Median_Kdrama = median(Kdrama, na.rm = TRUE),
    Max_Kdrama = max(Kdrama, na.rm = TRUE),
    Min_Kdrama = min(Kdrama, na.rm = TRUE),
    SD_Kdrama = sd(Kdrama, na.rm = TRUE),
    
    Mean_Samgyeopsal = mean(Samgyeopsal, na.rm = TRUE),
    Median_Samgyeopsal = median(Samgyeopsal, na.rm = TRUE),
    Max_Samgyeopsal = max(Samgyeopsal, na.rm = TRUE),
    Min_Samgyeopsal = min(Samgyeopsal, na.rm = TRUE),
    SD_Samgyeopsal = sd(Samgyeopsal, na.rm = TRUE),
    
    Mean_Dakgalbi = mean(Dakgalbi, na.rm = TRUE),
    Median_Dakgalbi = median(Dakgalbi, na.rm = TRUE),
    Max_Dakgalbi = max(Dakgalbi, na.rm = TRUE),
    Min_Dakgalbi = min(Dakgalbi, na.rm = TRUE),
    SD_Dakgalbi = sd(Dakgalbi, na.rm = TRUE),
    
    Mean_Seongsu = mean(Seongsu, na.rm = TRUE),
    Median_Seongsu = median(Seongsu, na.rm = TRUE),
    Max_Seongsu = max(Seongsu, na.rm = TRUE),
    Min_Seongsu = min(Seongsu, na.rm = TRUE),
    SD_Seongsu = sd(Seongsu, na.rm = TRUE),
    
    Mean_Myeongdong = mean(Myeongdong, na.rm = TRUE),
    Median_Myeongdong = median(Myeongdong, na.rm = TRUE),
    Max_Myeongdong = max(Myeongdong, na.rm = TRUE),
    Min_Myeongdong = min(Myeongdong, na.rm = TRUE),
    SD_Myeongdong = sd(Myeongdong, na.rm = TRUE),
    
    Mean_Hangang = mean(Hangang, na.rm = TRUE),
    Median_Hangang = median(Hangang, na.rm = TRUE),
    Max_Hangang = max(Hangang, na.rm = TRUE),
    Min_Hangang = min(Hangang, na.rm = TRUE),
    SD_Hangang = sd(Hangang, na.rm = TRUE)
  )

# 요약 통계량을 행으로 정리
summary_table_long <- summary_table %>%
  gather(key = "Metric", value = "Value")

# 변수 이름 추출
variable_names <- c("Japan_Visitors", "Kpop", "Seoul", "Kdrama", "Samgyeopsal", "Dakgalbi", "Seongsu", "Myeongdong", "Hangang")

# 변수별로 요약 통계량을 정리
summary_table_wide <- data.frame(
  Metric = c("Mean", "Median", "Max", "Min", "SD")
)

for (var in variable_names) {
  summary_table_wide <- summary_table_wide %>%
    mutate(!!var := summary_table_long$Value[grep(var, summary_table_long$Metric)])
}

# 정갈한 표 출력
print(kable(summary_table_wide, format = "markdown"))

# 데이터 프레임을 긴 형식으로 변환하여 박스플롯 생성
data_long <- data %>%
  gather(key = "Variable", value = "Value", -Date)

# 일본 방문객 수 박스플롯
ggplot(data_long %>% filter(Variable == "Japan_Visitors"), aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  labs(title = "Boxplot of Japan Visitors",
       x = "Variable",
       y = "Value") +
  theme_minimal()

# 나머지 변수들 박스플롯
ggplot(data_long %>% filter(Variable != "Japan_Visitors"), aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  labs(title = "Boxplot of Other Variables",
       x = "Variable",
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##############################SARIMAX
# 필요한 패키지 로드
# 필요한 패키지 로드
library(corrplot)
library(forecast)
library(dplyr)
library(tidyr)
library(Metrics)
library(zoo)
library(caret)
# 2009년 9월부터 12월까지의 외생 변수 데이터
exog_lag_data <- data.frame(
  Date = as.Date(c("2009-09-01", "2009-10-01", "2009-11-01", "2009-12-01")),
  Kpop = c(4, 4, 6, 6),
  Seoul = c(54, 43, 33, 27),
  Kdrama = c(37, 40, 38, 34),
  Samgyeopsal = c(11, 9, 9, 9),
  Dakgalbi = c(1, 0, 0, 0),
  Seongsu = c(0, 0, 0, 0),
  Myeongdong = c(27, 22, 23, 20),
  Hangang = c(31, 28, 19, 22)
)

data <- data.frame(
  Date = as.Date(paste0(dates, "-01")),
  Japan_Visitors = japan_visitors,
  Kpop = kpop,
  Seoul = seoul,
  Kdrama = kdrama,
  Samgyeopsal = samgyeopsal,
  Dakgalbi = dakgalbi,
  Seongsu = seongsu,
  Myeongdong = myeongdong,
  Hangang = hangang
)

# 상관행렬 계산 (Japan_Visitors 제외)
cor_matrix <- cor(data[ , -c(1, 2)], use = "complete.obs")

# 상관행렬 시각화
corrplot(cor_matrix, method = "number")

# 상관계수가 높은 변수 확인 (절대값 기준으로 0.8 이상)
high_corr_vars <- findCorrelation(cor_matrix, cutoff = 0.8, names = TRUE)

# 상관계수가 높은 변수 제거
reduced_data <- data[ , !(colnames(data) %in% high_corr_vars)]
reduced_data <- reduced_data[ , -6]

# 제거된 변수 확인
print("Removed Variables due to High Correlation:")
print(high_corr_vars)

train_data <- data %>% filter(Date <= as.Date("2023-12-01"))
test_data <- data %>% filter(Date >= as.Date("2024-01-01"))
train_y_ts <- ts(train_data$Japan_Visitors, start = c(2010, 1), frequency = 12)
test_y_ts <- ts(test_data$Japan_Visitors, start = c(2024, 1), frequency = 12)


# 외생 변수의 lag를 생성하는 함수 수정
create_lags <- function(data, lags, prefix, lag_data) {
  lagged_data <- data.frame(matrix(ncol = 0, nrow = length(data)))
  for (lag in 0:lags) {
    if (lag == 0) {
      lagged_col <- data
    } else {
      lagged_col <- c(lag_data[(length(lag_data) - lag + 1):length(lag_data)], data[1:(length(data) - lag)])
    }
    lagged_data <- cbind(lagged_data, lagged_col[1:length(data)])
    colnames(lagged_data)[ncol(lagged_data)] <- paste0(prefix, "_lag", lag)
  }
  return(lagged_data)
}

# 학습 데이터와 테스트 데이터 분리
train_data <- reduced_data %>% filter(Date <= as.Date("2023-12-01"))
test_data <- reduced_data %>% filter(Date >= as.Date("2024-01-01"))

# 각 변수의 lag 데이터 생성 (닭갈비 제외)
remaining_vars <- setdiff(remaining_vars, "Dakgalbi")
exog_lag_list <- lapply(remaining_vars, function(var) {
  create_lags(train_data[[var]], 4, var, exog_lag_data[[var]])
})
names(exog_lag_list) <- remaining_vars

# 테스트 데이터의 lag 데이터 생성 함수 수정
create_test_lags <- function(data, lags, prefix, lag_data) {
  lagged_data <- data.frame(matrix(ncol = 0, nrow = length(data)))
  for (lag in 0:lags) {
    if (lag == 0) {
      lagged_col <- data
    } else {
      lagged_col <- c(lag_data[(length(lag_data) - lag + 1):length(lag_data)], data[1:(length(data) - lag)])
    }
    lagged_col <- lagged_col[1:length(data)]  # 데이터 길이를 맞추기 위해 잘라내기
    lagged_data <- cbind(lagged_data, lagged_col)
    colnames(lagged_data)[ncol(lagged_data)] <- paste0(prefix, "_lag", lag)
  }
  return(lagged_data)
}

# 테스트 데이터의 lag 데이터 생성 (닭갈비 제외)
exog_test_lag_list <- lapply(remaining_vars, function(var) {
  create_test_lags(test_data[[var]], 4, var, tail(train_data[[var]], 4))
})
names(exog_test_lag_list) <- remaining_vars

# 최적의 lag 조합을 찾기 위한 변수 초기화
best_lags <- list()
best_rmse <- Inf
count = 0
# 다양한 lag 조합을 테스트하여 최적의 lag 선택 (닭갈비 제외)
for (seoul_lag in 0:4) {
  for (kdrama_lag in 0:4) {
    for (samgyeopsal_lag in 0:4) {
      for (seongsu_lag in 0:4) {
        for (myeongdong_lag in 0:4) {
          for (hangang_lag in 0:4) {
            count = count + 1
            if(count %% 5000 == 0) print("5000!")
            # 외생 변수의 lag를 생성
            exog_train <- data.frame(
              exog_lag_list$Seoul[1:nrow(train_data), seoul_lag + 1],
              exog_lag_list$Kdrama[1:nrow(train_data), kdrama_lag + 1],
              exog_lag_list$Samgyeopsal[1:nrow(train_data), samgyeopsal_lag + 1],
              exog_lag_list$Seongsu[1:nrow(train_data), seongsu_lag + 1],
              exog_lag_list$Myeongdong[1:nrow(train_data), myeongdong_lag + 1],
              exog_lag_list$Hangang[1:nrow(train_data), hangang_lag + 1]
            )
            
            colnames(exog_train) <- c(
              paste0("Seoul_lag", seoul_lag),
              paste0("Kdrama_lag", kdrama_lag),
              paste0("Samgyeopsal_lag", samgyeopsal_lag),
              paste0("Seongsu_lag", seongsu_lag),
              paste0("Myeongdong_lag", myeongdong_lag),
              paste0("Hangang_lag", hangang_lag)
            )
            
            # 결측값 처리 (lag로 인해 발생한 결측값 제거)
            exog_train <- fill_na_with_neighbors(exog_train)
            if (nrow(exog_train) == 0) next
            
            train_y <- train_data$Japan_Visitors[(nrow(train_data) - nrow(exog_train) + 1):nrow(train_data)]
            
            # 모델 적합
            fit <- tryCatch(auto.arima(as.numeric(train_y), xreg = as.matrix(exog_train), seasonal = TRUE), error = function(e) NULL)
            
            # 모델이 성공적으로 적합된 경우에만 예측 및 성능 평가
            if (!is.null(fit)) {
              exog_test <- data.frame(
                exog_test_lag_list$Seoul[1:nrow(test_data), seoul_lag + 1],
                exog_test_lag_list$Kdrama[1:nrow(test_data), kdrama_lag + 1],
                exog_test_lag_list$Samgyeopsal[1:nrow(test_data), samgyeopsal_lag + 1],
                exog_test_lag_list$Seongsu[1:nrow(test_data), seongsu_lag + 1],
                exog_test_lag_list$Myeongdong[1:nrow(test_data), myeongdong_lag + 1],
                exog_test_lag_list$Hangang[1:nrow(test_data), hangang_lag + 1]
              )
              
              colnames(exog_test) <- c(
                paste0("Seoul_lag", seoul_lag),
                paste0("Kdrama_lag", kdrama_lag),
                paste0("Samgyeopsal_lag", samgyeopsal_lag),
                paste0("Seongsu_lag", seongsu_lag),
                paste0("Myeongdong_lag", myeongdong_lag),
                paste0("Hangang_lag", hangang_lag)
              )
              
              # 결측값 처리 (lag로 인해 발생한 결측값 제거)
              exog_test <- fill_na_with_neighbors(exog_test)
              if (nrow(exog_test) == 0) next
              
              test_y <- test_data$Japan_Visitors[(nrow(test_data) - nrow(exog_test) + 1):nrow(test_data)]
              
              # 모델 예측
              forecast_values <- tryCatch(forecast(fit, xreg = as.matrix(exog_test), h = nrow(test_y))$mean, error = function(e) NULL)
              
              # 예측이 성공적으로 수행된 경우에만 RMSE 계산
              if (!is.null(forecast_values)) {
                rmse_value <- rmse(test_y, forecast_values)
                
                # 최적의 lag 업데이트
                if (rmse_value < best_rmse) {
                  best_rmse <- rmse_value
                  best_lags <- list(Seoul = seoul_lag, Kdrama = kdrama_lag, Samgyeopsal = samgyeopsal_lag, Seongsu = seongsu_lag, Myeongdong = myeongdong_lag, Hangang = hangang_lag)
                  print(paste("Lags - Seoul:", seoul_lag, "Kdrama:", kdrama_lag, "Samgyeopsal:", samgyeopsal_lag, "Seongsu:", seongsu_lag, "Myeongdong:", myeongdong_lag, "Hangang:", hangang_lag, "RMSE:", rmse_value))
                  
                }
              }
            }
          }
        }
      }
    }
  }
}

print(best_lags)
print(paste("Best RMSE:", best_rmse))

#만든 모델 사용하고 이후 값을 예측하는 작업
japan_visitors_ts <- ts(japan_visitors, start = c(2010, 1), frequency = 12)

# 데이터 필터링
train_data <- data %>% filter(Date <= as.Date("2023-12-01"))
test_data <- data %>% filter(Date >= as.Date("2024-01-01"))

# 종속 변수 (일본 방문객 수) 시계열 객체로 변환
train_y_ts <- ts(train_data$Japan_Visitors, start = c(2010, 1), frequency = 12)
test_y_ts <- ts(test_data$Japan_Visitors, start = c(2024, 1), frequency = 12)

# 외생 변수 데이터를 시계열 객체로 변환
seoul_ts <- ts(train_data$Seoul, start = c(2010, 1), frequency = 12)
kdrama_ts <- ts(train_data$Kdrama, start = c(2010, 1), frequency = 12)
samgyeopsal_ts <- ts(train_data$Samgyeopsal, start = c(2010, 1), frequency = 12)
seongsu_ts <- ts(train_data$Seongsu, start = c(2010, 1), frequency = 12)
myeongdong_ts <- ts(train_data$Myeongdong, start = c(2010, 1), frequency = 12)
hangang_ts <- ts(train_data$Hangang, start = c(2010, 1), frequency = 12)


# 가장 좋은 lag 조합
best_lags <- list(Seoul = 2, Kdrama = 4, Samgyeopsal = 3, Seongsu = 4, Myeongdong = 0, Hangang = 4)

# 종속 변수 (일본 방문객 수) 데이터를 ts 객체로 변환 (월별 데이터, 시작 연도와 월을 지정)
japan_visitors_ts <- ts(japan_visitors, start = c(2010, 1), frequency = 12)

# 수동으로 lag 값을 생성 (exog_lag_data 참고)
seoul_lag2 <- ts(c(33, 27, head(seoul_ts, -2)), start = c(2010, 1), frequency = 12)
kdrama_lag4 <- ts(c(37, 40, 38, 34, head(kdrama_ts, -4)), start = c(2010, 1), frequency = 12)
samgyeopsal_lag3 <- ts(c(9, 9, 9, head(samgyeopsal_ts, -3)), start = c(2010, 1), frequency = 12)
seongsu_lag4 <- ts(c(0, 0, 0, 0, head(seongsu_ts, -4)), start = c(2010, 1), frequency = 12)
myeongdong_lag0 <- myeongdong_ts
hangang_lag4 <- ts(c(31, 28, 19, 22, head(hangang_ts, -4)), start = c(2010, 1), frequency = 12)

# 외생 변수를 하나의 데이터프레임으로 결합
exog_train <- data.frame(
  Seoul = seoul_lag2,
  Kdrama = kdrama_lag4,
  Samgyeopsal = samgyeopsal_lag3,
  Seongsu = seongsu_lag4,
  Myeongdong = myeongdong_lag0,
  Hangang = hangang_lag4
)

# 결측값 제거
exog_train <- na.omit(exog_train)

# exog_train을 시계열 객체로 변환
exog_train_ts <- ts(exog_train, start = c(2010, 1), frequency = 12)

# 종속 변수 (일본 방문객 수) 설정
train_y <- train_y_ts[(length(train_y_ts) - nrow(exog_train_ts) + 1):length(train_y_ts)]

# SARIMAX 모델 적합
# 계절성 ARIMA 모델 적합
fit_seasonal <- Arima(train_y, order = c(3, 1, 3), seasonal = list(order = c(1, 1, 3), period = 12), xreg = as.matrix(exog_train_ts))

# 모델 요약 출력
summary(fit_seasonal)

# 테스트 데이터의 외생 변수 lag 생성
seoul_lag2_test <- ts(c(tail(seoul_ts, 2), head(test_data$Seoul, -2)), start = c(2024, 1), frequency = 12)
kdrama_lag4_test <- ts(c(tail(kdrama_ts, 4), head(test_data$Kdrama, -4)), start = c(2024, 1), frequency = 12)
samgyeopsal_lag3_test <- ts(c(tail(samgyeopsal_ts, 3), head(test_data$Samgyeopsal, -3)), start = c(2024, 1), frequency = 12)
seongsu_lag4_test <- ts(c(tail(seongsu_ts, 4), head(test_data$Seongsu, -4)), start = c(2024, 1), frequency = 12)
myeongdong_lag0_test <- ts(test_data$Myeongdong, start = c(2024, 1), frequency = 12)
hangang_lag4_test <- ts(c(tail(hangang_ts, 4), head(test_data$Hangang, -4)), start = c(2024, 1), frequency = 12)


# 외생 변수를 하나의 데이터프레임으로 결합
exog_test <- data.frame(
  Seoul = seoul_lag2_test,
  Kdrama = kdrama_lag4_test,
  Samgyeopsal = samgyeopsal_lag3_test,
  Seongsu = seongsu_lag4_test,
  Myeongdong = myeongdong_lag0_test,
  Hangang = hangang_lag4_test
)

# exog_test를 시계열 객체로 변환
exog_test_ts <- ts(exog_test, start = c(2024, 1), frequency = 12)

# 예측 수행
forecast_values <- forecast(fit_seasonal, xreg = as.matrix(exog_test_ts), h = length(test_y_ts))

# 예측값과 실제값 비교하여 RMSE 계산
# 예측값 추출
predicted_values <- as.numeric(forecast_values$mean)

# 예측값과 실제값 비교하여 RMSE 계산
rmse_value_seasonal <- rmse(as.numeric(test_y_ts), predicted_values)

# 결과 출력
print(paste("Test Data RMSE:", rmse_value_seasonal))


# 잔차 계산
residuals <- residuals(fit_seasonal)

# 잔차의 정규성 검정 - Q-Q plot
qqnorm(residuals)
qqline(residuals, col = "red")
title("Q-Q Plot of Residuals")

# 잔차의 ACF/PACF plot
par(mfrow = c(1, 2))
acf(residuals, main = "ACF of Residuals")
pacf(residuals, main = "PACF of Residuals")
par(mfrow = c(1, 1))

# Ljung-Box 테스트
# 보통 데이터 길이의 약 10%에서 15%를 lag 값으로 설정합니다
n <- length(residuals)
lag_value <- round(n * 0.1)  # 데이터 길이의 10%로 lag 설정
ljung_box_test <- Box.test(residuals, lag = lag_value, type = "Ljung-Box")
print(ljung_box_test)

# Shapiro-Wilk 정규성 테스트
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

# 잔차의 시계열 플롯
plot(residuals, type = "l", main = "Residuals over Time", ylab = "Residuals", xlab = "Time")

# 잔차의 히스토그램
hist(residuals, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")



library(forecast)




# 외생변수 데이터 (24년 1월 ~ 25년 12월)
future_exog <- data.frame(
  seoul = c(16, 18, 18, 16, 14, 13, 12, 12, 15, 15, 13, 11, 12, 13, 16, 15, 13, 11, 11, 11, 13, 14, 11, 9),
  kdrama = c(54, 57, 52, 46, 52, 49, 48, 50, 55, 50, 50, 44, 49, 54, 52, 52, 55, 52, 49, 51, 56, 51, 51, 45),
  samgyeopsal = c(76, 88, 88, 86, 92, 92, 91, 88, 88, 87, 88, 86, 88, 94, 94, 90, 97, 97, 96, 93, 93, 92, 93, 92),
  seongsu = c(43, 58, 75, 74, 70, 68, 68, 68, 70, 71, 75, 72, 71, 75, 74, 74, 74, 74, 74, 73, 76, 77, 80, 78),
  myeongdong = c(70, 94, 98, 89, 94, 99, 97, 101, 105, 103, 101, 92, 103, 111, 109, 99, 103, 106, 104, 108, 111, 111, 109, 103),
  hangang = c(13, 20, 23, 73, 52, 45, 42, 41, 40, 41, 39, 37, 38, 41, 42, 54, 49, 48, 48, 48, 47, 48, 46, 44)
)

# 수동으로 lag 값을 생성
seoul_lag2_future <- ts(c(17, 16, 16, 18, 18, 16, 14, 13, 12, 12, 15, 15, 13, 11, 12, 13, 16, 15, 13, 11, 11, 11, 13, 14), start = c(2024, 1), frequency = 12)
kdrama_lag4_future <- ts(c(64, 64, 63, 57, 54, 57, 52, 46, 52, 49, 48, 50, 55, 50, 50, 44, 49, 54, 52, 52, 55, 52, 49, 51), start = c(2024, 1), frequency = 12)
samgyeopsal_lag3_future <- ts(c(79, 82, 76, 88, 88, 86, 92, 92, 91, 88, 88, 87, 88, 86, 88, 94, 94, 90, 97, 97, 96, 93, 93, 92), start = c(2024, 1), frequency = 12)
seongsu_lag4_future <- ts(c(50, 48, 54, 38, 43, 58, 75, 74, 70, 68, 68, 68, 70, 71, 75, 72, 71, 75, 74, 74, 74, 74, 74, 73), start = c(2024, 1), frequency = 12)
myeongdong_lag0_future <- ts(future_exog$myeongdong, start = c(2024, 1), frequency = 12)
hangang_lag4_future <- ts(c(21, 21, 18, 14, 13, 20, 23, 73, 52, 45, 42, 41, 40, 41, 39, 37, 38, 41, 42, 54, 49, 48, 48, 48), start = c(2024, 1), frequency = 12)

# 미래 외생 변수를 하나의 데이터프레임으로 결합
exog_future <- data.frame(
  Seoul = seoul_lag2_future,
  Kdrama = kdrama_lag4_future,
  Samgyeopsal = samgyeopsal_lag3_future,
  Seongsu = seongsu_lag4_future,
  Myeongdong = myeongdong_lag0_future,
  Hangang = hangang_lag4_future
)

# exog_future를 시계열 객체로 변환
exog_future_ts <- ts(exog_future, start = c(2024, 1), frequency = 12)

# 예측 수행
forecast_values <- forecast(fit_seasonal, xreg = as.matrix(exog_future_ts), h = nrow(exog_future_ts))

# 예측값
predicted_values <- round(as.numeric(forecast_values$mean))

# 기존 데이터 (2010-01 ~ 2023-12)와 예측된 데이터 (2024-01 ~ 2025-12)를 결합
total_japan_visitors <- c(japan_visitors, predicted_values)

# 전체 데이터 시계열 객체로 변환
total_japan_visitors_ts <- ts(total_japan_visitors, start = c(2010, 1), frequency = 12)

# 예측값과 신뢰구간을 포함한 데이터프레임 생성
forecast_df <- data.frame(
  Date = seq(as.Date("2024-01-01"), by = "month", length.out = length(forecast_values$mean)),
  Forecast = forecast_values$mean,
  Lower = forecast_values$lower[, 2],
  Upper = forecast_values$upper[, 2]
)

# 기존 데이터와 예측 데이터를 결합
plot_df <- data.frame(
  Date = seq(as.Date("2010-01-01"), by = "month", length.out = length(total_japan_visitors)),
  Visitors = c(total_japan_visitors[1:length(japan_visitors)], rep(NA, length(predicted_values))),
  Type = c(rep("Observed", length(japan_visitors)), rep("Forecast", length(predicted_values)))
)

# 실제 관측치와 예측치를 결합하여 플롯 데이터프레임 생성
plot_df$Visitors[plot_df$Date >= as.Date("2024-01-01")] <- c(as.numeric(test_y_ts), predicted_values)

# 그래프 그리기
ggplot() +
  geom_line(data = plot_df[plot_df$Type == "Observed", ], aes(x = Date, y = Visitors), color = "blue", size = 1) +
  geom_line(data = plot_df[plot_df$Type == "Forecast", ], aes(x = Date, y = Visitors), color = "green", size = 1) +
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower, ymax = Upper), fill = "grey80", alpha = 0.5) +
  labs(title = "Japanese Visitors Forecast", x = "Year", y = "Number of Japanese Visitors") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  geom_vline(xintercept = as.Date("2024-01-01"), linetype = "dashed", color = "red") +
  geom_text(aes(x = as.Date("2024-01-01"), y = max(plot_df$Visitors, na.rm = TRUE), label = "Forecast Start"), vjust = -0.5, color = "red")





