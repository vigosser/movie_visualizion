# install.packages(c("kknn", "readxl", "sampling","openxlsx","readxl","ggplot2","factoextra","xlsx”))
library(kknn)
library(ggplot2)
library(factoextra)
library(readxl)
library(xlsx)

# 檔案要改成存放的路徑
movice<- read.xlsx("C:\\Users\\vigosser\\Desktop\\R\\r_project\\tmdb_5000_movies_cut.xlsx",
                   sheetIndex = 1, row.names=1)# full path
sam<-sample(c(1:4000),200) # 从前4000中挑100
# samp可以用来选择
movice.samp=movice[sam,c(2,4,5)]


#df=scale(movice.samp)
df=movice.samp

fviz_nbclust(na.omit(df), kmeans, method = "wss") + geom_vline(xintercept = 3, linetype = 2)

#可以发现聚为四类最合适，当然这个没有绝对的，从指标上看，选择坡度变化不明显的点最为最佳聚类数目。
#设置随机数种子，保证实验的可重复进行
set.seed(123)
#利用k-mean是进行聚类
km_result <- kmeans(na.omit(df), 3, nstart = 24)
#查看聚类的一些结果
print(km_result)
#提取类标签并且与原始数据进行合并
dd <- cbind(na.omit(movice.samp), cluster = km_result$cluster)
head(dd)

#查看每一类的数目
table(dd$cluster)

#进行可视化展示
fviz_cluster(km_result, data = na.omit(df),
             #palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07","#004E07"),
             #palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
             ellipse.type = "euclid",
             star.plot = TRUE, 
             repel = TRUE,
             ggtheme = theme_minimal()
)



result <- dist(df, method = "euclidean")
#产生层次结构
result_hc <- hclust(d = result, method = "ward.D2")
#进行初步展示
fviz_dend(result_hc, cex = 0.6)
fviz_dend(result_hc, k = 4, 
          cex = 0.5, 
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, 
          rect = TRUE          
)

