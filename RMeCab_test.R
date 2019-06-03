# RMeCab test
# Calculate the similarity of two documents

# #install
# install.packages("RMeCab", repos = "http://rmecab.jp/R", type = "source") 

#read library
library(RMeCab)

# 1 text
samp_text<-RMeCabC("すもももももももものうち")

# Check data
print(samp_text)
samp_text<-unlist(samp_text)
print(samp_text)
print(as.character(samp_text))
length(samp_text)
samp_text_speech<-names(samp_text)

print(samp_text_speech)
print(table(samp_text_speech))

# 2 text
samp_text1<-RMeCabC("すもももももももものうち")
samp_text2<-RMeCabC("うちにはもももすももも無いよ")

samp_text1<-RMeCabC("海は広いし大きいし塩味がする")
samp_text2<-RMeCabC("空は青いな広いな、海に似ている")

samp_text_sum<-c(samp_text1,samp_text2)
samp_text_sum<-unlist(samp_text_sum)
s<-names(samp_text_sum)

samp_text_sum<-subset(samp_text_sum,s=="名詞"|s=="動詞"|s=="形容詞"|s=="形容動詞")

result1<-table(factor(samp_text1,levels=unique(samp_text_sum)))
result2<-table(factor(samp_text2,levels=unique(samp_text_sum)))

as.numeric(result1)
as.numeric(result2)

# Make dataframe
df<-data.frame(as.numeric(result1),as.numeric(result2))
print(df)
names(df)<-c("t1","t2")
rownames(df)<-unique(samp_text_sum)
print(df)
df<-t(df)
View(df)


# Similarity
# easy sample 簡単例データ
#df<-matrix(c(1,3,2,1,3,2),nrow=2)

#Inner product 内積
as.numeric(df[1,] %*% df[2,])
#Cosine similarity　コサイン類似度
as.numeric(df[1,] %*% df[2,])/sqrt(sum(df[1,]^2)*sum(df[2,]^2))
#Euclidean distance ユークリッド距離
sqrt(sum((df[1,]-df[2,])^2))
#Squared Euclidean distance 平方ユークリッド距離
sum((df[1,]-df[2,])^2)
#Manhattan distance マンハッタン距離
sum(abs(df[1,]-df[2,]))

#Inner product after normalization=Cosine similarity 正規化してから内積=コサイン類似度
df2<-df/apply(df,1,function(x)sqrt(sum(x^2)))
print(df2)
as.numeric(df2[1,] %*% df2[2,])
