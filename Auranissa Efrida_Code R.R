df <- as.data.frame(new)
df
df$sex <- as.factor(df$sex)
y <- cbind(df$trestbps,df$chol,df$thalch)
totalmeans <- colMeans(y); totalmeans
df.group <- split(df[,2:4], df$sex)

n <- dim(df)[1] / length(unique(df$sex))

df.means <- sapply(df.group, function(x) {
  apply(x, 2, mean)
}, simplify = 'data.frame')

df.means

H = matrix(data = 0, nrow = 3, ncol = 3)
for (i in 1:dim(H)[1]) {
  for (j in 1:i) {
    H[i,j] <- n * sum((df.means[i,] - totalmeans[i]) * (df.means[j,] - totalmeans[j]))
    H[j,i] <- n * sum((df.means[j,] - totalmeans[j]) * (df.means[i,] - totalmeans[i]))
  }
}
H

E = matrix(data = 0, nrow = 3, ncol = 3)
for (i in 1:dim(E)[1]) {
  for (j in 1:i) {
    b <- c() 
    for (k in df.group) {
      a <- sum((k[,i] - mean(k[,i])) * (k[,j] - mean(k[,j])))
      b <- append(b, a)
    }
    E[i,j] <- sum(b)
    E[j,i] <- sum(b)
  }
}
E
#WILKS
Lambda <- det(E)/det(E + H); Lambda
summary(manova(y ~ df$sex), 'Wilks')$stats[,2][1]
#F test
df.manova <- summary(manova(y ~ df$sex))
df.manova
1
#ROy
EginvH.eigen <- eigen(ginv(E) %*% H)
roy.stat <- EginvH.eigen$values[1]; roy.stat
summary(manova(y ~ df$sex), 'Roy')$stats[,2][1]
theta <- roy.stat/(1+roy.stat); theta

#Pillai
pillai.stat <- sum(diag(ginv(E + H) %*% H)); pillai.stat
sum(EginvH.eigen$values / (1 + EginvH.eigen$values))
summary(manova(y ~ df$sex), 'Pillai')$stats[,2][1]

#Lawley-Hotelling Test
lawhot.stat <- sum(diag(ginv(E) %*% H)); lawhot.stat
sum(EginvH.eigen$values)
summary(manova(y ~ df$sex), 'Hotelling-Lawley')$stats[,2][1]

##Notes : Mohon maaf kak, saya menggunakan fungsi ginv dikarenakan di R saya tidak bisa 
#menginstall package matlib. Saya sudah mencoba beberapa kali dengan cara yang berbeda,
#menginstall ulang R, tetapi tetap tidak bisa dengan notes R versi saya tidak mendukung.

