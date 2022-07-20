
#####################

#install.packages("imager")
library('imager')
#install.packages("OpenImageR")
library('OpenImageR')

fpath <- system.file('extdata/parrots.png',package='imager')
papugi <- readImage(fpath)
class(papugi)
writeImage(papugi,'new_papugi.png')
imageShow(papugi)
#####################
#####################

R=papugi[,,1]
G=papugi[,,2]
B=papugi[,,3]
papugi_dim <- dim(papugi)

#head(papugi,10)
#tail(papugi,10)

d = expand.grid(x = 1:512, y = 1:768)
a=d[,1]
b=d[,2]
c=c(a,b,c(R),c(G),c(B))
Dane_papugi=matrix(c,ncol=5)
#head(Dane_papugi)
#tail(Dane_papugi)



imgRGB <- data.frame(
  x = rep(1:768, each = 512),
  y = rep(512:1, 768),
  R = c(R),
  G = c(G),
  B = c(B)
)
head(imgRGB)

dim(imgRGB)

#####################
#####################


kolorki0=Dane_papugi[,3:5]
kolorki1=imgRGB[,3:5]

####



#####################
#####################

#install.packages("dplyr")
library("dplyr")

f_join=full_join(df33aa,df33bb)
head(f_join)
mpty_M<- as.matrix(f_join[2:4])
#dim(mpty_M)
araj <- array(mpty_M, dim = c(512, 768, 3))
imageShow(araj)





#######################
####### test  2
#######################
kmin=kmeans(kolorki1,2)


real_join=kmin$centers[kmin$cluster,]

mpty_M<- as.matrix(real_join[,1:3])
#dim(mpty_M)
araj <- array(mpty_M, dim = c(512, 768, 3))
imageShow(araj)
writeImage(papugi,'new_papugi2.png')
#######################
####### test  16
#######################
kmin=kmeans(kolorki1,16)

real_join=kmin$centers[kmin$cluster,]

mpty_M<- as.matrix(real_join[,1:3])
#dim(mpty_M)
araj <- array(mpty_M, dim = c(512, 768, 3))
imageShow(araj)


#######################
####### test 32
#######################
kmin=kmeans(kolorki1,32)

real_join=kmin$centers[kmin$cluster,]
head(f_join)
mpty_M<- as.matrix(real_join[,1:3])
#dim(mpty_M)
araj <- array(mpty_M, dim = c(512, 768, 3))
imageShow(araj)

#######################
####### test 64
#######################
kmin=kmeans(kolorki1,64)

real_join=kmin$centers[kmin$cluster,] # <-- real centers

mpty_M<- as.matrix(real_join[,1:3])
#dim(mpty_M)
araj <- array(mpty_M, dim = c(512, 768, 3))
imageShow(araj)

#######################
####### test  256
#######################
kmin=kmeans(kolorki1,256)

real_join=kmin$centers[kmin$cluster,]

mpty_M<- as.matrix(real_join[,1:3])
#dim(mpty_M)
araj <- array(mpty_M, dim = c(512, 768, 3))
imageShow(araj)