#membaca data populasi
populasi = read.csv("data.csv", header=TRUE) #TRUE = baris pertama dibaca sebagai header 
populasi

#Cek dimensi
dim(populasi)

#cara generate angka random
n = 5
id <- sample (1:35,n) #dari data 1 sampai 35, yang diambil sebanyak n
id

#banyaknya data
N = dim(populasi)[1]
N

#cara menetapkan sampelnya
smpl <- populasi[id,] #id ngambil dari dara random sample
smpl

#1 menduga rata2 populasi
mean(smpl[,3]) #yang diambil semua baris, dan di kolom ke 3

#2 menduga total populasi
totalp <- N*mean(smpl[,3])
totalp

#ragam sample
var(smpl[,3])

#3 dugaan ragam rata2 populasi
ratas <- mean (smpl[,3])
ratas
sum<- 0
for (i in 1:n) { #berjalan dari sampel pertama sampe ke n
  sum <- sum +((smpl[i,3]-ratas)*(smpl[i,3]-ratas))
}

s_kuadrat_s=sum/(n-1)

vr = (s_kuadrat_s/n)*((N-n)/N)
vr


#4 dugaan ragam total populasi 
ratap <- mean (populasi[,3])
ratap
temp <- 0
for (i in 1:N) { #berjalan dari sampel pertama sampe ke n
  temp <- temp +((populasi[i,3]-ratap)*(populasi[i,3]-ratap))
}

s_kuadrat_p=temp/N

vt = N*N*(s_kuadrat_p/n)*((N-n)/N)
vt

#5 BoE Rata2 populasi
error_R<-2*sqrt(vr)
error_R

#6 BoE Total populasi
error_T <- 2*sqrt(vt*N*N)
error_T

#7 Rentang rata2
rentang_rata_populasi<-c((ratas-error_R),(ratas+error_R))
rentang_rata_populasi

#8 Rentang total populasi
rentang_total_populasi<-c((totalp-error_T),(totalp+error_T))
rentang_total_populasi

#9 n, jika B=10, untuk M ragam = 70
B = 10
Mu = 70
d = B^2 /4
d

n= N*Mu / (((N-1)*d)+Mu)
n
