#membaca data populasi
populasi = read.csv("data.csv", header=TRUE) #TRUE = baris pertama dibaca sebagai header 
populasi

#Cek dimensi
dim(populasi)

#banyaknya data
N = dim(populasi)[1]
N

#letak nomer jenis kelamin perempuan dan laki-laki
jk.pr = which(populasi[,2] == "Perempuan", arr.ind= TRUE) #[seluruh baris, kolom 2]
jk.pr

jk.lk = which(populasi[,2] == "Laki-Laki", arr.ind= TRUE) #[seluruh baris, kolom 2]
jk.lk

#populasi
pop.pr = populasi[jk.pr,]
pop.pr

pop.lk = populasi[jk.lk,]
pop.lk

# Mengetahui banyak laki dan perempuan
N1 = dim(pop.lk)[1]
N1
N2 = dim(pop.pr)[1]
N2

#cari np dan nl
n1 = N1/N*5
n1

n2 = N2/N*5
n2
#cara menentukan jumlah sampel perempuan dan laki-laki yang diingnkan
sample.lk = sample (jk.lk,nl)
sample.lk


sample.pr = sample (jk.pr,np)
sample.pr

#rata-rata tinggi 5 sampel
samplelk = populasi[sample.lk,]
samplelk

ratalk = mean(samplelk[,3])
ratalk

samplepr = populasi[sample.pr,]
samplepr

ratapr = mean(samplepr[,3])
ratapr

#1. dugaan rata-rata
Myu = ((N1*ratalk) + (N2*ratapr))/N
Myu

#BoE
error = 2*sqrt(Myu)
error

#2. Total
ragam.spr = var(samplepr[,3])
ragam.spr



ragam.slk = var(samplelk[,3])
ragam.slk

Tou = N1*(ratalk) + N2*(ratapr)
Tou

#BoE
error = 2*sqrt(Tou)
error

#3. n jika B rata-rata = 5
