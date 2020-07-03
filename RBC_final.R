#wgranie biblioteki
library(EBImage)


im_num = length(list.files("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/RBC's_exp/EBimage_RBC/images/2020.06.2911-57niebieski"))
c_num = 1

#pętla, przetwarzająca obrazy w folderze X
while (c_num <= im_num) { 
#read image
file = file.path("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/RBC's_exp/EBimage_RBC/images/2020.06.2911-57niebieski", 
paste(c_num, "_phase.tif", sep=""))

#wczytanie obrazu
I = readImage(file)
#wyświetlenie obrazu
display (I, method='raster')

#progowanie adaptacyjne
BW = thresh(I, w=40, h=40, offset=0.09)
display(BW, method='raster')

#wyznaczenie parametrów obiektów
BW_label = bwlabel(BW)
maxbw = max(BW_label)
compute1 = computeFeatures.shape (BW_label)
compute2 = computeFeatures.moment (BW_label)

#operacja morfologiczna zamknięcia
brush = makeBrush(3, shape='box')
BW_label = closing(BW_label,brush)
BW_label = bwlabel(BW_label)
maxbw = max(BW_label)
compute1 = computeFeatures.shape (BW_label)
compute2 = computeFeatures.moment (BW_label)
display(BW_label, method='raster')

#operacja morfologiczna wypełniania dziur
BW_label = fillHull(BW_label)
BW_label = bwlabel(BW_label)
maxbw = max(BW_label)
compute1 = computeFeatures.shape (BW_label)
compute2 = computeFeatures.moment (BW_label)
display(BW_label, method='raster')

#określenie kryterium pola powierzchni
SmallArea = 0.65*(mean(compute1[,1]))

#usunięcie obiektów na podstawie kryterium wielkości
for (i in 1:(maxbw)) {
	if (compute1[i,1]<SmallArea) {
	ImageNew = BW_label !=i
	BW_label = ImageNew*BW_label
	}
}
BW_label = bwlabel(BW_label)
maxbw = max(BW_label)
compute1 = computeFeatures.shape (BW_label)
compute2 = computeFeatures.moment (BW_label)
#usunięcie okrągłych obiektów
for (i in 1:(maxbw)) {
ratio = compute1[i, 6] / compute1[i, 5]
	if (ratio<0.6) {
	ImageNew = BW_label !=i
	BW_label = ImageNew*BW_label
	}
}
BW_label=bwlabel(BW_label)
maxbw = max(BW_label)
compute1 = computeFeatures.shape (BW_label)
compute2 = computeFeatures.moment (BW_label)
compute3 = computeFeatures.basic (BW_label, I)
display(BW_label, method='raster')

#usunięcie obiektów na podstawie jednorodnej intensywności
for (i in 1:maxbw) {
	if ((compute3[i,2]/compute3[i,1]*100) < 18) {
	ImageNew = BW_label !=i
	BW_label = ImageNew*BW_label
	}
}
BW_label=bwlabel(BW_label)
maxbw = max(BW_label)
compute1 = computeFeatures.shape (BW_label)
compute2 = computeFeatures.moment (BW_label)
compute3 = computeFeatures.basic (BW_label, I)
display(BW_label, method='raster')

##usunięcie obiektów na podstawie ekcentryczności
#for (i in 1:maxbw) {
#	if (compute2[i,4]<0.1) {
#	ImageNew = BW_label !=i
#	BW_label = ImageNew*BW_label
#	}
#}
BW_label=bwlabel(BW_label)
maxbw = max(BW_label)
compute1 = computeFeatures.shape (BW_label)
compute2 = computeFeatures.moment (BW_label)
compute3 = computeFeatures.basic (BW_label, I)

display(BW_label, method='raster')

#otworzenie biblioteki OpenImageR
library(OpenImageR)

#przekształcenie obiektów do postaci macierzy
BW_label = imageData(BW_label)
#funkcja zamieniająca radiany na stopnie
rad2deg=function(rad){(rad*180)/(pi)}

name='Image'
ImageList=list()



BW_Head_label=BW_label
compute1 = computeFeatures.shape (BW_Head_label)
compute2 = computeFeatures.moment (BW_Head_label)
maxbw2 = max(BW_Head_label)



BW_Head_label=bwlabel(BW_Head_label)
compute1 = computeFeatures.shape (BW_Head_label)
compute2 = computeFeatures.moment (BW_Head_label)
maxbw2=max(BW_Head_label)

#dodatkowe usunięcie obiektów na podstawie ich wielkości
#for (i in 1:(maxbw)) {
#	if (compute1[i,1]<900 | compute1[i,1]>1600) {
#	ImageNew = BW_Head_label !=i
#	BW_Head_label = ImageNew*BW_Head_label
#	}
#}
BW_Head_label = bwlabel(BW_Head_label)
maxbw2 = max(BW_Head_label)
compute1 = computeFeatures.shape (BW_Head_label)
compute2 = computeFeatures.moment (BW_Head_label)
display(BW_Head_label, method='raster')

##dodanie skali szarości do główek plemników
ImageNew = BW_Head_label
ImageNew[which(BW_Head_label>=1)] = 1
Gray_Head = ImageNew*I
display(Gray_Head, method='raster') 
Gray_Head = imageData(Gray_Head)

#stworzenie folderu do przechowywania danych
folderpath = file.path("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/RBC's_exp/EBimage_RBC/images/2020.06.2911-57niebieski",paste("Folder",c_num,sep=""))
dir.create(folderpath)
pathImage = file.path(paste(folderpath), paste("GrayImage",c_num, ".png", sep=""))
writeImage(Gray_Head, file=pathImage)

#pętla, w której tworzony jest profil wysokości
for (i in 1:maxbw2) {
angle= compute2[i, 5] 
b=compute2[i,2] - tan(angle)*compute2[i,1]
xc1=compute2[i,1] - ((compute2[i,3]/2)-3)*cos(angle)
xc1=xc1
xc1=round(xc1)
xc2=compute2[i,1] + ((compute2[i,3]/2)-3)*cos(angle)
xc2=xc2
xc2=round(xc2)
substr=xc2-xc1

	if (substr<30) {
	angle = compute2[i, 5] 
	yc1= compute2[i,2] - ((compute2[i,3]/2)-3)*sin(angle)
	yc1= yc1
	yc1= round(yc1)
	yc2 = compute2[i,2] + ((compute2[i,3]/2)-3)*sin(angle)
	yc2 = yc2
	yc2 = round(yc2)

	vectory = c(yc1:yc2)
	vectorx = sapply(vectory, function(vectory) x = (vectory-b)/(tan(angle)))

	dimension = length(vectory)
	vectorvalue = array(0, dim = dimension)
	j=1

		for (n in 1:dimension) {
		value = Gray_Head[vectorx[n], vectory[n]]
		vectorvalue[j] = value
		j = j+1
		}
	
      	mypath = file.path(paste(folderpath),paste("plot_", i, ".pdf", sep = ""))
	pdf(file=mypath, width=5, height=5)
	plot(vectorvalue, type='o', xlab="Distance", ylab="Intensity value", axes = FALSE)
	axis(1, seq(0,dimension,1))
	axis(2, seq(0,1, 0.1))
	dev.off()

	} else { vectorx=c(xc1:xc2)
	vectory = sapply(vectorx, function(vectorx) y = tan(angle)*vectorx+b)

	dimension=length(vectorx)
	vectorvalue=array(0, dim=dimension)
	j=1

		for (n in 1:dimension) {
		value=Gray_Head[vectorx[n], vectory[n]]
		vectorvalue[j] = value
		j=j+1
		}
	
	mypath = file.path(paste(folderpath),paste("plot_", i, ".pdf", sep = ""))
	pdf(file=mypath, width=5, height=5)
	plot(vectorvalue, type='o', xlab="Distance", ylab="Intensity value", axes = FALSE)
	axis(1, seq(0,dimension,1))
	axis(2, seq(0,1, 0.1))
	dev.off()
	}
}

timestamp <- read.table("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/RBC's_exp/EBimage_RBC/images/2020.06.2911-57niebieski/timestamps.txt", 
                 header = FALSE)

time <- data.frame(timestamp)

time_capt <- c(round(time[c_num,c("V4")],digits=2))




matrix2  = as.data.frame(compute2)
xpos=c(round(matrix2$m.cx*103.6/800, digits =3))
ypos=c(round(matrix2$m.cy*103.6/800, digits =3))








#zapisanie wyników pomiarów w folderze
matrix  = as.data.frame(compute1)
radius.min = c(round(matrix$s.radius.min*103.6/800, digits =2))
radius.max = c(round(matrix$s.radius.max*103.6/800, digits=2))
 radius.mean= c(round(matrix$s.radius.mean*103.6/800, digits=2))
radius.sd=c(round(matrix$s.radius.sd*103.6/800, digits=2))
perimeter = c(round(matrix$s.perimeter*103.6/800, digits=2))
area = c(round(matrix$s.area*(103.6/800)^2, digits=2))
df = data.frame(xpos, ypos, time_capt, radius.min,radius.max,radius.mean,radius.sd,perimeter,area)
path2 = file.path(paste("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/RBC's_exp/EBimage_RBC/images/2020.06.2911-57niebieski/tables"), paste("table_", c_num, ".csv", sep=""))
write.csv(df, file=path2)

c_num = c_num+1

}

im_num2 = length(list.files("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/RBC's_exp/EBimage_RBC/images/2020.06.2911-57niebieski/tables"))
c_num2=1
while (c_num2 <= im_num2) { 
#read image
import_1 = file.path("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/RBC's_exp/EBimage_RBC/images/2020.06.2911-57niebieski/tables", 
paste("table_",c_num2, ".csv", sep=""))
import_2 = file.path("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/RBC's_exp/EBimage_RBC/images/2020.06.2911-57niebieski/tables", 
paste("table_",c_num2+1, ".csv", sep=""))

import_data1<-read.table(import_1, header = TRUE, sep=",", stringsAsFactors = FALSE)

import_data2<-read.table(import_2, header = TRUE, sep=",", stringsAsFactors = FALSE)


delta_x<-abs(import_data2[1,2]-import_data1[1,2])
delta_y<-abs(import_data2[1,3]-import_data1[1,3])
delta_t<-abs(import_data2[1,4]-import_data1[1,4])

V_x0<-(delta_x/(delta_t*10^-3))
V_y0<-(delta_y/(delta_t*10^-3))

V_x=c(round(V_x0, digits =4))
V_y=c(round(V_y0, digits =4))
print(V_x,V_y)


final1<-data.frame(V_x)
print(final1, quote = FALSE)
final2<-data.frame(V_y)
print(final2, quote = FALSE)

out <- capture.output(final1)

cat(out, file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/RBC's_exp/EBimage_RBC/images/2020.06.2911-57niebieski/speed_V_x.txt", sep=";", append=TRUE)
out2 <- capture.output(final2)

cat(out2, file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/RBC's_exp/EBimage_RBC/images/2020.06.2911-57niebieski/speed_V_y.txt", sep=";", append=TRUE)


graphics.off()
c_num2=c_num2+1
}
#-----------------------------------------------------------------------------------------------------Koniec--------




#--------------------------------------------------------------------------------------
lin <- data.frame(x = seq(1,67,1), y=c(1.4444     , 1.4222   , 0.62     , 1.6444   , 0.89     , 1.5444     , 1.6444   , 1.23     , 0.9778     , 0.8889   , 0.93  , 0.8     , 0.7333   , 1.15     , 0.5889   , 0.07     , 1.9556     , 0.7111   , 1.97     , 1.1778     , 1.8444   , 1.94     , 0.3444     , 1.1111   , 0.78     , 2.1667     , 1.3444   , 0.52     , 1.9222   , 2.05     , 0.9222     , 0.1556  ,   2     , 1.1333     , 1.2667   , 3.22     , 1.4667     , 0.3555   , 0.82     , 0.0889   , 1.07     , 1.5778     , 0.3222   , 1.87     , 1.0222  , 0.7   , 0.32     , 4.4667     , 5.6222   , 1.75     , 0.3444     , 7.2778   , 3.71     , 0.7333   , 1.68     , 0.9556     , 0.0556   , 1.74  , 2.3     , 1.0444   , 0.64     , 2.6111     , 1.3222   , 0.03     , 0.7333   , 0.88     , 1.5778))


reduced_lin <- data.frame(x = seq(1,33,1), y=c(1.4444     , 1.4222   , 0.62     , 1.6444   , 0.89     , 1.5444     , 1.6444   , 1.23     , 0.9778     , 0.8889   , 0.93  , 0.8     , 0.7333   , 1.15     , 0.5889   , 0.07     , 1.9556     , 0.7111   , 1.97     , 1.1778     , 1.8444   , 1.94     , 0.3444     , 1.1111   , 0.78     , 2.1667     , 1.3444   , 0.52     , 1.9222   , 0.03     , 0.7333   , 0.88     , 1.5778))




lin2 <- data.frame(x = seq(1,67,1), y=c(3.1222     , 1.9444   , 0.57     , 2.0111   , 0.66     , 1.8222     , 3.1333   , 0.28  , 2.1     , 1.9444   , 0.42  , 0.7     , 0.8111   , 0.86     , 0.6556   , 0.66     , 2.0667     , 0.5111   , 1.35     , 2.0889  , 2.2  , 1.8     , 0.4889     , 1.7222   , 2.15     , 1.6111     , 2.2222   , 0.93     , 2.5667   , 0.88     , 1.9222     , 0.6889   , 0.44     , 1.3333  , 0.8   , 2.35     , 0.4556     , 0.9221   , 1.15     , 1.0889   , 1.27     , 0.5333  , 1.3   , 1.45     , 1.5556     , 1.2333   , 0.32     , 5.9333  , 5.3   , 2.12     , 2.4333     , 1.2333   , 5.38     , 0.8111   , 0.82     , 1.0778     , 0.1444   , 0.03     , 0.4556     , 1.0111   , 0.04     , 0.3667  , 1.3   , 0.41     , 0.5778   , 1.93     , 0.9111))






reduced_lin2 <- data.frame(x = seq(1,33,1), y=c(3.1222     , 1.9444   , 0.57     , 2.0111   , 0.66     , 1.8222     , 3.1333   , 0.28  , 2.1     , 1.9444   , 0.42  , 0.7     , 0.8111   , 0.86     , 0.6556   , 0.66     , 2.0667     , 0.5111   , 1.35     , 2.0889  , 2.2  , 1.8     , 0.4889     , 1.7222   , 2.15     , 1.6111     , 2.2222   , 0.93     , 2.5667   , 0.88     ,  0.5778   , 1.93     , 0.9111))








----------------------------------------------------------------------------------
library(ggplot2)


Vx<-lin$y
Vy<-lin2$y
Vx_reduced<-reduced_lin$y
Vy_reduced<-reduced_lin2$y

#--------------------------------------------------------------------full_range_gaphs-------------------------------------------------------------------

pdf(file = "/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/RBC's_exp/EBimage_RBC/images/2020.06.2911-57niebieski/speed_V_x_density.pdf", width=4, height=4)
ggplot(lin, aes(x=Vx)) + 
 geom_histogram(aes(y=..density..), colour="black", fill="white")+
 geom_density(fill="gray",alpha=.5)+
  geom_vline(aes(xintercept=mean(Vx)), color="black",
             linetype="dashed",alpha=.5)+annotate(geom="text", x = mean(lin$y), y = 0.75, colour = "black",label=paste(round(mean(lin$y),digits=2)," %+-%  ", round(sd(lin$y),digits=2)    ), parse = TRUE)+

 xlab(expression(V[x]*" ["*mu*m/s*"]"))+ylab("Density")+
  theme_bw(base_size = 15)+theme(axis.text.x = element_text(colour="black"),axis.text.y = element_text(colour="black"))


dev.off()


pdf(file = "/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/RBC's_exp/EBimage_RBC/images/2020.06.2911-57niebieski/speed_V_y_density.pdf", width=4, height=4)
ggplot(lin2, aes(x=Vy)) + 
 geom_histogram(aes(y=..density..), colour="black", fill="white")+
 geom_density(fill="gray",alpha=.5)+
  geom_vline(aes(xintercept=mean(Vy)), color="black",
             linetype="dashed",alpha=.5)+annotate(geom="text", x = mean(lin2$y), y = 0.75, colour = "black",label=paste(round(mean(lin2$y),digits=2)," %+-%  ", round(sd(lin2$y),digits=2)    ), parse = TRUE)+



 xlab(expression(V[y]*" ["*mu*m/s*"]"))+ylab("Density")+
 theme_bw(base_size = 15)+theme(axis.text.x = element_text(colour="black"),axis.text.y = element_text(colour="black"))




dev.off()


pdf(file = "/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/RBC's_exp/EBimage_RBC/images/2020.06.2911-57niebieski/speed_V_x_count.pdf", width=4, height=4)
ggplot(lin, aes(x=Vx)) + 
 geom_histogram(aes(y=..count..), colour="black", fill="white")+
 geom_density(fill="gray",alpha=.5)+
  geom_vline(aes(xintercept=mean(Vx)), color="black",
             linetype="dashed",alpha=.5)+annotate(geom="text", x = mean(lin$y), y = 12, colour = "black",label=paste(round(mean(lin$y),digits=2)," %+-%  ", round(sd(lin$y),digits=2)    ), parse = TRUE)+


 xlab(expression(V[x]*" ["*mu*m/s*"]"))+ylab("Count")+
  theme_bw(base_size = 15)+theme(axis.text.x = element_text(colour="black"),axis.text.y = element_text(colour="black"))




dev.off()


pdf(file = "/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/RBC's_exp/EBimage_RBC/images/2020.06.2911-57niebieski/speed_V_y_count.pdf", width=4, height=4)
ggplot(lin2, aes(x=Vy)) + 
 geom_histogram(aes(y=..count..), colour="black", fill="white")+
 geom_density(fill="gray",alpha=.5)+
  geom_vline(aes(xintercept=mean(Vy)), color="black",
             linetype="dashed",alpha=.5)+annotate(geom="text", x = mean(lin2$y), y = 11.5, colour = "black",label=paste(round(mean(lin2$y),digits=2)," %+-%  ", round(sd(lin2$y),digits=2)    ), parse = TRUE)+


 xlab(expression(V[y]*" ["*mu*m/s*"]"))+ylab("Count")+
 theme_bw(base_size = 15)+theme(axis.text.x = element_text(colour="black"),axis.text.y = element_text(colour="black"))
dev.off()

#-------------------------------------------------------------------------------
#------------------------------------------reduced_range_graphs----------------------------------------------------------



pdf(file = "/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/RBC's_exp/EBimage_RBC/images/2020.06.2911-57niebieski/speed_V_x_reduced_density.pdf", width=4, height=4)
ggplot(reduce_lin, aes(x=Vx_reduced)) + 
 geom_histogram(aes(y=..density..), colour="black", fill="white")+
 geom_density(fill="gray",alpha=.5)+
  geom_vline(aes(xintercept=mean(Vx_reduced)), color="black",
             linetype="dashed",alpha=.5)+annotate(geom="text", x = mean(reduced_lin$y), y = 1.4, colour = "black",label=paste(round(mean(reduced_lin$y),digits=2)," %+-%  ", round(sd(reduced_lin$y),digits=2)    ), parse = TRUE)+


 xlab(expression(V[x]*" ["*mu*m/s*"]"))+ylab("Density")+
  theme_bw(base_size = 15)+theme(axis.text.x = element_text(colour="black"),axis.text.y = element_text(colour="black"))


dev.off()


pdf(file = "/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/RBC's_exp/EBimage_RBC/images/2020.06.2911-57niebieski/speed_V_y_reduced_density.pdf", width=4, height=4)
ggplot(reduced_lin2, aes(x=Vy_reduced)) + 
 geom_histogram(aes(y=..density..), colour="black", fill="white")+
 geom_density(fill="gray",alpha=.5)+
  geom_vline(aes(xintercept=mean(Vy_reduced)), color="black",
             linetype="dashed",alpha=.5)+annotate(geom="text", x = mean(reduced_lin2$y), y = 1.4, colour = "black",label=paste(round(mean(reduced_lin2$y),digits=2)," %+-%  ", round(sd(reduced_lin2$y),digits=2)    ), parse = TRUE)+



 xlab(expression(V[y]*" ["*mu*m/s*"]"))+ylab("Density")+
 theme_bw(base_size = 15)+theme(axis.text.x = element_text(colour="black"),axis.text.y = element_text(colour="black"))




dev.off()


pdf(file = "/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/RBC's_exp/EBimage_RBC/images/2020.06.2911-57niebieski/speed_V_x_reduced_count.pdf", width=4, height=4)
ggplot(reduce_lin, aes(x=Vx_reduced)) + 
 geom_histogram(aes(y=..count..), colour="black", fill="white")+
 geom_density(fill="gray",alpha=.5)+
  geom_vline(aes(xintercept=mean(Vx_reduced)), color="black",
             linetype="dashed",alpha=.5)+annotate(geom="text", x = mean(reduced_lin$y), y = 3.5, colour = "black",label=paste(round(mean(reduced_lin$y),digits=2)," %+-%  ", round(sd(reduced_lin$y),digits=2)    ), parse = TRUE)+

 xlab(expression(V[x]*" ["*mu*m/s*"]"))+ylab("Count")+
  theme_bw(base_size = 15)+theme(axis.text.x = element_text(colour="black"),axis.text.y = element_text(colour="black"))


dev.off()


pdf(file = "/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/RBC's_exp/EBimage_RBC/images/2020.06.2911-57niebieski/speed_V_y_reduced_count.pdf", width=4, height=4)
ggplot(reduced_lin2, aes(x=Vy_reduced)) + 
 geom_histogram(aes(y=..count..), colour="black", fill="white")+
 geom_density(fill="gray",alpha=.5)+
  geom_vline(aes(xintercept=mean(Vy_reduced)), color="black",
             linetype="dashed",alpha=.5)+annotate(geom="text", x = mean(reduced_lin2$y), y = 4.5, colour = "black",label=paste(round(mean(reduced_lin2$y),digits=2)," %+-%  ", round(sd(reduced_lin2$y),digits=2)    ), parse = TRUE)+


 xlab(expression(V[y]*" ["*mu*m/s*"]"))+ylab("Count")+
 theme_bw(base_size = 15)+theme(axis.text.x = element_text(colour="black"),axis.text.y = element_text(colour="black"))




dev.off()














#-------------------------------------------------------------------------------
pdf(file = "/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/RBC's_exp/EBimage_RBC/images/2020.06.2911-57niebieski/qqplot_V_x.pdf", width=5, height=5)
ggplot(data = lin, mapping = aes(sample=y)) +     stat_qq_band(alpha=.5) +     stat_qq_line() +     stat_qq_point() +    labs(x = "Theoretical Quantiles", y = (expression(V[x]*" ["*mu*m/s*"]")))+
theme_bw(base_size = 15)+theme(axis.text.x = element_text(colour="black"),axis.text.y = element_text(colour="black"))

dev.off()
pdf(file = "/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/RBC's_exp/EBimage_RBC/images/2020.06.2911-57niebieski/qqplot_V_y.pdf", width=5, height=5)
ggplot(data = lin2, mapping = aes(sample=y)) +      stat_qq_band(alpha=.5) +     stat_qq_line() +     stat_qq_point() +     labs(x = "Theoretical Quantiles", y = (expression(V[y]*" ["*mu*m/s*"]")))+
theme_bw(base_size = 15)+theme(axis.text.x = element_text(colour="black"),axis.text.y = element_text(colour="black"))
dev.off()

#for FILE in `ls`; do mv $FILE `echo $FILE | sed -e 's:^0*::'`; done







