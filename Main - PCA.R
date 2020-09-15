################################################################################################################
## Data Analysis: PCA
################################################################################################################

#install.packages('factoextra')
library(factoextra)
library("corrplot")
#preparing the data
PCA.m <- select(comp.m,input.school,input.student)
PCA.e <- select(comp.e, input.school,input.student)

#Find the means and variance of data
apply(PCA.m,2,mean)
apply(PCA.m,2,var)

#performing PCA
PCA.m[is.na(PCA.m)] <- 0
PCA.m.out=prcomp(PCA.m, scale=TRUE)

#conducting PCA
biplot(PCA.m.out,scale=0)
#s.d. of each principal component

# variance explained by each principal component
PCA.m.var=PCA.m.out$sdev^2
PCA.m.var

#explained variation in proportion
m.pve=PCA.m.var/sum(PCA.m.var)


plot(m.pve,xlab="Principal Component", ylab="Proportion of Variance Explained",ylim=c(0,1),type='b')

plot(cumsum(m.pve),xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained",ylim=c(0,1),type='b')


# percentage of explained variance
fviz_eig(PCA.m.out)
# graph
fviz_pca_var(PCA.m.out,select.var= list(cos2 = 5),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(PCA.m.out, select.var= list(cos2 = 5))
# Controbution of variable to different principal component
fviz_contrib(PCA.m.out, choice = "var", axes = 1, top = 8)
fviz_contrib(PCA.m.out, choice = "var", axes = 2, top = 8)


