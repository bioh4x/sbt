
# head -n 1000 Data\ 013118/29062017-28012018/*-part.csv | grep 'iOS' | tr ',' '\t' | tr ' ' '_' | awk -F '\t' '{print $1, $4, $5, $30, $31, $9, $10, $11, $12}' | tr ' ' '\t' > example_10k.tsv    
# cat Data\ 013118/29062017-28012018/*-part.csv | grep 'iOS' | tr ',' '\t' | tr ' ' '_' | awk -F '\t' '{print $1, $4, $5, $30, $31}' | tr ' ' '\t' > example_all.tsv    

library(gplots)
library(colorRamps)

setwd ("/Users/apple/Desktop/devel.SBT")
allinput <- read.table("example_10k.tsv", sep="\t", header=FALSE, fill=TRUE)
# biginput <- read.table("example_all.tsv", sep="\t", header=FALSE, fill=TRUE)


# dim(allinput)
# [1] 27765     5
# dim(allinput[ allinput$V4 != "" , ])
# [1] 13616     5
#  dim(allinput[ allinput$V5 != "" , ])
# [1] 5880    5


subset <- allinput[ allinput$V5 != "" , ]
emotion_matrix_pre <- strsplit(as.character(subset$V4),'\\|')
emotion_matrix_post <- strsplit(as.character(subset$V5),'\\|')

png(filename="emotion_matrix_pre.png", width = 800, height = 1800 )
plot.new()
par(mar = c(2, 10, 2, 2))
barplot( sort(table(unlist(emotion_matrix_pre))) , horiz = TRUE, las=1,
         main="Most Abundantly Expressed Emotions (5880 randomly selected cases with pre- and post- mediation emotions.)\nPRE MEDIATION")
dev.off()
 
png(filename="emotion_matrix_post.png", width = 800, height = 1800 )
plot.new()
par(mar = c(2, 10, 2, 2))
barplot( sort(table(unlist(emotion_matrix_post))) , horiz = TRUE, las=1,
         main="Most Abundantly Expressed Emotions (5880 randomly selected cases with pre- and post- mediation emotions.)\nPOST MEDIATION")
dev.off()

emotion_abund_matrix_pre <- matrix(nrow=length(subset$V1),ncol=length(unique(unlist(emotion_matrix_pre))))
colnames(emotion_abund_matrix_pre) <- unique(unlist(emotion_matrix_pre))
rownames(emotion_abund_matrix_pre) <- subset$V1
ri <- 1
for(r in subset$V1){
  for(s in unique(unlist(emotion_matrix_pre))){
    emotion_abund_matrix_pre[ri,s] <- 0
  }
  ri <- ri + 1
}
ri <- 1
for(r in subset$V1){
  for(q in unlist(emotion_matrix_pre[[ri]])){
    emotion_abund_matrix_pre[ri,q] <- emotion_abund_matrix_pre[ri,q] + 1
  }
  ri <- ri + 1
}

emotion_abund_matrix_post <- matrix(nrow=length(subset$V1),ncol=length(unique(unlist(emotion_matrix_post))))
colnames(emotion_abund_matrix_post) <- unique(unlist(emotion_matrix_post))
rownames(emotion_abund_matrix_post) <- subset$V1
ri <- 1
for(r in subset$V1){
  for(s in unique(unlist(emotion_matrix_post))){
    emotion_abund_matrix_post[ri,s] <- 0
  }
  ri <- ri + 1
}
ri <- 1
for(r in subset$V1){
  for(q in unlist(emotion_matrix_post[[ri]])){
    emotion_abund_matrix_post[ri,q] <- emotion_abund_matrix_post[ri,q] + 1
  }
  ri <- ri + 1
}

png(filename="emotion_abund_matrix_pre.heatmap.png", width = 1400, height = 1400 )
heatmap.2( 
  emotion_abund_matrix_pre[rowSums(emotion_abund_matrix_pre) > 0,],
  col=c(rev(gray.colors(150)),matlab.like2(150)),
  trace="none",
  margins = c(15,15),
  keysize=0.7, key.par = list(cex=0.5),
  main="Pre-meditation Emotion Scores from 4,638 Sampling that also have Post-Mediation Emotions"
)
dev.off()

library(vegan)
dist.bray<-vegdist(t(emotion_abund_matrix_pre), method="bray") # creates a Bray-Curtis dissimilarity matrix from the species data.frame. adding “pa” , would make the matrix unweighted (presence absence). Other
png(filename="dist.bray.pre.png", width = 1400, height = 1400 )
heatmap.2( 
  as.matrix(dist.bray),
  col=c(rev(gray.colors(150)),matlab.like2(150)),
  trace="none",
  margins = c(15,15),
  keysize=0.7, key.par = list(cex=0.5),
  main="Pre-meditation Emotion Scores from 4,638 Sampling that also have Post-Mediation Emotions - Bray-Curtis Distance Matrix"
)
dev.off()
 
library(ape)
pcoa.dist<-pcoa(dist.bray) 
png(filename=paste0("pcoa.pre.png"), width = 1400, height = 1400 )
plot(pcoa.dist$vectors[,1:2],
     #bg=col_type,
     #bg=alpha(col_type, 0.6),
     #col="white",
     #pch=pch_type,
     #cex=2,
     #alpha = 0.6,
     xlab=paste("PCoA.1(",round(100*pcoa.dist$values[1,3], digits=2),"%)"), 
     ylab=paste("PCoA.2(",round(100*pcoa.dist$values[2,3], digits=2),"%)"),
     main=paste0("Pre-meditation Emotion Scores from 4,638 Sampling that also have Post-Mediation Emotions - Bray-Curtis Distance - PCoA\n")
)
  
#if(dim(pcoa.dist$vectors)[1] <= 100){
  text( pcoa.dist$vectors, rownames( as.matrix(dist.bray) ), pos=3, col="blue", cex=0.6 )
#  text( pcoa.dist$vectors, as.character(unlist(groups[col_name])), pos=1, col="darkgray", cex=0.7 )
#}
dev.off()
 



png(filename="emotion_abund_matrix_post.heatmap.png", width = 1400, height = 1400 )
heatmap.2( 
  emotion_abund_matrix_post[rowSums(emotion_abund_matrix_post) > 0,],
  col=c(rev(gray.colors(150)),matlab.like2(150)),
  trace="none",
  margins = c(15,15),
  keysize=0.7, key.par = list(cex=0.5),
  main="Pre-meditation Emotion Scores from 4,638 Sampling that also have Post-Mediation Emotions"
)
dev.off()
 
library(vegan)
dist.bray<-vegdist(t(emotion_abund_matrix_post), method="bray") # creates a Bray-Curtis dissimilarity matrix from the species data.frame. adding “pa” , would make the matrix unweighted (postsence absence). Other
png(filename="dist.bray.post.png", width = 1400, height = 1400 )
heatmap.2( 
  as.matrix(dist.bray),
  col=c(rev(gray.colors(150)),matlab.like2(150)),
  trace="none",
  margins = c(15,15),
  keysize=0.7, key.par = list(cex=0.5),
  main="Post-meditation Emotion Scores from 4,638 Sampling that also have Post-Mediation Emotions - Bray-Curtis Distance Matrix"
)
dev.off()

library(ape)
pcoa.dist<-pcoa(dist.bray) 
png(filename=paste0("pcoa.post.png"), width = 1400, height = 1400 )
plot(pcoa.dist$vectors[,1:2],
     #bg=col_type,
     #bg=alpha(col_type, 0.6),
     #col="white",
     #pch=pch_type,
     #cex=2,
     #alpha = 0.6,
     xlab=paste("PCoA.1(",round(100*pcoa.dist$values[1,3], digits=2),"%)"), 
     ylab=paste("PCoA.2(",round(100*pcoa.dist$values[2,3], digits=2),"%)"),
     main=paste0("Post-meditation Emotion Scores from 4,638 Sampling that also have Post-Mediation Emotions - Bray-Curtis Distance - PCoA\n")
)

#if(dim(pcoa.dist$vectors)[1] <= 100){
text( pcoa.dist$vectors, rownames( as.matrix(dist.bray) ), pos=3, col="blue", cex=0.6 )
#  text( pcoa.dist$vectors, as.character(unlist(groups[col_name])), pos=1, col="darkgray", cex=0.7 )
#}
dev.off()









png(filename="emotion_abund_matrix_post.heatmap.png", width = 1400, height = 1400 )
heatmap.2( 
  emotion_abund_matrix_post[rowSums(emotion_abund_matrix_post) > 0,],
  col=c(rev(gray.colors(150)),matlab.like2(150)),
  trace="none",
  margins = c(15,15),
  keysize=0.7, key.par = list(cex=0.5),
  main="4,638 Pre-meditation Emotions - Sampling that also have Post-Mediation Emotions"
)
dev.off()



