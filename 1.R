library(fmsb)
result_2010_1 <- matrix(Won=final_result_2010[1,c(5,12)], 
                            Draw=final_result_2010[1,c(6,13)],
                            Lost=final_result_2010[1,c(7,14)],
                            Goal_For=final_result_2010[1,c(8,15)],
                            Goal_Against=final_result_2010[1,c(8,15)])

RNGkind("Mersenne-Twister")

op <- par(mar=c(1,2,2,1),mfrow=c(2,2))
radarchart(result_2010_1,axistype=2,pcol=topo.colors(2),plty=1,
           title="Argentina vs .Mexico")
radarchart(dat,axistype=3,pty=32,plty=1,axislabcol="grey",na.itp=FALSE,title="(no points, axis=3, na.itp=FALSE)")
radarchart(dat,axistype=0,plwd=1:5,pcol=1,title="(use lty and lwd but b/w, axis=0)")
par(op)