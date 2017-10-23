# Loading libraries
require(reshape2)
require(circlize)
require(dplyr)
require(magrittr)
# Loading dataset
source("df.R")

mat<-as.matrix(df)
tratamientos<-apply(mat[,1:4],1,function(x)paste0(x,collapse="_"))
df_trat<-data.frame("Tratamientos"=tratamientos,df[,5:8])
mat<-dcast(df_trat,formula=Tipo_de_permanencia~Tratamientos,value.var="Permanencia_promedio")

# Código para heatmap
col_fun = colorRamp2(c(-2, 0, 2), c("green", "black", "red"))
factors = df$Perturbación

mat_list<-lapply(levels(factors),function(x)mat%>%select(contains(x)))
names(mat_list)<-c("a","b","d","e")
dend_list = list(a = as.dendrogram(hclust(dist(t(mat_list[["a"]])))),
                 b = as.dendrogram(hclust(dist(t(mat_list[["b"]])))),
                 d = as.dendrogram(hclust(dist(t(mat_list[["d"]])))),
                 e = as.dendrogram(hclust(dist(t(mat_list[["e"]]))))	)

circos.par(cell.padding = c(0, 0, 0, 0), gap.degree = 5)
circos.initialize(factors, xlim = cbind(c(0, 0), table(factors)))
circos.track(ylim = c(0, 10), bg.border = NA, panel.fun = function(x, y) {
    sector.index = CELL_META$sector.index
    m = mat_list[[sector.index]]
    dend = dend_list[[sector.index]]

    m2 = m[, order.dendrogram(dend)]
    col_mat = col_fun(m2)
    nr = nrow(m2)
    nc = ncol(m2)
    for(i in 1:nr) {
        circos.rect(1:nc - 1, rep(nr - i, nc), 
            1:nc, rep(nr - i + 1, nc), 
            border = col_mat[i, ], col = col_mat[i, ])
    }
})
