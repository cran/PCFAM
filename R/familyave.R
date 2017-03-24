familyave <-
function(Xall,myfam,top=5){
   Xall = t(scale(t(Xall)))
    corX = fastcov(scale(Xall))
    #myfam = findfamilies(corX)
    avecor = rowMeans(corX)
    temp = tapply(avecor, myfam, which.min)
    familyID = as.vector(names(temp))
    best = rep(0, length(familyID))
    family.matrix = matrix(0, dim(Xall)[1], length(familyID))
    for (j in (1:length(familyID))) {
        best[j] = grep(T, myfam == familyID[j])[temp[j]]
        family.all = Xall[, grep(T, myfam == familyID[j])]
        new.vector = rowMeans(family.all)
        mu = mean(new.vector)
        new.vector = (new.vector - mu) * mean(apply(family.all, 
            2, sd)) + mu
        family.matrix[, j] = new.vector
    }
    index.sing = grep(T, myfam == 0)
    Xun_update = cbind(Xall[, index.sing], family.matrix[, -1])
    Xun_update = scale(Xun_update)
    proj_update.svd = svd(Xun_update)
    proj_updateV = t(((diag(1/proj_update.svd$d)) %*% t(proj_update.svd$u) %*% 
        Xall)[1:top, ])
    return(familyave.pc = proj_updateV)
}
