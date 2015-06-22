## Helper functions for BlightSTAT.
## Run this before any of the other scripts

positionLabels <- function(dat, cats){
	#Finds the correct positions of labels in a stacked bar chart
	inds <- rep_len(1:cats, length.out = length(dat))
	pos <- dat[1] - (0.5 * dat[1])
	for(i in 1:length(dat)){
		location <- inds[i]
		if( location != 1){
			pos[i] <- sum(dat[1:location]) - (sum(0.5 * dat[location]))
		} else{
			pos[i] <- dat[location] - (0.5 * dat[location])
		}
		if(location == cats){dat <- dat[-(1:cats)]}
	}
	return(pos)
}


findDupes <- function(test.data, full.data){
	#Finds duplicates
	test <- rle(sort(full.data[full.data %in% test.data]))
	dupe.index <- which(test$lengths>1)
	dupes <- test$values[dupe.index]
	return(dupes)
}

legendCounts <- function(cats){
		cats.count <- table(cats)
		cats.final <- c()
		for(i in 1: length(cats)){
			count.ind <- which(names(cats.count) == cats[i])
			cats.final[i] <- paste0(cats[i], " (", cats.count[count.ind],")")
		}
		return(cats.final)
}
