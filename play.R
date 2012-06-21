### DEFAULT SYMBOLS AND COLORS ###
xsym = 4
xcol = "red"
osym = 1
ocol = "blue"

### ADVANCED CUSTOMIZATION EXAMPLE 1 ###
# xsym = "e"; osym = -0x03C0 # e vs pi

### ADVANCED CUSTOMIZATION EXAMPLE 2 ###
# xsym = -0x2665 ### 0x2665 is Unicode for Heart
# xcol = "red"
# osym = -0x263B ### 0x263B Smiley Face
# ocol = "purple"

### For more Unicode representations of symbols visit http://www.ssec.wisc.edu/~tomw/java/unicode.html
### For more information about colors in R visit http://research.stowers-institute.org/efg/R/Color/Chart/
### Don't forget to add a negative sign in front of the Unicode. P.S. Not all symbols show up.

play = function(n=4,xw=0,ow=0,xname="X",oname="O",debug=FALSE) {
	## These lines ask the players to input their names.
	# if ( xname == "X" ) {
		# xname = readline("Please type Xs Player's name: ")
	# }
	# if ( oname == "O" ) {
		# oname = readline("Please type Os Player's name: ")
	# }
	game.over = FALSE; turn = 0
	x.moves = NULL; o.moves = NULL
	error = ""; status = paste("What is ",oname,"'s move?",sep="")
	x.wins = xw; o.wins = ow
	check.victory = function(df,p) {
		test = t(apply(df,1,function(x,y){return(x-y);},y=p))
		if ( sum(apply(test,1,function(x){return(identical(c(x[2],x[3]),c(0,0)));}))==(n-1) ) return(TRUE)
		indices = as.numeric(which(test[,1]==0))
		if ( length(indices) == 1 ) coplanar = t(as.matrix(df[indices,],ncol=3))
		else coplanar = df[indices,]
		indices = as.numeric(which(test[,1]!=0))
		if ( length(indices) == 1 ) noncoplanar = t(as.matrix(df[indices,],ncol=3))
		else noncoplanar = df[indices,]
		if ( dim(coplanar)[1] != 0 ) {
			test.cp = t(apply(coplanar,1,function(x,y){return(x-y);},y=p))
			if ( debug ) print("Checking for coplanar victory")
			if( !is.null(dim(test.cp[test.cp[,2]==0,])) ) {
				if (debug) { print("...checking horizontal..."); }
				if( dim(test.cp[test.cp[,2]==0,])[1]==(n-1) ) return(TRUE) # H	
				if (debug) { print("Nothing horizontally..."); }
			}
			if( !is.null(dim(test.cp[test.cp[,3]==0,])) ) {
				if (debug) { print("...checking vertical..."); }
				if( dim(test.cp[test.cp[,3]==0,])[1]==(n-1) ) return(TRUE) # V
				if (debug) { print("Nothing vertically..."); }
			}
			if (debug) { print("...checking diagonal..."); }
			if( sum(apply(test.cp,1,function(x){return(x[2]==x[3]);}))==(n-1) ) return(TRUE) # D
			if( sum(apply(test.cp,1,function(x){return(x[2]==-x[3]);}))==(n-1) ) return(TRUE) # D
			if (debug) { print("Nothing diagonally..."); }
		}
		if ( dim(noncoplanar)[1] != 0 ) {
			if (debug) { print("Checking for crossplanar victory..."); }
			test.ncp = t(apply(noncoplanar,1,function(x,y){return(x-y);},y=p))
			if (debug) { print("...checking horizontal..."); }
			test.h.pos = apply(test.ncp,1,function(x){return(x[2]==0&&x[1]==x[3]);})
			test.h.neg = apply(test.ncp,1,function(x){return(x[2]==0&&x[1]==-x[3]);})
			if (debug) { print("...checking vertical..."); }
			test.v.pos = apply(test.ncp,1,function(x){return(x[3]==0&&x[1]==x[2]);})
			test.v.neg = apply(test.ncp,1,function(x){return(x[3]==0&&x[1]==-x[2]);})
			if (debug) { print("...checking diagonal..."); }
			test.d1 = apply(test.ncp,1,function(x,y){return(x[2]==x[3]&&x[1]==x[2]);},y=p)
			test.d2 = apply(test.ncp,1,function(x,y){return(x[2]==-x[3]&&x[1]==-x[2]);},y=p)
			test.d3 = apply(test.ncp,1,function(x,y){return(x[2]==-x[3]&&x[1]==x[2]);},y=p)
			test.d4 = apply(test.ncp,1,function(x,y){return(x[2]==x[3]&&x[1]==-x[2]);},y=p)
			if ( sum(test.h.pos)==(n-1) ) {
				if ( are.unique(test.ncp[test.h.pos,1]) ) return(TRUE) # H
				else if ( debug ) { print("not unique"); }
			}
			if ( sum(test.h.neg)==(n-1) ) {
				if ( are.unique(test.ncp[test.h.neg,1]) ) return(TRUE) # H
				else if ( debug ) { print("not unique"); }
			}
			if (debug) { print("Nothing horizontally..."); }
			if ( sum(test.v.pos)==(n-1) ) {
				if ( are.unique(test.ncp[test.v.pos,1]) ) return(TRUE) # H
				else if ( debug ) { print("not unique"); }
			}
			if ( sum(test.v.neg)==(n-1) ) {
				if ( are.unique(test.ncp[test.v.neg,1]) ) return(TRUE) # H
				else if ( debug ) { print("not unique"); }
			}
			if (debug) { print("Nothing vertically...."); }
			if ( sum(test.d1)==(n-1) ) return(TRUE) # D
			if ( sum(test.d2)==(n-1) ) return(TRUE) # D
			if ( sum(test.d3)==(n-1) ) return(TRUE) # D
			if ( sum(test.d4)==(n-1) ) return(TRUE) # D
			if (debug) { print("Nothing diagonally..."); }
		}
		if (debug) { print(paste("End of turn",turn)); }
		return(FALSE)
	}
	are.unique = function(x) {
		for ( i in 1:length(x) ) if ( x[i] %in% x[-i] ) { return(FALSE); }
		return(TRUE)
	}
	display = function(n,xs,os,status,error="") {
		if(!is.null(dev.list())) dev.off()
		w = n^2+(n-1)
		h = 2*n-1
		dev.new(width=w, height=h,units="in", res=72)
		#windows(width=w,height=h)
		plot(bty="n",x=0,y=0,xlim=c(0,w),type="n",mar=c(0,0,0,0),ylim=c(0,n),yaxt="n",xaxt="n",xlab=error,ylab="",main=status)		
		for ( i in 1:n ) {
			startX = (n+1)*(i-1)
			rect(startX,0,((n+1)*i)-1,n,lwd=2)
			for ( j in 1:n ) {
				for ( k in 1:n ) {
					replicate(n^2,rect(startX+j-1,k-1,startX+j,k))
				}
			}
		}
		if ( !is.null(os) ) {
			for ( i in 1:dim(os)[1] ) {
				X = (n+1)*(os[i,1]-1)+os[i,3]-1
				Y = (n)-os[i,2]
				points(X+.5,Y+.5,pch=osym,lwd=2,cex=2,col=ocol)
			}
		}
		if ( !is.null(xs) ) {
			for ( i in 1:dim(xs)[1] ) {
				X = (n+1)*(xs[i,1]-1)+xs[i,3]-1
				Y = (n)-xs[i,2]
				points(X+.5,Y+.5,pch=xsym,lwd=2,cex=2,col=xcol)
			}
		}
		L = locator(1); X = floor(L$x); Y = floor(L$y); return(c(X,Y))
	}
	while(!game.over) {
		input = display(n=n,xs=x.moves,os=o.moves,status,error)
		X = input[1]; Y = input[2]; w = n^2+(n-1)
		if ( X < 0 || X > w || length(which(seq(n,w,(n+1))==X))!=0 || Y < 0 || Y > n ) {
			error = "Invalid move."
		} else {
			error = ""
			point = c((X%/%(n+1))+1,n-Y,1+X-((X%/%(n+1))*(n+1)))
			turn = turn + 1
			m = NULL
			if ( turn %% 2 == 0 ) {
				m = x.moves
				status = paste("What is ",oname,"'s move?",sep="")
			} else {
				m = o.moves
				status = paste("What is ",xname,"'s move?",sep="")
			}
			if ( turn > 2 ) {
				if ( sum(apply(rbind(x.moves,o.moves),1,function(x,y){return(identical(x,y));},y=point))==0 ) {
					game.over = check.victory(df=m,p=point)
					if ( game.over ) {
						if ( turn %% 2 == 0 ) { status = paste(xname,"wins!"); x.moves = rbind(x.moves,point); x.wins = x.wins + 1; }
						else { status = paste(oname,"wins!"); o.moves = rbind(o.moves,point); o.wins = o.wins + 1; }
						status = paste(status," (",xname,": ",x.wins," & ",oname,":",o.wins,")",sep="")
						error = "Click inside any of the boxes to play again."
						L = display(n=n,xs=x.moves,os=o.moves,status,error)
						play(n,xw=x.wins,ow=o.wins,xname,oname)
					}
					if ( turn %% 2 == 0 ) x.moves = rbind(x.moves,point)
					else o.moves = rbind(o.moves,point)
					error = ""
				} else {
					turn = turn - 1
					if ( status == paste("What is ",oname,"'s move?",sep="") ) status = paste("What is ",xname,"'s move?",sep="")
					else status = paste("What is ",oname,"'s move?",sep="")
					error = "That space is taken. Please try again."
				}
			} else if ( turn == 2 ) {
				if (identical(as.numeric(o.moves[1,]),point)) {
					turn = turn - 1
					status = paste("What is ",xname,"'s move?",sep="")
					error = "That space is taken. Please try again."
				} else {
					status = paste("What is ",oname,"'s move?",sep="")
					x.moves = rbind(x.moves,point)
					error = ""
				}
			} else {
				if ( turn %% 2 == 0 ) x.moves = rbind(x.moves,point)
				else o.moves = rbind(o.moves,point)
			}
		}
	}
}
