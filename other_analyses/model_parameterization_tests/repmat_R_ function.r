repmat = function(X,m,n){
##R equivalent of repmat (matlab)
mx = dim(X)[1]
nx = dim(X)[2]
matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)}

#from http://haky-functions.blogspot.com/2006/11/repmat-function-matlab.html