gpb.args.check <-
function(pp, aval, bval, wts)
{ 
   if(is.null(pp)|any(pp<0)|any(pp>1))
 {
    stop("invalid values in pp.")
 }
 
 if(!is.null(aval))
 {
   if(length(aval)!=length(pp) | any(round(aval)!=aval))
   {
      stop("aval must have the same length as pp and must be integer!")     
   }
 }

 if(!is.null(bval))
 {
   if(length(bval)!=length(pp) | any(round(bval)!=bval))
   {
      stop("bval must have the same length as pp and must be integer!")     
   }
 }
 
 if(!is.null(wts))
 {
   if(length(wts)!=length(pp) | any(round(wts)!=wts))
   {
      stop("wts must have the same length as pp and must be integer!")     
   }
 }

 if(any(aval>bval))
 {
   stop("aval can not be larger than bval, switch aval and bval and set pp to 1-pp for those with aval larger than bval!")
 }
}

pgpb <-
function(kk, pp, aval, bval, wts=NULL)
{
 if(is.null(kk)| any(kk!=as.integer(kk)))
 {
    stop("invalid values in kk.")
 }

 gpb.args.check(pp=pp, aval=aval, bval=bval, wts=wts)

 if(is.null(wts))
 {
    wts=rep(1,length(pp))
 }

 if(is.null(aval))
 {
    aval=rep(0, length(pp))
 }

 if(is.null(bval))
 {
    bval=rep(1, length(pp))
 }

 mm=length(kk)
 res=double(mm)

 npp=length(pp)

 asum=sum(wts*aval)
 bsum=sum(wts*bval)
             
 n=sum(bsum-asum)
 kk.a=kk-asum
 
 aval.a=aval#-asum
 bval.a=bval#-asum

 avec=double(n+1)
 bvec=double(n+1)

 funcate=1
 ex=0

 tmp=.C("gpb_dft_cf",as.double(res),as.integer(kk.a),as.integer(mm),as.integer(n),as.double(pp),as.double(avec),as.double(bvec),as.integer(funcate),as.double(ex),as.integer(npp),as.integer(wts), as.integer(aval.a), as.integer(bval.a), as.integer(asum), PACKAGE="GPB")

 res=tmp[[1]]

 res[kk<asum]=0
 res[kk>=bsum]=1

 return(res)
}

dgpb <-
function(kk, pp, aval, bval, wts=NULL)
{
 if(is.null(kk)| any(kk!=as.integer(kk)))
 {
    stop("invalid values in kk.")
 }

 gpb.args.check(pp=pp, aval=aval, bval=bval, wts=wts)

 if(is.null(wts))
 {
    wts=rep(1,length(pp))
 }

 if(is.null(aval))
 {
    aval=rep(0, length(pp))
 }

 if(is.null(bval))
 {
    bval=rep(1, length(pp))
 }


 mm=length(kk)
 res=double(mm)

 npp=length(pp)

 asum=sum(wts*aval)
 bsum=sum(wts*bval)
                        
 n=sum(bsum-asum)
 kk.a=kk-asum
 
 aval.a=aval#-asum
 bval.a=bval#-asum

 avec=double(n+1)
 bvec=double(n+1)

 funcate=2
 ex=0

 tmp=.C("gpb_dft_cf",as.double(res),as.integer(kk.a),as.integer(mm),as.integer(n),as.double(pp),as.double(avec),as.double(bvec),as.integer(funcate),as.double(ex),as.integer(npp),as.integer(wts), as.integer(aval.a), as.integer(bval.a), as.integer(asum), PACKAGE="GPB")

 res=tmp[[1]]

 res[res<0]=0
 res[kk<asum|kk>bsum]=0

 return(res)
}


qgpb <-
function(qq, pp, aval, bval, wts=NULL)
{
 if(any(qq<0)|any(qq>1))
 {
    stop("invalid values in qq.")
 }

 gpb.args.check(pp=pp, aval=aval, bval=bval, wts=wts)

 if(is.null(wts))
 {
    wts=rep(1,length(pp))
 }

 if(is.null(aval))
 {
    aval=rep(0, length(pp))
 }

 if(is.null(bval))
 {
    bval=rep(1, length(pp))
 }

 nn=1
 mm=length(qq)
 res=double(mm)

 npp=length(pp)

 asum=sum(wts*aval)
 bsum=sum(wts*bval)    

 n=sum(bsum-asum)
 
 aval.a=aval#-asum
 bval.a=bval#-asum

 avec=double(n+1)
 bvec=double(n+1)

 funcate=3
 ex=qq

 tmp=.C("gpb_dft_cf",as.double(res),as.integer(nn),as.integer(mm),as.integer(n),as.double(pp),as.double(avec),as.double(bvec),as.integer(funcate),as.double(ex),as.integer(npp),as.integer(wts), as.integer(aval.a), as.integer(bval.a), as.integer(asum), PACKAGE="GPB")

 res=tmp[[1]]

 return(res)
}

rgpb <-
function(m, pp, aval, bval, wts=NULL)
{
 qq=runif(m)
 res=qgpb(qq=qq, pp=pp, aval=aval, bval=bval, wts=wts)
 return(res)
}

