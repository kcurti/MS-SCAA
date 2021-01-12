function(name,L)
{
  n = nchar(name)
  if(substring(name,n-3,n)==".pin")
    file_name = name
  else
    file_name = paste(name,".pin",sep="")

  cat("# \"",name,".pin\" produced by Kpin_write() from ADMButils; ",date(),"\n", file=file_name,sep="")
  for(i in 1:length(L))
  {
    x = L[[i]]

    if(data.class(x)=="character")
      cat("#",L[[i]],"\n\n",file=file_name,append=T)


    if(data.class(x)=="numeric")
      cat("#",names(L)[i],"\n",L[[i]],"\n\n",file=file_name,append=T)

    if(data.class(x)=="matrix")
    {
      cat("#",names(L)[i],"\n",file=file_name,append=T)
      write.table(L[[i]],,col=F,row=F,quote=T,file=file_name,append=T)
      cat("\n",file=file_name,append=T)
    }
    if(data.class(x)=="array")
    {
      #SHIT - ONLY GIVING 15 ROWS
      cat("#",names(L)[i],"\n",file=file_name,append=T)
      write.table(L[[i]],eol="\r",col=F,row=F,quote=T,file=file_name,append=T)
      cat("\n",file=file_name,append=T)
    }
  }
}
