#functions to estimate the skeleton of an image
#based on 
# //*********************************************************************************************************
# //R Image Thinning Library 
# //Created By - Jake Drew, Dr. Michael Hahsler 
# //Version -    1.0, 04/28/2013
# //*********************************************************************************************************

# code adapted using skeletonization table from ImageJ skeletonize function:
# https://imagej.nih.gov/ij/source/ij/process/BinaryProcessor.java
  

absDiff <- function(matrix1,matrix2){

  r <- nrow(matrix1)
  c <- ncol(matrix1)
  destMatrix <- matrix1
  for(r in 0:r-1)
  {
    for(c in 0:c-1)
    {
      destMatrix[r,c] <- abs(matrix1[r,c]-matrix1[r,c])
    }
  }
  return(destMatrix)
}



countNonZero <- function(inputMatrix)
{
  return(length(inputMatrix[inputMatrix > 0]))
}

thinningIteration <- function(imageMatrix, iter,table){

  imageInput <- imageMatrix
  r <- nrow(imageInput) - 1
  c <- ncol(imageInput) - 1
  for(i in 2:r)  {
    for(j in 2:c)   {
      p5 <- imageInput[i,j]
      if (p5==1){
        
        # p6 <- imageInput[i-1, j]
        # p9 <- imageInput[i-1, j+1]
        # p8 <- imageInput[i, j+1]
        # p7 <- imageInput[i+1, j+1]
        # p4 <- imageInput[i+1, j]
        # p1 <- imageInput[i+1, j-1]
        # p2 <- imageInput[i, j-1]
        # p3 <- imageInput[i-1, j-1]
        
        p1 <- imageInput[i-1, j-1]
        p2 <- imageInput[i-1, j]
        p3 <- imageInput[i-1, j+1]
        p4 <- imageInput[i, j-1]
        p6 <- imageInput[i, j+1]
        p7 <- imageInput[i+1, j-1]
        p8 <- imageInput[i+1, j]
        p9 <- imageInput[i+1, j+1]

        
        index <- 1
        if (p1!=0) index <- index+1
        if (p2!=0) index <- index+2
        if (p3!=0) index <- index+4
        if (p6!=0) index <- index+ 8
        if (p9!=0) index <- index+16
        if (p8!=0) index <- index+ 32
        if (p7!=0) index <- index+ 64
        if (p4!=0) index <- index+ 128
       # cat(index)
      #  cat("/n")
        code = table[index]
        if (iter==0) { #odd pass
          if (code==2||code==3) {
            imageMatrix[i,j] <- 0
            #pixelsRemoved++;
          }
        } else { #even pass
          if (code==1||code==3) {
            imageMatrix[i,j] <- 0
            #pixelsRemoved++;
          }
        }
      }
      
      
      #     A  <- (p2 == 0 && p3 == 1) + (p3 == 0 && p4 == 1) + 
      #       (p4 == 0 && p5 == 1) + (p5 == 0 && p6 == 1) + 
      #       (p6 == 0 && p7 == 1) + (p7 == 0 && p8 == 1) +
      #       (p8 == 0 && p9 == 1) + (p9 == 0 && p2 == 1)
      #     B  <- p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9
      #     if(iter == 0){
      #       m1 <- (p2 * p4 * p6)
      #       m2 <- (p4 * p6 * p8)
      #     }
      #     else {
      #       m1 <- (p2 * p4 * p8)
      #       m2 <- (p2 * p6 * p8)
      #     }
      #     if (A == 1 && (B >= 2 && B <= 6) && m1 == 0 && m2 == 0)
      #     {
      #       imageInput[i,j] <- 0
      #     }
      #   }
      # }
      
    }
  }
  return(imageMatrix)
}


thinImage <- function(imageMatrix){
  UseMethod("thinImage")
}

thinImage.default <- function(imageMatrix)
{
  table  <- c(0,0,0,0,0,0,1,3,0,0,3,1,1,0,1,3,0,0,0,0,0,0,0,0,0,0,2,0,3,0,3,3,
              0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,3,0,2,2,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              2,0,0,0,0,0,0,0,2,0,0,0,2,0,0,0,3,0,0,0,0,0,0,0,3,0,0,0,3,0,2,0,
              0,0,3,1,0,0,1,3,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
              3,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              2,3,1,3,0,0,1,3,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              2,3,0,1,0,0,0,1,0,0,0,0,0,0,0,0,3,3,0,1,0,0,0,0,2,2,0,0,2,0,0,0)
  
  table2  <- c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,2,2,0,0,0,0,
               0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,2,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,
               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
               0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,
               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,
               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
               0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  
  
  im <- imageMatrix
  prev <- im
#  count <- 0
  repeat {
    im <- thinningIteration(im, 1,table)
    im <- thinningIteration(im, 0,table)
    diff <- prev-im
   # count <- count+1
   # cat(count)
    prev <- im
    if(countNonZero(diff) <= 0)
    {
      break
    } 
  }
    repeat {
      im <- thinningIteration(im, 0,table2)
      im <- thinningIteration(im, 1,table2)
      diff <- prev-im
    #  count <- count+1
     # cat(count)
      prev <- im
      if(countNonZero(diff) <= 0)
      {
        break
      }
    #remove stuck pixels
    
  } 
  
  return(im)
}
