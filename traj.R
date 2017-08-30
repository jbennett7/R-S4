setClass(
    Class="Trajectories",
    representation=representation(
        times = "numeric",
        traj = "matrix"
    ),
    validity=function(object){
        cat("~~~ Trajectories: inspector ~~~ \n")
        if(length(object@times) != ncol(object@traj)){
            stop ("[Trajectories: validation] the number of temporal measurements does not correspond to the number given.")
        } else {}
        return(TRUE) 
    }
)
setMethod(
    f= "initialize",
    signature="Trajectories",
    definition=function(.Object, times, traj){
        cat("~~~ Trajectory initializer ~~~\n")
        if(!missing(traj)){
            colnames(traj) <- paste("T", times,sep="")
            rownames(traj) <- paste("I",1:nrow(traj),sep="")
            .Object@traj <- traj
            .Object@times <- times
            validObject(.Object)
        }
        return(.Object)
    }
)
setMethod(
    f= "plot",
    signature= "Trajectories",
    definition=function (x,y,...){
        matplot(x@times, t(x@traj), xaxt="n", type="l", ylab= "", xlab= "", pch=1)
        axis(1, at=x@times)
    }
)
setMethod(
    f= "print",
    signature= "Trajectories",
    definition=function (x,...){
        cat("*** Class Trajectories, print method *** \n")
        cat("* Times ="); print (x@times)
        cat("* Traj = \n"); print (x@traj)
        cat("****** End print (trajectories) ****** \n")
    }
)
setMethod(
    f= "show",
    signature= "Trajectories",
    definition=function (object){
        if (length(object@traj) > 0){
            cat("Class Trajectories: show method \n")
            print (object@times)
            print (object@traj)
        } else {
            cat("Howdy Folks. \n")
        }
    }
)
setGeneric(
    name= "countMissing",
    def=function(object,value){standardGeneric("countMissing")}
)   
setMethod(
    f= "countMissing",
    signature= "Trajectories",
    definition=function(object){
        return(sum(is.na(object@traj)))
    }
)
lockBinding("countMissing",.GlobalEnv)
## Errors because of lockBinding
#setGeneric(
#    name= "countMissing",
#    def=function(object,value){standardGeneric("countMissing")}
#)   
setGeneric("getTimes", function(object){standardGeneric ("getTimes")})
setMethod("getTimes","Trajectories",
    function(object){
        return(object@times)
    }
)
setGeneric("getTraj", function(object){standardGeneric ("getTraj")})
setMethod("getTraj","Trajectories",
    function(object){
        return(object@traj)
    }
)
setGeneric("getTrajInclusion", function(object){standardGeneric ("getTrajInclusion")})
setMethod("getTrajInclusion","Trajectories",
    function(object){
        return(object@traj[,1])
    }
)
setGeneric("setTimes<-", function(object, value){standardGeneric ("setTimes<-")})
setReplaceMethod(
    f="setTimes",
    signature="Trajectories",
    definition=function(object,value){
        object@times <- value
        validateObject(object)
        return(object)
    }
)
setMethod(
    f="[",
    signature="Trajectories",
    definition=function(x,i,j,drop){
        if(i=="times"){return(x@times)}else{}
        if(i=="traj"){return(x@traj)}else{}
    }
)
setReplaceMethod(
    f="[",
    signature="Trajectories",
    definition=function(x,i,j,value){
        if(i=="times"){x@times <- value}else{}
        if(i=="traj"){x@traj <- value}else{}
        validateObject(x)
        return(x)
    }
)

## Also can use setGenericVerif to lock the method as well.
#setGenericVerif <- function(x,y){if(!isGeneric(x){setGeneric(x,y)}else{}}
## and then never use setGeneric, but always use setGenericVerif.

tr <- trajectories <- function(times, traj){
    cat("~~~~ Trajectories: constructor ~~~~\n")
    return(new(Class="Trajectories",times=times,traj=traj))
}

#trajPitie <- new(Class="Trajectories")
trajCochin <- new(
    Class="Trajectories",
    times=c(1,3,4,5),
    traj=rbind(
        c(15, 15.1, 15.2, 15.2),
        c(16, 15.9, 16, 16.4),
        c(15.2, NA, 15.3, 15.3)
    )
)

setClass(
    Class="TrajectoriesBis",
    representation=representation(
        times = "numeric",
        traj = "matrix"
    ),
#   prototype=prototype(
#       times = 1,
#       traj = matrix (0)
#   )
)
#removeClass("TrajectoriesBis")
setMethod("initialize",
    "TrajectoriesBis",
    function(.Object, nbWeek, BMIinit){
        traj <- outer(BMIinit, 1:nbWeek, function(init,week){return(init+0.1*week)})
        colnames(traj) <- paste("T",1:nbWeek,sep="")
        rownames(traj) <- paste("I",1:nrow(traj),sep="")
        .Object@times <- 1:nbWeek
        .Object@traj <- traj
        return(.Object)
    }
)

setClass(
    Class="Partition",
    representation=representation(
        nbGroups="numeric",
        part="factor"
    )
)
setGeneric("getNbGroups",function(object){standardGeneric("getNbGroups")})
setMethod("getNbGroups","Partition",function(object){return(object@nbGroups)})
setGeneric("getPart",function(object){standardGeneric("getPart")})
setMethod("getPart","Partition",function(object){return(object@part)})

slotNames("Trajectories")
getSlots("Trajectories")
getClass("Trajectories")
showMethods(class="Trajectories")
getMethod(f="plot", signature="Trajectories")
existsMethod(f="plot", signature="Trajectories")

par(mfrow=c(1,2))
plot(trajCochin)
#plot(trajStAnne)
print(trajCochin)
trajCochin
#trajPitie

new(Class="Trajectories", times=1:2,traj=matrix(1:2,ncol=2))
#new(Class="Trajectories", times=1:3, traj=matrix(1:2,ncol=2))
new(Class="Trajectories", times=c(1,2,4,8), traj=matrix(1:8,nrow=2))
new(Class="TrajectoriesBis", nbWeek=4, BMIinit=c(16,17,15.6))
trajectories(time=c(1,2,4),traj=matrix(1:6,ncol=3))

partCochin <- new(Class="Partition", nbGroups=2, part=factor(c("A","B","A","B")))
partStAnnie <- new(Class="Partition", nbGroups=2, part=factor(rep(c("A","B"),c(50,30))))

setGeneric("test", function(x,y,...){standardGeneric("test")})
setMethod("test", "numeric", function(x,y,...){cat("x is numeric =",x,"\n")})
setMethod("test", "character", function(x,y,...){cat("x is character =",x,"\n")})
setMethod(
    f= "test",
    signature= c(x="numeric", y="character"),
    definition= function(x,y,...){
        cat("more complicated...")
        cat("x is numeric =", x, " AND y is a character = ", y, "\n")
    }
)
test(3.14)
test("Hello")
test(3.14,"Goodbye")
showMethods(test)

setClass(
    Class="TrajPartitioned",
    representation=representation(listPartitions="list"),
    contains="Trajectories"
)
tdPitie <- new("TrajPartitioned")
show(tdPitie)
unclass(tdPitie)

partCochin2 <- new("Partition", nbGroups=3, part=factor(c("A","C","C","B")))
#tdCochin <- new(
#    Class="TrajPartition",
#    times=c(1,3,4,5),
#    traj=trajCochin@traj,
#    listPartitions=list(partCochin,partCochin2)
#)
#getMethod("initialize","TrajPartition")
existsMethod("initialize","TrajPartition")
hasMethod("initialize","TrajPartition")
selectMethod("initialize","TrajPartition")
