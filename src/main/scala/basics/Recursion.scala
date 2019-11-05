package basics

object Recursion extends App {
    def factorial(n: Int) : Int =  if(n < 2) 1 else n * factorial(n-1)
    def fib(n:Int) : Int =  if (n<2) 1 else fib(n-2)+fib(n-1)
    def packBins(bins: Array[Double], objs: Array[Double]):Boolean = {
        def helper(o: Int): Boolean ={
            if( o >= objs.length) true
            else {
                var ret = false 
                for(b <- bins.indices){
                    if (objs(o) <= bins(b) ){
                        bins(b) -= objs(o)
                        ret ||= helper(o+1)
                        bins(b) += objs(o)
                    }
                }
                ret
            }
        } 
        helper(0)
    }
}