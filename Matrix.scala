//http://stackoverflow.com/questions/1252915/scala-how-to-define-generic-function-parameters
//http://dcsobral.blogspot.de/2010/06/implicit-tricks-type-class-pattern.html
//http://www.scala-lang.org/docu/files/ScalaByExample.pdf#
//http://scalatutorial.wordpress.com/2011/12/
//http://www.azavea.com/blogs/labs/2011/06/scalas-numeric-type-class-pt-2/


object Matrix extends App{
	case class Matrix[N:Numeric](val offset: Int, val elements: IndexedSeq[N]) {
		import Numeric.Implicits._//Magic for numeric! Normal operators usable
		require((offset > 0 && elements.length == (offset * offset)) || offset == 0, "Inappropriate offset or element count for quadratic matrix")
	   	def apply(x: Int, y: Int) = if (elements.isDefinedAt(y * offset + x)) elements(y * offset + x) else Double.NaN
	    def change(x: Int, y: Int)(newVal: N) = if (elements.isDefinedAt(y * offset + x)) this.copy(elements = elements.updated(y * offset + x, newVal)) else NaM
	    def +(mat: Matrix[N]) = new Matrix(offset, (elements, mat.elements).zipped map (_+_))
	    def -(mat: Matrix[N]) = new Matrix(offset, (elements, mat.elements).zipped map (_-_))
	    def *(fac: N) = new Matrix(offset, (for { i <- 1 to elements.length } yield (elements(i)*fac)))
	    
	    //empty "" needed in some case to evade errors because of import Numeric.Implicits._
	    override def toString = {
	    	"Matrix(" + (for { i <- 0 to (offset * offset) - 1 } yield i match {
		        case i if elements.length == 1 => "(" + elements(i) + ")"
		        case 0 => "(" + elements(i) + ","
		        case i if i + 1 == elements.length => ""+elements(i) + ")"
		        case i if (i + 1) % offset == 0 => ""+elements(i) + ")("
		        case _ => ""+elements(i) + ","
	    	}).mkString + ")"
	    }
	    
	    def x(mat: Matrix[N]) = { //beim ueberladen macht NaM probleme
    		if (offset > 0 && offset == (mat.elements.length/offset)) {
    			val dim = offset*offset
				val collect = for{pos <- 0 to dim-1
		    		//Left matrix
			        val xRow = (pos / offset).abs
			        val xRowStart = xRow*offset
			        val xRowEnd = xRowStart + offset - 1
			        //Right matrix
			        val yCol = pos % mat.offset
			        
			        val colElements = for { xIndex <- xRowStart to xRowEnd
			          	val yIndex = yCol+((xIndex-xRowStart)*mat.offset)
			        } yield (elements(xIndex)*mat.elements(yIndex))
				} yield colElements.reduceLeft{_+_}
	        new Matrix(offset, collect)
	      } else {
	    	  	NaM
	        }
	    }
        
	    import java.util.concurrent._
	    def xpar(mat: Matrix[N], threadMulti:Double = 0) = {
	        if (offset > 0 && offset == (mat.elements.length/offset)) {
		        val dim = offset*offset
		        val definedThreads = if((Runtime.getRuntime().availableProcessors() * threadMulti).toInt > 0) (Runtime.getRuntime().availableProcessors() * threadMulti).toInt else 1
		        val matrixThreadRatio = dim / definedThreads
		        val neededThreads = if(matrixThreadRatio >=1 )  definedThreads else dim
		        val tasksPerThread = (matrixThreadRatio + 1).abs
		        val collect = scala.collection.mutable.IndexedSeq.fill[N](dim)(implicitly[Numeric[N]].zero)
		        val Threads = for {threadCounter <- 0 to neededThreads - 1
			        val from = tasksPerThread * threadCounter
			        val to = if (threadCounter == neededThreads - 1) dim-1 else (tasksPerThread * threadCounter) + tasksPerThread-1
			        val newThread = new Thread(new Runnable(){
			        	def run(){
			            	for(pos <- from to to){
			            		//Left matrix
						        val xRow = (pos / offset).abs
						        val xRowStart = xRow*offset
						        val xRowEnd = xRowStart + offset - 1
						        //Right matrix
						        val yCol = pos % mat.offset
						        
						        for { 	xIndex <- xRowStart to xRowEnd
						          	val yIndex = yCol+((xIndex-xRowStart)*mat.offset)
						        } yield (collect(pos) = collect(pos)+(elements(xIndex)*mat.elements(yIndex))) //reduceLeft was slower then multiple assignments
			            	}
			            }
			        })
		        } yield newThread
		        Threads.foreach(_.start) //start all threads
		        Threads.foreach(_.join)  //join all threads => wait for slowest thread
		        new Matrix(offset, collect)
	        } else {
	        	NaM
	        }
		} 
	}
  
	object Matrix {
		def apply[N:Numeric](dim: Int)(elements: N*) = {
			val n = implicitly[Numeric[N]]
			import n._
			elements.length match {
				case length if length == 0 => new Matrix(dim, (for { i <- 1 to (dim * dim) } yield zero))
				case length if length == 1 => new Matrix(dim, (for { i <- 1 to (dim * dim) } yield elements(0)))
				case length if length == dim * dim => new Matrix(dim, elements.toIndexedSeq)
				case length if length < dim * dim => new Matrix(dim, elements.toIndexedSeq ++ (for { i <- 1 to ((dim * dim) - elements.length) } yield zero))
				case _ => new Matrix(0, IndexedSeq()) //empty matrix
			}
		}

		def apply(dim: Int, f: (Int, Int) => Int) = {
	    	val elements = for {
	    		i <- 0 to dim - 1
	    		j <- 0 to dim - 1
	    	} yield f(i, j)
	    	new Matrix(dim, elements)
	    }
	    
	    def apply[N: Numeric](arr: Array[Array[N]]) = {
	    	val elements = for (elem <- arr if (elem.length == arr(0).length)) yield elem
	    	if (elements.length == arr.length) new Matrix(arr(0).length, elements.flatten) else new Matrix(0, IndexedSeq()) //empty matrix
	    }
	}

	object NaM extends Matrix(0, IndexedSeq[Int]()) {
//		override def apply(x: Int, y: Int) = Double.NaN
//	    override def change(x: Int, y: Int)(newVal: N) = NaM
//	    override def +(mat: Matrix) = NaM
//	    override def -(mat: Matrix[N]) = NaM
//	    override def *(fac: N) = NaM
//	    override def x(mat: Matrix[N]) = NaM
//	    override def xpar(mat: Matrix[N], threadMulti:Double, enforceMulti:Boolean = false) = NaM 
	    override def toString = "NaM"
	}

	object Environment {
		def printOS = {
			import java.util._
	      	println(
	    		  "# cores\t: " + java.lang.Runtime.getRuntime().availableProcessors() +
	        	"\n# OS\t: " + System.getProperty("os.name") + " " + System.getProperty("os.version") + " " + System.getProperty("os.arch") +
	        	"\n# JVM \t: " + System.getProperty("java.version"))
	    	}
  	}
  
  	import java.util.concurrent.TimeUnit
  	import java.lang.String.format
  	def measureTime(block: => Any, loopCount:Int = 1) = {
	  	for(i <- 0 until loopCount){
			val start = java.lang.System.nanoTime()
			val result = block
			val time = java.lang.System.nanoTime() - start
			val min = TimeUnit.NANOSECONDS.toMinutes(time)
			val sec = TimeUnit.NANOSECONDS.toSeconds(time - TimeUnit.MINUTES.toNanos(min))
			val milli = TimeUnit.NANOSECONDS.toMillis(time - TimeUnit.MINUTES.toNanos(min) - TimeUnit.SECONDS.toNanos(sec))
			val micro = TimeUnit.NANOSECONDS.toMicros(time - TimeUnit.MINUTES.toNanos(min) - TimeUnit.SECONDS.toNanos(sec) - TimeUnit.MILLISECONDS.toNanos(milli))
			val nano = TimeUnit.NANOSECONDS.toNanos(time - TimeUnit.MINUTES.toNanos(min) - TimeUnit.SECONDS.toNanos(sec) - TimeUnit.MILLISECONDS.toNanos(milli) - TimeUnit.MICROSECONDS.toNanos(micro))
			println(result)
			println("%02d min %02d sec %03d ms %03d µ %03d ns".format(min,sec,milli,micro,nano))
			Thread.sleep(1000) // sleep 1s
	  	}
  	}
	  
	Environment.printOS // man will ja wissen auf welcher Maschine man arbeitet
	
	
	println(Matrix(3)(1, 2, 3) + " #=> Matrix((1,2,3)(0,0,0)(0,0,0))") // 3x3 Matrix, nur erste Zeile 1,2,3 dann Nullen
	println(Matrix(3)(1.0, 2, 3) + " #=> Matrix((1.0,2.0,3.0)(0.0,0.0,0.0)(0.0,0.0,0.0))")
	//println(Matrix(1)(true)) //<- compiliert nicht
	println
	val mat1 = Matrix(5, (i, j) => if (i == j) 1 else 0) // 5x5 Einheits-Matrix
	val mat2 = Matrix(Array(Array(3, 2, 1), Array(0, 1, 0), Array(0, 0, 3)))
	val mat3 = Matrix(1)(1.01) // 1x1 Matrix
	println(mat1 + " #=> Matrix((1,0,0,0,0)(0,1,0,0,0)(0,0,1,0,0)(0,0,0,1,0)(0,0,0,0,1))")
	println(mat2 + " #=> Matrix((3,2,1)(0,1,0)(0,0,3))")
	println(mat3 + " #=> Matrix((1.01))")
	println(mat1(0, 0) + " #=> 1 da i==j (0==0)")
	println(mat1(4, 0) + " #=> 0 da i!=j (4!=0)")
	println(mat1(4, 4) + " #=> 1 da i==j (4==4)")
	println(mat1(5, 5) + " #=> NaN da Matrix Elemente von 0 bis 4 gehen")
	println
	println(mat1 + mat1 + " #=> Matrix((2,0,0,0,0)(0,2,0,0,0)(0,0,2,0,0)(0,0,0,2,0)(0,0,0,0,2))")
	println(Matrix(3)(1, 2, 3) + mat2 + " #=> Matrix((4,4,4)(0,1,0)(0,0,3))")
	println(Matrix(3)(1, 2, 3) + mat2 + Matrix(3, (i, j) => i + j) + " #=> Matrix((4,5,6)(1,3,3)(2,3,7))")
	println(mat3 + Matrix(1)(0) + " #=> Matrix((1.01))")
	println()
	
	
	val mmul1 = Matrix(5)(3)
	val mmul2 = Matrix(5)(2)
	val mmul3 = Matrix(300)(1) 
	val mmul4 = Matrix(300)(1)
	val mmul5 = Matrix(2)(1,2,2,1)
	val mmul6 = Matrix(2)(2,3,3,2)
	
//	measureTime(mmul5 xpar (mmul6, 1), 3)
	measureTime(mmul3 x mmul4, 3)
	measureTime(mmul3 xpar (mmul4, 1), 3)
//	measureTime(mmul3 xpar (mmul4, 2), 2)

//	measureTime(mmul3 xpar (mmul4, 1), 3)
//	measureTime(mmul3 xpar (mmul4, 2), 3)
//	measureTime(mmul3 xpar (mmul4, 3), 3)
}