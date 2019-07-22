package pagerank_multiplex.core

import breeze.linalg._

import java.io._

import org.scalatest._

class PageRankMultiplexTest extends FlatSpec with Matchers {
    private def time[R](block: => R): R = {
        val t0 = System.currentTimeMillis()
        val result = block
        val t1 = System.currentTimeMillis()
        info("Elapsed time: " + (t1 - t0).toDouble / 1000.0 + "s")
        result
    }
    
    "Page rank" should "compute" in {
        // Load the matrix
        val limit: Int = 73264
        val matrixRTtmp = csvread(new File("MPR/RT.txt"), '\t')
        val matrixRT = CSCMatrix.zeros[Double](limit, limit)
        for (i <- 0 until matrixRTtmp.rows) {
            if (matrixRTtmp(i, 0).toInt <= limit && matrixRTtmp(i, 1).toInt <= limit) {
                matrixRT(matrixRTtmp(i, 0).toInt - 1, matrixRTtmp(i, 1).toInt - 1) = matrixRTtmp(i, 2)
            }
        }

        val matrixMentiontmp = csvread(new File("MPR/Mention.txt"), '\t')
        val matrixMention = CSCMatrix.zeros[Double](limit, limit)
        for (i <- 0 until matrixMentiontmp.rows) {
            if (matrixMentiontmp(i, 0).toInt <= limit && matrixMentiontmp(i, 1).toInt <= limit) {
                matrixMention(matrixMentiontmp(i, 0).toInt - 1, matrixMentiontmp(i, 1).toInt - 1) = matrixMentiontmp(i, 2)
            }
        }

        val matrixReplytmp = csvread(new File("MPR/Reponse.txt"), '\t')
        val matrixReply = CSCMatrix.zeros[Double](limit, limit)
        for (i <- 0 until matrixReplytmp.rows) {
            if (matrixReplytmp(i, 0).toInt <= limit && matrixReplytmp(i, 1).toInt <= limit) {
                matrixReply(matrixReplytmp(i, 0).toInt - 1, matrixReplytmp(i, 1).toInt - 1) = matrixReplytmp(i, 2)
            }
        }
        
        val multiplex = List[CSCMatrix[Double]](matrixRT, matrixReply, matrixMention)
        
        // Run the Multiplex Page Rank
        //val result = time {PageRankMultiplex.run(multiplex)}
        val result = time {PageRankMultiplex.runPar(multiplex)}
        
        // Print the results
        println("Result RT layer:")
        for ((k, v) <- result.layersResult(0).activeIterator if v > 0) {
            println(k + " : " + v)
        }
        println("Result mention layer:")
        for ((k, v) <- result.layersResult(1).activeIterator if v > 0) {
            println(k + " : " + v)
        }
        println("Result reply layer:")
        for ((k, v) <- result.layersResult(2).activeIterator if v > 0) {
            println(k + " : " + v)
        }
        
        println("Global result RT layer:")
        for ((k, v) <- result.globalResult(0).activeIterator if v > 0) {
            println(k + " : " + v)
        }
        println("Global result mention layer:")
        for ((k, v) <- result.globalResult(1).activeIterator if v > 0) {
            println(k + " : " + v)
        }
        println("Global result reply layer:")
        for ((k, v) <- result.globalResult(2).activeIterator if v > 0) {
            println(k + " : " + v)
        }
    }
}
