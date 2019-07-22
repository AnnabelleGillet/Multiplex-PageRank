package pagerank_multiplex.core

import breeze.linalg._

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object PageRankMultiplex {
    private val vQuadraticError = 0.0001
    
    case class PageRankMultiplexSparseResult(layersResult: List[SparseVector[Double]], globalResult: List[SparseVector[Double]])
    
    /**
     * Page Rank Multiplex on CSCMatrix.
     * 
     * @param multiplex: List[CSCMatrix] -> The List of layers used to compute the Multiplex Page Rank. The layers must be ordered by importance in the List.
     * @param alpha: Double (default 0.85) -> The teleportation factor.
     * @param multiplicative: Boolean (default true) -> If the Multiplex Page Rank is multiplicative. If additive is also true, the Multiplex Page Rank is combined.
     * @param additive: Boolean (default true) -> If the Multiplex Page Rank is additive. If multiplicative is also true, the Multiplex Page Rank is combined.
     * @return PageRankMultiplexSparseResult(layersResult: List[SparseVector[Double]], globalResult: List[SparseVector[Double]]):
     * 		layersResult: The Page Rank results for each layer took individually.
     * 		globalResult: The Multiplex Page Rank results for each step.
     */
    def run(multiplex: List[CSCMatrix[Double]], alpha: Double = 0.85, multiplicative: Boolean = true, additive: Boolean = true): PageRankMultiplexSparseResult = {
        var jump = List[SparseVector[Double]]()
        var layersResult = List[SparseVector[Double]]()
        var globalResult = List[SparseVector[Double]]()
        //for jj=1:max(size(A))
        for (i <- 0 until multiplex.size) {
            val currentMatrix: CSCMatrix[Double] = multiplex(i)
            /*
             * x0{jj}=(sum(A{jj},1)>0)+(sum(A{jj},2)>0)';
             * x0{jj}=(x0{jj}>0);
             * norma=nnz(x0{jj});
             * x0{jj}=x0{jj}/norma;
             * x0{jj}=x0{jj}';
             */
            
            val sumColumns = (SparseVector.fill[Double](currentMatrix.cols){1.0}.t * currentMatrix).t
            val sumRows = currentMatrix * SparseVector.fill[Double](currentMatrix.rows){1.0}
            
            val positiveColumns = sumColumns *:* (SparseVector.fill[Double](sumColumns.length){1.0} /:/ sumColumns)
            
            val x0: SparseVector[Double] = {
                val sumTotal = sumColumns +:+ sumRows
                val xtmp = sumTotal *:* (SparseVector.fill[Double](sumTotal.length){1.0} /:/ sumTotal)
                var nnz = xtmp.activeSize
                xtmp /:/ nnz.toDouble
            }
            
            /*
             * D{jj}=sum(A{jj},1)+(sum(A{jj},1)==0);
             * D{jj}=ones(1,max(size(A{jj})))./D{jj};
             * n=numel(D{jj});
             * D{jj}=spdiags(D{jj}(:),0,n,n);
             */
            val d: CSCMatrix[Double] = diag((SparseVector.fill[Double](sumColumns.length){1.0} /:/ sumColumns))
            
            /*
             * l{jj}=sum(A{jj},1)>0;
             * jump{jj}=alpha*l{jj}';
             */ 
            val newJump: SparseVector[Double] = positiveColumns *:* alpha
            jump = jump :+ newJump
   
            /*  
             * vt=x0{jj};
             *  
             * last_v1 = ones(max(size(A{jj})),1) * inf;
             */
            var vt = x0.copy
            var lastV1 = SparseVector.fill(max(currentMatrix.rows, currentMatrix.cols)){Double.PositiveInfinity}
            
            /* while(norm(vt - last_v1, 2) > v_quadratic_error*norm(vt))
             *         last_v1 = vt;
             *               
             *         vt=A{jj}*D{jj}*(vt.*jump{jj})+(sum((1-jump{jj}).*vt,1)*x0{jj});
             *             
             * end
             */
            val invJump: DenseVector[Double] = (DenseVector.ones[Double](currentMatrix.cols) -:- jump(i))
            while (norm(vt - lastV1, 2) > (vQuadraticError * norm(vt, 2))) {
                lastV1 = vt
                vt = currentMatrix * d * (vt *:* jump(i)) + (x0 *:* (invJump dot vt))
            }
            
            /* x{jj}=vt;
             * end
             * 
             * X{1}=x{1};
             */   
            layersResult = layersResult :+ vt.copy
        }
        
        globalResult = globalResult :+ layersResult(0).copy
        
        /*for jj=1:max(size(A))-1
        */
        for (i <- 0 until multiplex.size - 1) {
            /* n=max(size(A{jj+1}));
             */  
            val currentMatrix = multiplex(i + 1)
            val n = max(currentMatrix.cols, currentMatrix.rows)
             
            /* G{jj+1}=(spdiags(X{jj}(:),0,n,n))*A{jj+1};
             * G{jj+1}= sum(G{jj+1},1) + ((sum(G{jj+1},1))==0);
             * G{jj+1}=ones(1,n)./G{jj+1};
             * G{jj+1}=spdiags(G{jj+1}(:),0,n,n);
             */
            val g = {
                val g1 = diag(globalResult(i)) * currentMatrix
                val g2: SparseVector[Double] = {
                    val sumColumns = (SparseVector.fill[Double](g1.cols){1.0}.t * g1).t
                    
                    var tmp = SparseVector.fill[Double](g1.cols){1.0}
                    for ((i, v) <- sumColumns.activeIterator if v > 0) {
                        tmp(i) = v
                    }
                    tmp
                }
                val g3 = SparseVector.fill[Double](n){1.0} /:/ g2
                diag(g3)
            }
            
             
            /* Gaus=spdiags((X{jj}(:)).^(beta),0,n,n);
             *  
             * M{jj+1}=Gaus*A{jj+1}*G{jj+1};
             */      
            val gaus: CSCMatrix[Double] = if (multiplicative) diag(globalResult(i)) else diag(SparseVector.fill[Double](globalResult(i).length){1.0})
            val m = gaus * currentMatrix * g
            
            /* vt=X{jj};
             * last_v1 = ones(max(size(A{jj+1})),1) * inf;
             */
            var vt = globalResult(i)
            var lastV1 = SparseVector.fill(max(currentMatrix.rows, currentMatrix.cols)){Double.PositiveInfinity}
            
            /* while(norm(vt - last_v1, 2) > v_quadratic_error*norm(vt))
             *         last_v1 = vt;
             *          
             *         vt = (M{jj+1}*(vt.*jump{jj+1}))+sum((1-jump{jj+1}).*vt,1)*X{jj}.^(gamma)/sum(X{jj}.^(gamma));
             *         vt=vt/sum(vt);
             * end
             */ 
            val tmp = if (additive) globalResult(i) else SparseVector.fill[Double](globalResult(i).length){1.0}
            val invJump: DenseVector[Double] = (DenseVector.ones[Double](currentMatrix.cols) -:- jump(i + 1))
            while (norm(vt - lastV1, 2) > (vQuadraticError * norm(vt, 2))) {
                lastV1 = vt
                vt = (m * (vt *:* jump(i + 1))) + (tmp *:* (invJump dot vt)) / sum(tmp)
                vt = vt / sum(vt)
            }

            /* X{jj+1}=vt;
             *      
             * end
             */
            globalResult = globalResult :+ vt.copy
         }
        
        PageRankMultiplexSparseResult(layersResult, globalResult)
    }

    /**
     * Page Rank Multiplex on CSCMatrix with parallelism.
     * 
     * @param multiplex: List[CSCMatrix] -> The List of layers used to compute the Multiplex Page Rank. The layers must be ordered by importance in the List.
     * @param alpha: Double (default 0.85) -> The teleportation factor.
     * @param multiplicative: Boolean (default true) -> If the Multiplex Page Rank is multiplicative. If additive is also true, the Multiplex Page Rank is combined.
     * @param additive: Boolean (default true) -> If the Multiplex Page Rank is additive. If multiplicative is also true, the Multiplex Page Rank is combined.
     * @return PageRankMultiplexSparseResult(layersResult: List[SparseVector[Double]], globalResult: List[SparseVector[Double]]):
     * 		layersResult: The Page Rank results for each layer took individually.
     * 		globalResult: The Multiplex Page Rank results for each step.
     */
    def runPar(multiplex: List[CSCMatrix[Double]], alpha: Double = 0.85, multiplicative: Boolean = true, additive: Boolean = true): PageRankMultiplexSparseResult = {
        var jump = List[SparseVector[Double]]()
        var layersResultFuture = scala.collection.mutable.Map[Int, Future[SparseVector[Double]]]()
        var globalResult = List[SparseVector[Double]]()
        //for jj=1:max(size(A))
        for (i <- 0 until multiplex.size) {
            val currentMatrix: CSCMatrix[Double] = multiplex(i)
            /*
             * x0{jj}=(sum(A{jj},1)>0)+(sum(A{jj},2)>0)';
             * x0{jj}=(x0{jj}>0);
             * norma=nnz(x0{jj});
             * x0{jj}=x0{jj}/norma;
             * x0{jj}=x0{jj}';
             */
            
            val sumColumns = (SparseVector.fill[Double](currentMatrix.cols){1.0}.t * currentMatrix).t
            val sumRows = currentMatrix * SparseVector.fill[Double](currentMatrix.rows){1.0}
            
            val positiveColumns = sumColumns *:* (SparseVector.fill[Double](sumColumns.length){1.0} /:/ sumColumns)
            
            /*
             * l{jj}=sum(A{jj},1)>0;
             * jump{jj}=alpha*l{jj}';
             */ 
            val newJump: SparseVector[Double] =  positiveColumns *:* alpha
            jump = jump :+ newJump
            
            val resultFuture = Future {
                val x0: SparseVector[Double] = {
                    val sumTotal = sumColumns +:+ sumRows
                    val xtmp = sumTotal *:* (SparseVector.fill[Double](sumTotal.length){1.0} /:/ sumTotal)
                    var nnz = xtmp.activeSize
                    xtmp /:/ nnz.toDouble
                }

                /*
                 * D{jj}=sum(A{jj},1)+(sum(A{jj},1)==0);
                 * D{jj}=ones(1,max(size(A{jj})))./D{jj};
                 * n=numel(D{jj});
                 * D{jj}=spdiags(D{jj}(:),0,n,n);
                 */
                val d: CSCMatrix[Double] = diag((SparseVector.fill[Double](sumColumns.length){1.0} /:/ sumColumns))
                   
                /*  
                 * vt=x0{jj};
                 *  
                 * last_v1 = ones(max(size(A{jj})),1) * inf;
                 */
                var vt = x0.copy
                var lastV1 = SparseVector.fill(max(currentMatrix.rows, currentMatrix.cols)){Double.PositiveInfinity}
                
                /* while(norm(vt - last_v1, 2) > v_quadratic_error*norm(vt))
                 *         last_v1 = vt;
                 *               
                 *         vt=A{jj}*D{jj}*(vt.*jump{jj})+(sum((1-jump{jj}).*vt,1)*x0{jj});
                 *             
                 * end
                 */
                val invJump: DenseVector[Double] = (DenseVector.ones[Double](currentMatrix.cols) -:- jump(i))
                while (norm(vt - lastV1, 2) > (vQuadraticError * norm(vt, 2))) {
                    lastV1 = vt
                    vt = currentMatrix * d * (vt *:* jump(i)) + (x0 *:* (invJump dot vt))
                }
                vt
            }
            
            /* x{jj}=vt;
             * end
             * 
             * X{1}=x{1};
             */   
            layersResultFuture(i) = resultFuture
        }
        
        globalResult = globalResult :+ Await.result(layersResultFuture(0), 100.seconds)//layersResult(0).copy
        
        /*for jj=1:max(size(A))-1
        */
        for (i <- 0 until multiplex.size - 1) {
            /* n=max(size(A{jj+1}));
             */  
            val currentMatrix = multiplex(i + 1)
            val n = max(currentMatrix.cols, currentMatrix.rows)
             
            /* G{jj+1}=(spdiags(X{jj}(:),0,n,n))*A{jj+1};
             * G{jj+1}= sum(G{jj+1},1) + ((sum(G{jj+1},1))==0);
             * G{jj+1}=ones(1,n)./G{jj+1};
             * G{jj+1}=spdiags(G{jj+1}(:),0,n,n);
             */
            val g = {
                val g1 = diag(globalResult(i)) * currentMatrix
                val g2: SparseVector[Double] = {
                    val sumColumns = (SparseVector.fill[Double](g1.cols){1.0}.t * g1).t
                    
                    var tmp = SparseVector.fill[Double](g1.cols){1.0}
                    for ((i, v) <- sumColumns.activeIterator if v > 0) {
                        tmp(i) = v
                    }
                    tmp
                }
                val g3 = SparseVector.fill[Double](n){1.0} /:/ g2
                diag(g3)
            }
            
             
            /* Gaus=spdiags((X{jj}(:)).^(beta),0,n,n);
             *  
             * M{jj+1}=Gaus*A{jj+1}*G{jj+1};
             */      
            val gaus: CSCMatrix[Double] = if (multiplicative) diag(globalResult(i)) else diag(SparseVector.fill[Double](globalResult(i).length){1.0})
            val m = gaus * currentMatrix * g
            
            /* vt=X{jj};
             * last_v1 = ones(max(size(A{jj+1})),1) * inf;
             */
            var vt = globalResult(i)
            var lastV1 = SparseVector.fill(max(currentMatrix.rows, currentMatrix.cols)){Double.PositiveInfinity}
            
            /* while(norm(vt - last_v1, 2) > v_quadratic_error*norm(vt))
             *         last_v1 = vt;
             *          
             *         vt = (M{jj+1}*(vt.*jump{jj+1}))+sum((1-jump{jj+1}).*vt,1)*X{jj}.^(gamma)/sum(X{jj}.^(gamma));
             *         vt=vt/sum(vt);
             * end
             */ 
            val tmp = if (additive) globalResult(i) else SparseVector.fill[Double](globalResult(i).length){1.0}
            val invJump: DenseVector[Double] = (DenseVector.ones[Double](currentMatrix.cols) -:- jump(i + 1))
            while (norm(vt - lastV1, 2) > (vQuadraticError * norm(vt, 2))) {
                lastV1 = vt
                vt = (m * (vt *:* jump(i + 1))) + (tmp *:* (invJump dot vt)) / sum(tmp)
                vt = vt / sum(vt)
            }
            /* X{jj+1}=vt;
             *      
             * end
             */
            globalResult = globalResult :+ vt.copy
         }
        
        var layersResult = scala.collection.mutable.Map[Int, SparseVector[Double]]()
        for ((key, value) <- layersResultFuture) {
            layersResult(key) = Await.result(value, 100.seconds)
        }
        PageRankMultiplexSparseResult(layersResult.values.toList, globalResult)
    }
}