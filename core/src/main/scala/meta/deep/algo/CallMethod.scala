package meta.deep.algo

import meta.deep.IR.Predef._

case class CallMethod[R: CodeType](methodId: Int,
                                   argss: List[List[OpenCode[_]]])
    extends Algo[R] {

  override def codegen(): Unit = {
    val initParam: OpenCode[Any] = code"()"

    val flattendArgs = argss.flatten

    val saveMethodParamsList = flattendArgs.zipWithIndex.foldRight(initParam)((
        a,
        b) =>
      code"""Instructions.saveMethodParam(${Const(methodId)}, ${Const(a._2)}, ${a._1}); 
      $b""")

    val restoreMethodParam =
      code"Instructions.restoreMethodParams(${Const(methodId)})"

    // 1. Save next position on stack, so that we continue after the method call
    // 2. Save old parameter registers
    // 3. Set new parameter into registers
    // 4. Jump to method

    val f1: OpenCode[Unit] =
      code"""
                 $saveMethodParamsList; 
                 ()
            """
    // 5. Method will return to position pushed on stack and contain returnValue

    // 6. Restore saved variable registers from stack
    val f2: OpenCode[Unit] =
      code"""
               $restoreMethodParam
          ()
          """

    val a2 = EdgeInfo("Method Call (" + methodId + ") Method to f2",
                              CodeNodeMtd(methodId, end = true),
                              CodeNodePos(AlgoInfo.posCounter + 1),
                              code"()",
                              methodCallInfo = (this, 1))

    val a1 = EdgeInfo("Method Call (" + methodId + ") f1",
                              CodeNodePos(AlgoInfo.posCounter),
                              CodeNodeMtd(methodId),
                              f1,
                              storePosRef = List(List(a2)),
                              methodCallInfo = (this, 0))

    AlgoInfo.stateGraph.append(a1)
    AlgoInfo.nextPos()
    AlgoInfo.stateGraph.append(a2)
    AlgoInfo.stateGraph.append(
      EdgeInfo("Method (" + methodId + ") f2",
                        CodeNodePos(AlgoInfo.posCounter),
                        CodeNodePos(AlgoInfo.posCounter + 1),
                        f2,
                        methodCallInfo = (this, 2)))
    AlgoInfo.nextPos()
  }
}
