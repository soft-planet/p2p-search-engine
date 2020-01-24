package de.htwb.wissrep.util

class Handler[I, O] private (handlerFun: I => O){

    def apply(input: I): O = handlerFun(input)

    def before[T](nextHandlerFunction: O => T): Handler[I, T] = 
        new Handler[I, T](x => nextHandlerFunction(this(x)))
    
        def before[T](nextHandler: Handler[O, T]): Handler[I, T] = 
        this.before(nextHandler(_))

    def after[S](previousHandlerFunction: S => I): Handler[S, O] = 
        new Handler(x => this(previousHandlerFunction(x)))
    def after[S](previousHandler: Handler[S, I]): Handler[S, O] = 
        this.after(previousHandler(_))
}

object Handler{
    def apply[I, O](handlerFun: I => O) = new Handler[I, O](handlerFun)
}