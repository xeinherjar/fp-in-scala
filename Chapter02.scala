object Chapter02 {
    def fibonacci(n: Int): Int = {
        @annotation.tailrec
        def go(n: Int, prev: Int, next: Int): Int = {
            if (n == 0) return prev
            go(n - 1, next, prev + next)
        }
        go(n, 0, 1)
    }

    def isSorted[A](xs: Array[A], ordered: (A, A) => Boolean): Boolean = {
        @annotation.tailrec
        def loop(idx: Int): Boolean = {
            if (idx >= xs.length - 1) true
            else if (ordered(xs(idx), xs(idx + 1))) loop(idx + 1)
            else false
        }
        loop(0)
    }

    def curry[A, B, C](f: (A, B) => C): A => (B => C) =
        a => b => f(a, b)

    def uncurry[A, B, C](f: A => B => C): (A, B) => C =
        (a, b) => f(a)(b)
}
