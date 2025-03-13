object Neg

def neg[A] : (A => Boolean) => A => Boolean = p => !p(_)
