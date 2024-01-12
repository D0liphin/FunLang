val INT_SQRT_MAX: Int = 46340;

def spinloop(count: Int): Void = {
    if count <= 0 
    then ...
    else {
        write("");
        spinloop(count - 1);
    }
}

def spinsleep(ms: Int): Void = spinloop(ms * 100_000);

def write_ellipses(count: Int, ms: Int): Void = {
    if count == 0 
    then ...
    else {
        spinsleep(ms);
        write(".");
        write_ellipses(count - 1, ms);
    }
}

def idbg(tag: String, n: Int): Void = {
    write(tag);
    write(" = ");
    write(n, false);
    write('\n', true);
}

def itpldbg(tag: String, n1: Int, n2: Int): Void = {
    write(tag);
    write(" = (");
    write(n1, false);
    write(", ");
    write(n2, false);
    write(")");
    write("\n");
}

def sqr(n: Int): Int = n * n;

def midpoint(lb: Int, ub: Int): Int = (lb + ub) / 2;

def intsqrtrec(lb: Int, ub: Int, target: Int): Int = {
    spinsleep(100);
    itpldbg("(lb, ub)", lb, ub);
    if lb + 1 == ub then {
        lb
    } else if sqr(midpoint(lb, ub)) > target then {
        intsqrtrec(lb, midpoint(lb, ub), target)
    } else {
        intsqrtrec(midpoint(lb, ub), ub, target)
    }
}

// Compute the square root of `n` and floor it
def intsqrt(n: Int): Int = {
    write("calculating sqrt(");
    write(n, false);
    write(")");
    write_ellipses(4, 500);
    write("\n");
    intsqrtrec(0, INT_SQRT_MAX, n)
}

{
    write(intsqrt(625), false);
    write('\n', true);
}