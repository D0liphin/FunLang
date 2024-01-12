// This is a nostd example that prints the first 25 fibonacci values in order
// Try changing it to use doubles!

val COUNT: Int = 20;

def iprint(n: Int): Void = {
    write(n, false);
    write(" ");
}

def fibacc(i: Int, n1: Int, n2: Int, max: Int): Int = {
    iprint(n1 + n2);
    if i == max 
    then n1 + n2 
    else fibacc(i + 1, n2, n1 + n2, max)
}

def fib(n: Int): Void = {
    if n == 0 then ...
    else {
        if n == 1 then iprint(1) 
        else {
            iprint(1);
            iprint(1);
            fibacc(2, 1, 1, n);
        }
    }
}

{
    print_bool(5 == 5);
    new_line();
    fib(25);
    write("\n");
}
