val HELLO_WORLD: String = "Hello, world!";

def println(string: String): Void = {
    write(string);
    cwrite('\n');
}

{
    println("Hello, world!");
}
