// consts are defined with PascalCase (not my choice)
val From: Int = 1;

// define a function "five" that returns 5
def 五(): Int = 5;

// write a range from..upto to stdout
def print_range(from: Int, upto: Int): Void = {
    if from >= upto then ... else {
        iwrite(from);
        print_range(from, upto) 
    }
}

// notice how 五 evaluates to 5 without even calling it.
def main(): Void = print_range(From, 五);
