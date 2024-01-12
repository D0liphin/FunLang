// Prints a new line to stdout and immediately fflush()
def new_line(): Void = write("\n");

// Does nothing, but will be called -- consider using a Void literal instead
// `...`
def skip(): Void = ...;

// Print an integer as decimal
def print_int(n: Int): Void = write(n, false);

// Print a single space (U+0020) to stdout 
def print_space(): Void = write(" ");

// Print a single asterisk (U+002A) to stdout
def print_star(): Void = write("*");

// Print an integer, interpreted as a unicode codepoint to stdout
def print_char(ch: Int): Void = write(ch, true);

// Print a boolean to stdout
def print_bool(b: Bool): Void = {
    if b then write("true")
    else write("false")
}

// Print a string to stdout
def print_string(s: String): Void = write(s);