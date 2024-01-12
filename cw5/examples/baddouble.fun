
val MyDouble: Double = 42.42.42;

val MyPtr: VoidPtr = &MyDouble;

print_string("こんにちは世界！\n");
// unterminated strings really make the lexer unhappy!
print_string("this string does not terminate);
