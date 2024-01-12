/**
 * @file funstd.c
 * @author Oliver Iliffe (oliver.iliffe@kcl.ac.uk)
 * @brief Builtin functions for the fun language, compiled 
 * @version 0.1.0
 * @date 2024-01-11
 * 
 * License: MIT
 * 
 * You will need to compile this with clang, and then do a quick replace of all
 * matches of "buitin_" with "builtin." For this to work at the moment. Then a 
 * few more things... This is not really meant to be used for actually 
 * generating the final file -- just as a guide.
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

/**
 * @brief A `*mut [u8]`
 */
typedef struct {
    uint8_t *data;
    size_t len;
} FunString;

/**
 * @brief Print a `FunString` to stdout
 */
void builtin_swrite(FunString s) {
    fwrite(s.data, 1, s.len, stdout);
    fflush(stdout);
}

/** max value of utf8 codepoint */
#define UTF8MAX 0x10ffff
#define UTF8MAX1 0x80
#define UTF8MAX2 0x800
#define UTF8MAX3 0x10000

/**
 * @brief Convert a codepoint to its utf8 encoding
 * @return The number of bytes in the encoding
 * @note All of `out` might be clobbered. This also assumes that `i` is a valid
 * codepoint. Behaviour is undefined if it is not.
 */
size_t builtin_encode_utf8(uint8_t out[4], uint32_t i) {
    if (i < UTF8MAX1) {
        out[0] = (int8_t)i;
        return 1;
    } else if (i < UTF8MAX2) {
        out[0] = 0b11000000 | (i >> 6);       // first 5 bits
        out[1] = 0b10000000 | (i & 0b111111); // last 6 bits
        return 2;
    } else if (i < UTF8MAX3) {
        out[0] = 0b11100000 | (i >> 12);             // first 4 bits
        out[1] = 0b10000000 | ((i >> 6) & 0b111111); // penultimate 6-bit chunk
        out[2] = 0b10000000 | (i & 0b111111);        // last 6 bits
        return 3;
    } else {
        out[0] = 0b11110000 | (i >> 18);              // first 3 bits
        out[1] = 0b10000000 | ((i >> 12) & 0b111111); // penultimate 6-bit chunk
        out[2] = 0b10000000 | ((i >> 6) & 0b111111);  // penultimate 6-bit chunk
        out[3] = 0b10000000 | (i & 0b111111);         // last 6 bits
        return 4;
    }
}

/**
 * @brief Validates a unicode codepoint
 */
bool builtin_valid_codepoint(int32_t i) {
    return !(i > UTF8MAX || i >= 0xd800 && i <= 0xdfff);
}

/**
 * @brief Write an integer to stdout, encoded as utf8 if it's a valid codepoint
 */
void builtin_ciwrite(int32_t i) {
    if (!valid_codepoint(i)) {
        i = 0xfffd; // replacement
    }

    uint8_t data[4];
    size_t len = encode_utf8(data, i);

    FunString string;
    string.data = data;
    string.len = len;

    builtin_swrite(string);
}

/**
 * @brief Write an integer to stdout using the `%d` format specifier from 
 * `printf()`
 */
void builtin_iiwrite(int32_t i) {
    printf("%d", i);
}

/**
 * @brief Write an integer to stdout, formatted as a character or as an integer
 * depending on whether or not the `ischar` flag is `true`. 
 * @note Yes, I know I should have made the second argument to this an `int`...
 */
void builtin_iwrite(int32_t i, bool ischar) {
    if (ischar) {
        builtin_ciwrite(i);
    } else {
        builtin_iiwrite(i);
    }
}

/**
 * @brief Write a double to stdout, but prioritising prettiness!
 */
void builtin_dwrite(double n) {
    printf("%g", n);
}