
#ifndef GENERAL_H
#define GENERAL_H


#include <stdlib.h>

#include <stdint.h>
typedef int64_t s64;
typedef int32_t s32;
typedef int16_t s16;
typedef int8_t  s8;

typedef uint64_t u64;
typedef uint32_t u32;
typedef uint16_t u16;
typedef uint8_t  u8;

#include <assert.h>

#include <string.h>

#ifdef ENV64
typedef s64 string_length_type;
#else
typedef s32 string_length_type;
#endif

struct String {
    char *data = nullptr;
    string_length_type length = 0;

    String() { data = nullptr; }

    // String(char *str) {
    //     this->data = str;
    //     this->length = strlen(str)
    // }

    char operator[](string_length_type index) const {
        assert(index >= 0 && index < length);

        return data[index];
    }

    String substring(string_length_type start, string_length_type slen) {
        assert(start < length && start+slen <= length);

        String s;
        s.data = data + start;
        s.length = slen;
        return  s;
    }
};

// void advance(String *s, s64 amount = 1) {
//     if (s->length) {
//         s->data += amount;
//         s->length -= amount;
//     }
// }

inline String to_string(char *c_string) {
    String s;
    s.data = c_string;
    s.length = strlen(c_string);
    return s;
}

inline char *to_c_string(String s) {
    auto length = s.length;

    char *mem = (char *)malloc(length + 1);
    memcpy(mem, s.data, length);
    mem[s.length] = 0;
    return mem;
}

inline bool operator==(const String &s, const String &t) {
    if (s.length != t.length) return false;
    if (s.data == nullptr && t.data != nullptr) return false;
    if (t.data == nullptr && s.data != nullptr) return false;
    if (s.data == nullptr && t.data == nullptr) return true;

    for (string_length_type i = 0; i < s.length; ++i) {
        if (s[i] != t[i]) return false;
    }

    return true;
}

inline String copy_string(String s) {
    String out;
    out.length = s.length;

    auto length = s.length;
    if (s.data && s.length) {
        out.data = (char *)malloc(length);
        memcpy(out.data, s.data, length);
    }
    return out;
}

struct Span {
    string_length_type start;
    string_length_type length;

    Span(string_length_type start = 0, string_length_type length = 0) {
        assert(start >= 0);
        assert(length >= 0);

        this->start = start;
        this->length = length;
    }

    bool fits_in_string(String text) {
        return (!(text.length < start || text.length < start + length));
    }

    void map_to_text_coordinates(String text, string_length_type *line_start, string_length_type *char_start, string_length_type *line_end, string_length_type *char_end) {
        assert(fits_in_string(text));

        string_length_type line_count = 0;
        string_length_type char_count = 0;
        for (string_length_type i = 0; i < text.length; ++i) {
            if (i == start) {
                *line_start = line_count;
                *char_start = char_count;
            } else if (i == start+length) { // @TODO -1 ?
                *line_end = line_count;
                *char_end = char_count;
                return;
            }

            if (text[i] == '\n') {
                line_count++;
                char_count = 0;
                continue;
            }

            char_count++;
        }
    }
};

struct TextSpan {
    Span span;
    String string;

    TextSpan() {
    }

    TextSpan(String string, Span span) {
        this->string = string;
        this->span = span;

        assert(span.fits_in_string(string));
    }

    String get_text() {
        String s;
        s.data = string.data + span.start;
        s.length = span.length;
        return s;
    }
};

#ifdef ENV64
typedef s64 array_count_type;
#else
typedef s32 array_count_type;
#endif

template<typename T>
struct Array {
    T *data = nullptr;
    array_count_type count = 0;
    array_count_type allocated = 0;

    const int NEW_MEM_CHUNK_ELEMENT_COUNT =  16;

    Array(array_count_type reserve_amount = 0) {
        reserve(reserve_amount);
    }

    ~Array() {
        reset();
    }

    void reserve(array_count_type amount) {
        if (amount <= 0) amount = NEW_MEM_CHUNK_ELEMENT_COUNT;
        if (amount <= allocated) return;

        T *new_mem = (T *)malloc(amount * sizeof(T));
        
        if (data) {
            memcpy(new_mem, data, count * sizeof(T));
            free(data);
        }

        data = new_mem;
        allocated = amount;
    }

    void resize(array_count_type amount) {
        reserve(amount);
        count = amount;
        // @TODO maybe default initalized all elements
        // that we grew by?
    }

    void add(T element) {
        if (count+1 >= allocated) reserve(allocated + NEW_MEM_CHUNK_ELEMENT_COUNT);

        data[count] = element;
        count += 1;
    }

    T pop() {
        assert(count > 0);
        T result = data[count-1];
        count -= 1;
        return result;
    }

    void clear() {
        count = 0;
    }

    void reset() {
        count = 0;
        allocated = 0;

        if (data) free(data);
        data = nullptr;
    }

    T &operator[] (array_count_type index) {
        assert(index >= 0 && index < count);
        return data[index];
    }

    T *begin() {
        return &data[0];
    }

    T *end() {
        return &data[count];
    }
};

template<typename A, typename B>
struct Tuple {
    A item1;
    B item2;
};

template<typename A, typename B>
Tuple<A, B> MakeTuple(A a, B b) {
    Tuple<A, B> t;
    t.item1 = a;
    t.item2 = b;
    return t;
}

// @Incomplete
// struct Pool {
//     struct Chunk {
//         void *data = nullptr;
//         s64 used = 0;
//         s64 allocated = 0;
//     };
//     Array<>
// };

#endif
