
#ifndef GENERAL_H
#define GENERAL_H


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

struct String {
    char *data = nullptr;
    s64 length = 0;

    String() { }

    // String(char *str) {
    //     this->data = str;
    //     this->length = strlen(str)
    // }

    char operator[](s64 index) const {
        assert(index >= 0 && index < length);

        return data[index];
    }

    String substring(s64 start, s64 slen) {
        assert(start < length && start+slen <= length);

        String s;
        s.data += start;
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
    char *mem = (char *)malloc(s.length + 1);
    memcpy(mem, s.data, s.length);
    mem[s.length] = 0;
    return mem;
}

inline bool operator==(const String &s, const String &t) {
    if (s.length != t.length) return false;
    if (s.data == nullptr && t.data != nullptr) return false;
    if (t.data == nullptr && s.data != nullptr) return false;
    if (s.data == nullptr && t.data == nullptr) return true;

    for (s64 i = 0; i < s.length; ++i) {
        if (s[i] != t[i]) return false;
    }

    return true;
}

inline String copy_string(String s) {
    String out;
    out.length = s.length;
    if (s.data && s.length) {
        out.data = (char *)malloc(s.length);
        memcpy(out.data, s.data, out.length);
    }
    return out;
}

struct Span {
    s64 start;
    s64 length;

    Span(s64 start = 0, s64 length = 0) {
        assert(start >= 0);
        assert(length >= 0);

        this->start = start;
        this->length = length;
    }

    bool fits_in_string(String text) {
        return (!(text.length < start || text.length < start + length));
    }

    void map_to_text_coordinates(String text, s64 *line_start, s64 *char_start, s64 *line_end, s64 *char_end) {
        assert(fits_in_string(text));

        s64 line_count = 0;
        s64 char_count = 0;
        for (s64 i = 0; i < text.length; ++i) {
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

template<typename T>
struct Array {
    T *data = nullptr;
    s64 count = 0;
    s64 allocated = 0;

    const int NEW_MEM_CHUNK_ELEMENT_COUNT =  16;

    Array(s64 reserve_amount = 0) {
        reserve(reserve_amount);
    }

    void reserve(s64 amount) {
        if (amount <= 0) amount = NEW_MEM_CHUNK_ELEMENT_COUNT;
        if (amount <= allocated) return;

        T *new_mem = (T *)malloc(amount * sizeof(T));
        
        if (data) {
            memcpy(new_mem, data, count * sizeof(T));
            free(data);
        }

        data = new_mem;
    }

    void resize(s64 amount) {
        reserve(amount);
        count = amount;
        // @TODO maybe default initalized all elements
        // that we grew by?
    }

    void add(T element) {
        if (count+1 > allocated) reserve(allocated + NEW_MEM_CHUNK_ELEMENT_COUNT);

        data[count] = element;
        count += 1;
    }

    T pop() {
        assert(count > 0);
        T result = data[count-1];
        count -= 1;
        return result;
    }

    T &operator[] (s64 index) {
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


// @Incomplete
struct Pool {
    struct Chunk {
        void *data = nullptr;
        s64 used = 0;
        s64 allocated = 0;
    };
    Array<>
};

#endif
