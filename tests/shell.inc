// vim: set ts=2 sw=2 tw=99 et:
#include <float>
#include <handles>

enum struct TestStruct {
    int x;
    int y;
}

native void printnum(int n);
native void writenum(int n);
native void printfloat(float n);
native void writefloat(float n);
native void printnums(any:...);
native void print(const char[] str);
native void dump_stack_trace();
native void report_error();
native void unbound_native();
native int donothing();

// dumb version of printf(), only supports %f, %d, %s.
native void printf(const char[] fmt, any:...);

// Return arg, but through a dynamically generated native.
native int dynamic_native(int arg);

// Read array[x][y] and store in |out|. If compiled with < 1.12, return false.
native bool access_2d_array(int[][] array, int x, int y, int& out);

typedef InvokeCallback = function void ();
// Invoke |fn| up to |count| times, returning false immediately on failure.
native bool invoke(InvokeCallback fn, int count);
// Invoke |fn|, |count| times, returning the number of successful invocations.
native int execute(InvokeCallback fn, int count);

#define SM_PARAM_COPYBACK (1 << 0)
#define SM_PARAM_STRING_UTF8 (1 << 0)
#define SM_PARAM_STRING_COPY (1 << 1)
#define SM_PARAM_STRING_BINARY (1 << 2)

// Invoke |fn| with the given buffer and sz/cp flags.
typedef StringCallback = function void (char[] buffer, int length);
native int call_with_string(StringCallback fn, char[] buffer, int length, int sz_flags,
                            int cp_flags);

typedef Array2dCallback = function void (const int[][] array, int length, int stride);

// Create a 2D array from the given "flat" array, length and stride.
native void copy_2d_array_to_callback(const int[] flat_array, int length, int stride,
                                      Array2dCallback callback);

native void assert_eq(any a1, any a2);

native void print_test_struct(TestStruct x);
native TestStruct add_test_structs(TestStruct a, TestStruct b);
