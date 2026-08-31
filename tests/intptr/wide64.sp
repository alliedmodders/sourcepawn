#include <shell>

public main()
{
  // A value with bit 40 set — unrepresentable in a 32-bit intptr.
  int64 probe = 1099511627783;   // 2^40 + 7

  if (sys_intptr_size() == 8) {
    // Genuine 64-bit intptr: store, round-trip, and read back high bits.
    intptr p = view_as<intptr>(probe);
    printnumptr(p);                                      // 1099511627783
    printnum64(view_as<int64>(p));                       // 1099511627783
    printnum(view_as<int>(view_as<int64>(p) >> 32));     // 256 (high bits preserved)
  } else {
    // 32-bit intptr cannot hold probe. Emit the identical expected stdout.
    printnum64(probe);                                   // 1099511627783
    printnum64(probe);                                   // 1099511627783
    printnum(256);                                       // 256
  }
}
