// Below structs cribbed from tools/perf/util/jitdump.h in the Linux source tree.

#define JITHEADER_MAGIC 0x4A695444
#define JITHEADER_VERSION 1

enum jitdump_flags_bits {
  JITDUMP_FLAGS_ARCH_TIMESTAMP_BIT,
  JITDUMP_FLAGS_MAX_BIT,
};

#define JITDUMP_FLAGS_ARCH_TIMESTAMP (1ULL << JITDUMP_FLAGS_ARCH_TIMESTAMP_BIT)

struct jitheader {
  uint32_t magic;      /* characters "jItD" */
  uint32_t version;    /* header version */
  uint32_t total_size; /* total size of header */
  uint32_t elf_mach;   /* elf mach target */
  uint32_t pad1;       /* reserved */
  uint32_t pid;        /* JIT process id */
  uint64_t timestamp;  /* timestamp */
  uint64_t flags;      /* flags */
};

enum jit_record_type {
  JIT_CODE_LOAD       = 0,
  JIT_CODE_MOVE       = 1,
  JIT_CODE_DEBUG_INFO = 2,
  JIT_CODE_CLOSE      = 3,
  JIT_CODE_MAX,
};

/* record prefix (mandatory in each record) */
struct jr_prefix {
  uint32_t id;
  uint32_t total_size;
  uint64_t timestamp;
};

struct jr_code_load {
  struct jr_prefix p;

  uint32_t pid;
  uint32_t tid;
  uint64_t vma;
  uint64_t code_addr;
  uint64_t code_size;
  uint64_t code_index;
  const char name[0];
  unsigned char bytes[0];
};

struct debug_entry {
  uint64_t addr;
  int lineno;
  int discrim;
  const char name[0];
};

struct jr_code_debug_info {
  struct jr_prefix p;

  uint64_t code_addr;
  uint64_t nr_entry;
  struct debug_entry entries[0];
};

