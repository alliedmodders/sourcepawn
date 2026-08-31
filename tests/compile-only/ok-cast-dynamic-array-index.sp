native int strcmp(const String:str1[], const String:str2[], bool:caseSensitive=true);

public int CF_ArrayIndex(const String:ary[][], String:value[], max) {
  for (new i = 0; i < max; ++i) {
    if (strcmp(String:ary[i], value) == 0) {
      return i;
    }
  }
  return -1;
}

public void main() {}
