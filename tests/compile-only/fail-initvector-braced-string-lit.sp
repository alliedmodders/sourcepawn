enum struct X {
  char msg[32];
}

X gWhatever = {{"asdf"}};

public main()
{
  return sizeof(gWhatever);
}
