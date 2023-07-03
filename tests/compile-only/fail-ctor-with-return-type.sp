using __intrinsics__.Handle;

// If you rename JsonObject to JsonObject2 - you won't get the error
methodmap JsonObject < Handle {
  public native void JsonObject();
};

public Plugin myinfo = {
  name = "methodmap bug",
  author = "",
  description = "",
  version = "",
  url = ""
};
