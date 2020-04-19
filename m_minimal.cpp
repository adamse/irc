#include "inspircd.h"
#include "HsFFI.h"
#include "Minimal_stub.h"

typedef struct {
  HsWord64 len;
  HsPtr data;
} String;

#define HSSTR(std) {(std).length(), (HsPtr)(std).data()}
#define PTR(str) (HsPtr)(&(str))
#define CSTR(hs) std::string((const char*) (hs)->data, (size_t)(hs)->len)

void HSLog(LogLevel lvl, String* hs) {
  ServerInstance->Logs->Log("m_minimal", lvl, CSTR(hs));
}

class MinimalModule : public Module {

  HsStablePtr state;

  public:
  MinimalModule() : state(NULL) {
    int argc = 0;
    char* argv[] = {NULL};
    char** argp = argv;
    hs_init(&argc, &argp);
    state = hs_module_init((HsFunPtr)&HSLog);
  }

  ~MinimalModule() {
    hs_module_cleanup(state);
    hs_exit();
  }

  Version GetVersion() {
    char* version = (char*)hs_module_version();
    return Version(version, VF_VENDOR);
  }

  void OnUserPostMessage(User* user, const MessageTarget& mt, const MessageDetails& details) override {
    String nick = HSSTR(user->nick);
    String target = HSSTR(mt.GetName());
    String message = HSSTR(details.text);

    hs_module_OnUserPostMessage(state, PTR(nick), mt.type, PTR(target), PTR(message));
  }
};

MODULE_INIT(MinimalModule)