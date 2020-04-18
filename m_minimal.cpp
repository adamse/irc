#include "inspircd.h"
#include "HsFFI.h"
#include "Minimal_stub.h"

typedef struct {
  HsWord64 len;
  HsPtr data;
} String;

#define STR(std) {(std).length(), (HsPtr)(std).data()}
#define PTR(str) (HsPtr)(&(str))

class MinimalModule : public Module {

  HsStablePtr state;

  public:
  MinimalModule() : state(NULL) {
    int argc = 0;
    char* argv[] = {NULL};
    char** argp = argv;
    hs_init(&argc, &argp);
    state = hs_module_init();
  }

  ~MinimalModule() {
    hs_module_cleanup(state);
    hs_exit(); // the GHC rts cannot be reloaded currently, hence this is a bit fishy to do :)
  }

  Version GetVersion() {
    char* version = (char*)hs_module_version();
    return Version(version, VF_VENDOR);
  }

  void OnUserPostMessage(User* user, const MessageTarget& mt, const MessageDetails& details) override {
    String nick = STR(user->nick);
    String target = STR(mt.GetName());
    String message = STR(details.text);

    hs_module_OnUserPostMessage(state, PTR(nick), mt.type, PTR(target), PTR(message));
  }
};

MODULE_INIT(MinimalModule)