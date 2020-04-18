all: m_minimal.so

GHC_OPTS=-fforce-recomp -fPIC -dynamic $(INC)
CXX_OPTS=-optc-fno-rtti
HS_OPTS=-osuf hs_o
LINK_OPTS=-package text -package async -shared -lHSrts_thr_l-ghc$(GHC_VERSION)

%.hs_o: %.hs
	ghc $(GHC_OPTS) $(HS_OPTS) -c $*.hs

%.o: %.cpp
	ghc $(GHC_OPTS) $(CXX_OPTS) -c $*.cpp

m_minimal.so: Minimal.hs_o m_minimal.o
	ghc $(GHC_OPTS) $(LINK_OPTS) -c $^ -o $@

test: m_minimal.so .FORCE
	$(RM) -f test
	gcc $(INC) -ldl test.cpp -o test

install: m_minimal.so
	cp m_minimal.so $(INSPIRCD_RUN_PATH)/modules/

clean:
	$(RM) *.hs_o
	$(RM) *.hi
	$(RM) *.o
	$(RM) *.so
	$(RM) *_stub.h
	$(RM) test

.FORCE: