%.hs_o: %.hs
	ghc -fforce-recomp -optl-fPIC -fPIC -c $*.hs -osuf hs_o -dynamic

%.o: %.cpp
	ghc -fforce-recomp $(INC) -fPIC -c $*.cpp -dynamic

m_minimal.so: Minimal.hs_o m_minimal.o
	ghc -fforce-recomp -c $^ -shared -dynamic -o $@ -lHSrts_thr_l-ghc$(GHC_VERSION)

all: m_minimal.so

test: m_minimal.so .FORCE
	rm test
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