# makefile for the Prog8 compiler

# default if not provided
prg = vtsdemo.prg
bin = vtsbank.bin

# This finds either prog8c (preferred) or a prog8c*.jar in JARS
JARS = ~/Downloads/prog8c*.jar ~/Downloads/prog8*/prog8c*.jar
PROG8JAR = $(shell ls 2>/dev/null -t $(JARS) | head -1)
ifneq ($(shell which prog8c),)
  PCC=prog8c
else
  ifeq ($(findstring jar,$(PROG8JAR)),jar)
    PCC=java -jar $(PROG8JAR)
  else
    PCC=@echo Prog8 compiler missing, please put in ~/Downloads
  endif
endif

PCCARGS = -asmlist -target cx16 

all: $(bin) $(prg)

%.prg: %.p8
	$(PCC) $(PCCARGS) -emu "$<"

%.bin: %.p8
	$(PCC) $(PCCARGS) "$<"

clean:
	rm -f $(prg)
	rm -f $(bin)