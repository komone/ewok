include Makedefs

#Libs to build
LIBS = v8erl
LIB_TARGETS = $(addsuffix .$(LIB_EXT),$(addprefix lib/,$(LIBS)))

#Beams to build
ERL_FILES = $(wildcard $(ERL_DIR)/*.erl)
BEAM_FILES = $(addsuffix .beam,$(notdir $(basename $(ERL_FILES))))
BEAM_TARGETS = $(addprefix $(EBIN_DIR)/,$(BEAM_FILES))

APPS = $(wildcard $(ERL_DIR)/*.app)
APP_TARGETS = $(addprefix $(EBIN_DIR)/,$(notdir $(APPS)))

JS_FILES = $(wildcard js/*.js)
JSC_FILES = $(addsuffix .jsc,$(notdir $(basename $(JS_FILES))))
JSC_TARGETS = $(addprefix js/,$(JSC_FILES))

#Compile flags
CXXFLAGS  = -I./include $(V8_INC) $(ERL_INC) -g -Wall -m32 -I$(EI_INC)

default: all


$(LIB_DIR):
	install -d $(LIB_DIR)

$(EBIN_DIR): 
	install -d $(EBIN_DIR)

localdirs: $(LIB_DIR) $(EBIN_DIR)

all: localdirs $(LIB_TARGETS) $(BEAM_TARGETS) $(APP_TARGETS) $(JSC_TARGETS)

$(SRC_DIR)/%.o: $(SRC_DIR)/%.cc include/js2erl.h
	$(CPP) -c $(CXXFLAGS) -o $@ $<

lib/v8erl.so: CXXFLAGS += -fPIC
lib/v8erl.so: $(SRC_DIR)/driver.o $(SRC_DIR)/js2erl.o
	$(CPP) $(LDFLAGS) $(EI_LIB) $(V8_LIB) -lv8 -lerl_interface -lei $^ -o $@

$(EBIN_DIR)/%.beam: $(ERL_DIR)/%.erl
	erlc -W -o $(EBIN_DIR)/ $<

$(EBIN_DIR)/%.app: $(ERL_DIR)/%.app
	cp $< $@

clean:    
	rm -rf src/*.o js/*.jsc $(LIB_DIR) $(EBIN_DIR)

js/%.jsc: js/%.js
	js -s -C $<
	touch $@

.PHONY: all clean localdirs test

test: all
	erl -pa ebin -noshell -run v8 test -run init stop
