CC ?= cc
AR ?= ar
CFLAGS ?= -O2

# Paths
OPAM_SWITCH_PREFIX ?= $(shell opam var prefix)
CN_RUNTIME := $(OPAM_SWITCH_PREFIX)/lib/cn/runtime

LUA_SRC_DIR := ../src

INCLUDES := \
	-I$(LUA_SRC_DIR) \
	-I$(CN_RUNTIME)/include

OBJS := lua_wrappers.o
LIB  := lua_wrappers.a

all: $(LIB)

$(LIB): $(OBJS)
	$(AR) rcs $@ $^

lua_wrappers.o: lua_wrappers.c
	$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@

clean:
	rm -f $(OBJS) $(LIB)
