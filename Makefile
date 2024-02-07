ERL_FILES := $(wildcard *.erl lib/*.erl)

all: $(ERL_FILES)
    make -C lib
    erl -compile $(ERL_FILES)

clean:
    rm -f *.beam *.dump lib/*.beam client/*.beam

run_tests: all
    erl -noshell -eval "eunit:test(test_client), halt()"