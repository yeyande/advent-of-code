BUILD = ./build
BIN = $(BUILD)/Main

all: $(BIN) run

.PHONY: run
run: $(BIN)
	$(BIN)

$(BIN): Main.hs | $(BUILD)
	ghc -dynamic -O2 -outputdir $(BUILD) -o $@ $^
	
$(BUILD):
	mkdir -p $@

.PHONY: clean
clean:
	rm -rf $(BUILD)
