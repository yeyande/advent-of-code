BUILD = ./build
BIN = $(BUILD)/Main

all: $(BIN) run

.PHONY: run
run: $(BIN)
	$(BIN)

$(BIN): Main.hs | $(BUILD)
	ghc -outputdir $(BUILD) -o $@ $^
	
$(BUILD):
	mkdir -p $@

.PHONY: clean
clean:
	rm -rf $(BUILD)
