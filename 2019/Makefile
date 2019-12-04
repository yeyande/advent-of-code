BUILD = ./build
BIN = $(BUILD)/Main

all: $(BIN)

$(BIN): Main.hs | $(BUILD)
	ghc -outputdir $(BUILD) -o $@ $^
	
$(BUILD):
	mkdir -p $@

.PHONY: clean
clean:
	rm -rf $(BUILD)
