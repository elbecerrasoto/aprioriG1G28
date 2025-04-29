DATA = data
TARGZ = .data.tar.gz

.PHONY help:
help:
	less Makefile

$(DATA): $(TARGZ)
	@if [ -d "$(DATA)" ]; then echo "$(DATA) already exists, skipping extraction."; else tar -xzvf $< ; fi
	@touch $@ # Update data/ creation, as tar -xz keep the tape date.

.PHONY style:
style:
	Rscript -e 'styler::style_dir(".", recursive = FALSE)'

.PHONY clean:
	rm -rf results data
