FILES:=daemons.el daemons-systemd.el daemons-sysvinit.el daemons-shepherd.el daemons-brew.el
VERSION:=2.1.0
PACKAGE_NAME:=daemons-$(VERSION)

all: $(PACKAGE_NAME).tar

$(PACKAGE_NAME).tar: clean $(FILES)
	mkdir $(PACKAGE_NAME)
	cp $(FILES) $(PACKAGE_NAME)
	tar -cf $(PACKAGE_NAME).tar $(PACKAGE_NAME)

clean:
	rm -f $(PACKAGE_NAME).tar
	rm -rf $(PACKAGE_NAME)
	rm -rf .cask

compile: cask
	! (cask eval "(let ((byte-compile-error-on-warn t)) \
	                 (cask-cli/build))" 2>&1 \
	   | egrep -a "(Warning|Error):") ; \
	  (ret=$$? ; cask clean-elc && exit $$ret)

.PHONY: compile
