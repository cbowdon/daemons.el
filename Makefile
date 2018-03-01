FILES:=daemons.el daemons-systemd.el daemons-sysvinit.el
VERSION:=0.2.0
PACKAGE_NAME:=daemons-$(VERSION)

all: $(PACKAGE_NAME).tar

$(PACKAGE_NAME).tar: clean $(FILES)
	mkdir $(PACKAGE_NAME)
	cp $(FILES) $(PACKAGE_NAME)
	tar -cf $(PACKAGE_NAME).tar $(PACKAGE_NAME)

clean:
	rm -f $(PACKAGE_NAME).tar
	rm -rf $(PACKAGE_NAME)

